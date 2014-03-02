-- | This is the core module of TeXPrep.
-- It parses LaTeX and converts it to fodt, but conversion of specific commands is mostly outsourced to the Cmd module. Here, only those are processed that require access to the .aux file.
-- The module exports all the datatypes that are necessary to write a config file, and also some helper functions to make configuration easier. See “texprep.conf.hs” for details.
module TeXPrep.Core
	(Config (..), TexElem (..), Procsr, Writer
	,readTex
	,procTex
	,writeFodt, writeOpts, writeParBeg, writeParEnd
	,detex, isArg, isCmd, isCmnt, isOpt, isTxt
	) where


--																					{{{
import Control.Monad (mplus)
import Data.List (intercalate)
import Debug.Trace (trace)
import Text.ParserCombinators.Parsec
--																					}}}


-- = data =========================================================================={{{==


-- | The type of user-defined postprocessors.
type Procsr =	[TexElem]				-- ^ The parsed TeX file.
				-> [TexElem]			-- ^ The postprocessed file.


-- | The type of user-defined writers.
type Writer =	TexElem					-- ^ The TexElem to be written.
				-> Maybe (String, [TexElem], String)
										-- ^ What goes before the args, the args to be further processed, and what goes after them.


-- | This is to enable user-specified commands.
data Config = Config
	{errMsg		:: Maybe String			-- ^ Required by Dyre.
	,aliases	:: [(String,String)]	-- ^ User-specified command aliases.
	,styles		:: [String]				-- ^ User-specified fodt styles.
	,procsrs	:: [Procsr]				-- ^ User-specified postprocessors.
	,writers	:: [Writer]}			-- ^ User-specified writers.


-- | This one is meant to be internal to the reader.
-- You never know how many {}'and []'there will be, and in what order.
data TexArgOpt =
		TexArg	[TexElem]				-- ^ Anything inside {}.
	|	TexOpt	String					-- ^ Anything inside [].
	deriving Show


-- | The main data type for storing parsed LaTeX.
data TexElem =
		TexCmd	{name	:: String		-- ^ Commands, switches and environments.
				,opts	:: [String]
				,args	:: [TexElem]
				,cmnt	:: [String]}	-- ^ For internal use: footnote numbers &c.
	|	TexCmnt	{text	:: String}		-- ^ Comments.
	|	TexTxt	{text	:: String}		-- ^ Pure text outside of all commands.
	deriving (Eq,Show)


--																					}}}
-- = reading ======================================================================={{{==


-- | Read TexElems inside {}.
readArg :: Parser TexArgOpt
readArg = do
	args <- between (char '{') (char '}') (many readElem)
	return $ TexArg (concat args)


-- | Read args and opts.
readArgOpts :: Parser ([TexElem],[String])
readArgOpts = do
	aos <- many $ readArg <|> readOpt
	return (unpackArgs aos, unpackOpts aos)


-- | Read anything that begins with '\', including special chars, hyphens, linebreaks, and environments, but not switches.
readCmd :: Parser [TexElem]
readCmd = do
	char '\\'
	choice [readCmdEnv, readCmdHyph, readCmdPar, readCmdSpec, readCmdAny]

-- | Fallback to read undefined commands.
readCmdAny :: Parser [TexElem]
readCmdAny = do
	name <- many1 $ letter <|> oneOf "@*"
	optional $ many (string " ")
	(args, opts) <- readArgOpts
	return [TexCmd name opts args []]

-- | Read environment into a single command.
readCmdEnv :: Parser [TexElem]
readCmdEnv = do
	try $ string "begin{"
	name <- manyTill (letter <|> oneOf "@*") (char '}')
	aos <- many $ readArg <|> readOpt
	let args = unpackArgs aos
	let opts = unpackOpts aos
	env <- manyTill readElem (try $ string ("\\end{" ++ name ++ "}"))
	return [TexCmd name opts (args ++ concat env) []]

-- | Read soft hyphens.
readCmdHyph :: Parser [TexElem]
readCmdHyph = do
	char '-'
	hyph <- option ' ' (char '/')
	return (if hyph=='/'
		then [TexTxt "-"]
		else [TexTxt [toEnum 173::Char]])

-- | Read forced line breaks.
readCmdPar :: Parser [TexElem]
readCmdPar = do
	char '\\'
	spaces
	return [TexCmd "#linebreak" [] [] ["indent"]]

-- | Read special characters (#, $, %, &c.)
readCmdSpec :: Parser [TexElem]
readCmdSpec = do
	cmd <- count 1 (oneOf "#$%^&_{} ")
	return [TexTxt cmd]


-- | Read comments.
readCmnt :: Parser [TexElem]
readCmnt = do
	char '%'
	cmnt <- manyTill (noneOf "\n") newline
	return [TexCmnt cmnt, TexCmd "#linebreak" [] [] ["indent"]]


-- | Read any TexElem.
readElem :: Parser [TexElem]
readElem = choice [readCmd, readCmnt, readPar, readSwitch, readTxt]


-- | Read Strings inside [].
readOpt :: Parser TexArgOpt
readOpt = do
	opt <- between (char '[') (char ']') (many $ noneOf "]")
	return $ TexOpt opt


-- | Read linebreak.
readPar :: Parser [TexElem]
readPar = do
	char '\n'
	spaces
	return [TexCmd "#linebreak" [] [] ["indent"]]


-- | Read switch into a single command.
readSwitch :: Parser [TexElem]
readSwitch = do
	cont <- fmap (unpackArgs . (:[])) readArg
	if null cont
		then return [TexTxt "<>"]
		else do
			let fstCmd = head cont
			return (if isCmd fstCmd
				then [fstCmd {args = args fstCmd ++ tail cont}]
				else [TexCmd "{" [] cont []])


-- | The main parser.
readTex :: String -> Either ParseError [TexElem]
readTex = parse (fmap concat $ many readElem) ""


-- | Read text outside of commands.
readTxt :: Parser [TexElem]
readTxt = do
	text <- many1 $ noneOf "%\\{}\n"
	return [TexTxt text]


--																					}}}
-- = postprocessing ================================================================{{{==


-- | Replace user-specified aliases.
procAliases :: [(String,String)] -> Procsr
procAliases _ [] = []
procAliases as (t:ts)
	| isCmd t	= repl t {args = procAliases as (args t)} : procAliases as ts
	| otherwise	= t : procAliases as ts
	where
	repl r = case lookup (name r) as of
		Just n	-> r {name = n}
		Nothing	-> r


-- | Resolve references.
procRefs :: [TexElem] -> Procsr
procRefs aux = procRefsRepl (procRefsCollect aux)

-- | Helper for procRefs. Collects the references from the aux file.
procRefsCollect :: [TexElem] -> [(String,String)]
procRefsCollect aux = [(rName x, rTrgt x) | x <- aux, isCmd x, name x == "newlabel"]
	where
	rName = text . head . args
	rTrgt = text . head . args . (!!1) . args

-- | Helper for procRefs. Replaces the collected references in the tex file.
procRefsRepl :: [(String,String)] -> [TexElem] -> [TexElem]
procRefsRepl _ [] = []
procRefsRepl aux (t:ts)
	| isCmd t	= if name t == "ref"
					then repl t : procRefsRepl aux ts
					else t {args = procRefsRepl aux (args t)} : procRefsRepl aux ts
	| otherwise	= t : procRefsRepl aux ts
	where
	repl r = case lookup (text . head . args $ r) aux of
		Just t	-> TexTxt t
		Nothing	-> (trace $ "WARNING: resolution not found for the reference to “" ++ (text . head . args $ r) ++ "”.")
					r


-- | Resolve section numbers.
procSecNrs :: [TexElem] -> Procsr
procSecNrs aux = procSecNrsRepl (procSecNrsCollect aux)

-- | Helper for procSecNrs. Collects the section numbers from the aux file.
procSecNrsCollect :: [TexElem] -> [(String,String,String)]
procSecNrsCollect auxs = do
	aux <- filter isSecNr auxs
	let nmb = text . head . args . (!!1) . args . (!!1) . args $ aux
	let typ = text . head . args . (!!1) . args $ aux
	let tmp = drop 2 . args . (!!1) . args $ aux
	let nam = if (text . last) tmp == typ ++ "." ++ nmb
				then detex . dropEnd 2 $ tmp
				else detex . dropEnd 1 $ tmp
	return (nam, typ, nmb)
	where
	isSecNr e = isCmd e
				&& name e == "@writefile"
				&& (name . (!!1) . args) e == "contentsline"
				&& (isCmd . (!!1) . args . (!!1) . args) e


-- | Helper for procSecNrs. Replaces the collected section numbers in the tex file.
procSecNrsRepl :: [(String,String,String)] -> [TexElem] -> [TexElem]
procSecNrsRepl _ [] = []
procSecNrsRepl aux (t:ts)
	| isCmd t	= if name t `elem` ["part","chapter","section","subsection","subsubsection"]
					then repl t : procSecNrsRepl aux ts
					else t {args = procSecNrsRepl aux (args t)} : procSecNrsRepl aux ts
	| otherwise	= t : procSecNrsRepl aux ts
	where
	repl r = case lookup3 (detex (args r), name r) aux of
		Just n	-> r {args = TexTxt (n++" ") : args r}
		Nothing	-> (trace $ "WARNING: number not found for " ++ name r ++ " “" ++ detex (args r) ++ "”.")
					r


-- | Process special characters.
-- Fodt will not accept unescaped "\&", "\<", "\>".
procSpec :: Procsr
procSpec [] = []
procSpec (t:ts)
	| isCmnt t || isTxt t
				= t {text = replace' '>' "&gt;" . replace' '<' "&lt;" . replace' '&' "&amp;"
							$ text t} : procSpec ts
	| otherwise	= t {args = procSpec (args t)} : procSpec ts


-- | Postprocess after imperfect parsing.
-- Fix special characters, and whatever defined in the config.
procTex ::	Config -> ([TexElem],[TexElem]) -> [TexElem]
procTex cfg (aux,tex) = foldr1 (.) procsrs' tex
	where
	procsrs' = [procSpec . procRefs aux . procSecNrs aux]
				++ procsrs cfg
				++ [procAliases (aliases cfg)]


--																					}}}
-- = writing ======================================================================={{{==


-- | The ending of a fodt document.
writeColoph :: String
writeColoph = "\n\n\t\t</office:text>\n\t</office:body>\n</office:document>"

-- | Write a TexElem.
-- Use writers defined in the config and fallback to writeElemAny.
writeElem :: Config -> TexElem -> String
writeElem cfg elem = let writers' = writers cfg ++ [writeElemLinebreak, writeElemAny]
	in case choose writers' elem of
		Just (b,i,a)	-> b ++ concatMap (writeElem cfg) i ++ a
		Nothing			-> error "ERROR: Weird, this shouldn’t have happened. Please let kamil.stachowski [at] gmail.com know."

-- | Write any TexElem.
-- Used as fallback when command not defined in the config.
writeElemAny :: Writer
writeElemAny (TexCmd name opts args _) = Just
								("&lt;" ++ name ++ writeOpts opts ++ "&gt;"
								,args
								,"&lt;/" ++ name ++ "&gt;")
writeElemAny (TexCmnt text)	= Just
								("&lt;!--" ++ text ++ "--&gt;"
								,[]
								,"")
writeElemAny (TexTxt text)	= Just
								(text
								, []
								, "")

-- | Write a line break.
-- Taking care of whether the next paragraph should be indented.
writeElemLinebreak :: Writer
writeElemLinebreak (TexCmd "#linebreak" _ _ cmnt) = Just
								(writeParEnd
								,[]
								,writeParBeg (head cmnt == "indent"))
writeElemLinebreak _ = Nothing


-- | Write opts nicely, XML-style.
writeOpts :: [String] -> String
writeOpts [] = ""
writeOpts opts = " opts=\"" ++ intercalate "," opts ++ "\""


-- | Start new paragraph.
-- The Bool is whether the paragraph should be indented.
writeParBeg :: Bool -> String
writeParBeg indent = "\n<text:p text:style-name=\"Text_20_body"
						++ (if indent then "_20_indent" else "")
						++ "\">\n"


-- | End paragraph.
writeParEnd :: String
writeParEnd = "\n</text:p>\n"
-- writeParEnd = "</text:p>"


-- | The beginning of a fodt document.
writePream :: Config -> String
writePream cfg = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n<office:document xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\" xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\" xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\" xmlns:draw=\"urn:oasis:names:tc:opendocument:xmlns:drawing:1.0\" xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\" xmlns:number=\"urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0\" xmlns:svg=\"urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0\" xmlns:chart=\"urn:oasis:names:tc:opendocument:xmlns:chart:1.0\" xmlns:dr3d=\"urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0\" xmlns:math=\"http://www.w3.org/1998/Math/MathML\" xmlns:form=\"urn:oasis:names:tc:opendocument:xmlns:form:1.0\" xmlns:script=\"urn:oasis:names:tc:opendocument:xmlns:script:1.0\" xmlns:config=\"urn:oasis:names:tc:opendocument:xmlns:config:1.0\" xmlns:ooo=\"http://openoffice.org/2004/office\" xmlns:ooow=\"http://openoffice.org/2004/writer\" xmlns:oooc=\"http://openoffice.org/2004/calc\" xmlns:dom=\"http://www.w3.org/2001/xml-events\" xmlns:xforms=\"http://www.w3.org/2002/xforms\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:rpt=\"http://openoffice.org/2005/report\" xmlns:of=\"urn:oasis:names:tc:opendocument:xmlns:of:1.2\" xmlns:xhtml=\"http://www.w3.org/1999/xhtml\" xmlns:grddl=\"http://www.w3.org/2003/g/data-view#\" xmlns:officeooo=\"http://openoffice.org/2009/office\" xmlns:tableooo=\"http://openoffice.org/2009/table\" xmlns:drawooo=\"http://openoffice.org/2010/draw\" xmlns:calcext=\"urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0\" xmlns:field=\"urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0\" xmlns:formx=\"urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0\" xmlns:css3t=\"http://www.w3.org/TR/css3-text/\" office:version=\"1.2\" office:mimetype=\"application/vnd.oasis.opendocument.text\">\n"
			++ "\t<office:styles>\n"
			++ "\t\t<style:style style:name=\"Standard\" style:family=\"paragraph\" style:class=\"text\"/>\n"
			++ "\t\t<style:style style:name=\"Text_20_body\" style:display-name=\"Text body\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"text\">\n"
			++ "\t\t\t<style:paragraph-properties fo:margin-top=\"0mm\" fo:margin-bottom=\"0mm\" style:contextual-spacing=\"false\" fo:text-align=\"justify\" style:justify-single-word=\"false\" style:auto-text-indent=\"false\"/>\n"
			++ "\t\t\t<style:text-properties fo:font-size=\"10pt\" style:font-size-asian=\"10pt\" style:font-weight-complex=\"normal\"/>\n"
			++ "\t\t</style:style>\n"
			++ "\t\t<style:style style:name=\"Text_20_body_20_indent\" style:display-name=\"Text body indent\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\" style:class=\"text\">\n"
			++ "\t\t\t<style:paragraph-properties fo:margin-top=\"0mm\" fo:margin-bottom=\"0mm\" fo:margin-left=\"0mm\" fo:margin-right=\"0mm\" fo:text-indent=\"5.27mm\" style:contextual-spacing=\"false\" fo:text-align=\"justify\" style:justify-single-word=\"false\" style:auto-text-indent=\"false\"/>\n"
			++ "\t\t\t<style:text-properties fo:font-size=\"10pt\" style:font-size-asian=\"10pt\" style:font-weight-complex=\"normal\"/>\n"
			++ "\t\t</style:style>\n"
			++ concat (styles cfg)
			++ "\t</office:styles>\n"
			++ "\t<office:body>\n"
			++ "\t\t<office:text>\n"
			++ "\n"


-- | The main writer.
writeFodt :: Config -> [TexElem] -> String
writeFodt cfg elems = writePream cfg
					++ writeParBeg False
					++ concatMap (writeElem cfg) elems
					++ writeParEnd
					++ writeColoph
-- writeFodt _ t = show t


--																					}}}
-- = helpers ======================================================================={{{==


-- | Return the result of the first function that doesn’t fail.
choose :: [a -> Maybe b] -> a -> Maybe b
choose fs e = foldr1 mplus (map ($ e) fs)


-- | Throw away commands and join the text.
detex :: [TexElem] -> String
detex = concatMap getText
	where
	getText :: TexElem -> String
	getText (TexCmd _ _ a _) = detex a
	getText (TexCmnt t) = t
	getText (TexTxt t) = t


-- | Drop from the end of a list.
dropEnd :: Int -> [a] -> [a]
dropEnd n l = take (-n + length l) l


-- | Check if a TexArg.
isArg :: TexArgOpt -> Bool
isArg (TexArg {}) = True
isArg _ = False


-- | Check if a TexCmd.
isCmd :: TexElem -> Bool
isCmd (TexCmd {}) = True
isCmd _ = False


-- | Check if a TexCmnt.
isCmnt :: TexElem -> Bool
isCmnt (TexCmnt {}) = True
isCmnt _ = False


-- | Check if a TexOpt.
isOpt :: TexArgOpt -> Bool
isOpt (TexOpt {}) = True
isOpt _ = False


-- | Check if a TexTxt.
isTxt :: TexElem -> Bool
isTxt (TexTxt {}) = True
isTxt _ = False


-- | A version of lookup for a triples.
lookup3 :: (Eq a, Eq b) => (a,b) -> [(a,b,c)] -> Maybe c
lookup3 _ [] = Nothing
lookup3 n ((x,y,z):xs)
	| n == (x,y)	= Just z
	| otherwise		= lookup3 n xs


-- | Replace e.g. Chars in a String with Strings.
replace' :: (Eq a) => a -> [a] -> [a] -> [a]
replace' _ _ [] = []
replace' x y (z:zs) = (if z==x then y else [z]) ++ replace' x y zs


-- | Extract args from [TexArgOpt]
unpackArgs :: [TexArgOpt] -> [TexElem]
unpackArgs aos = concatMap unpack (filter isArg aos)
	where
	unpack (TexArg a) = a


-- | Extract opts from [TexArgOpt]
unpackOpts :: [TexArgOpt] -> [String]
unpackOpts aos = map unpack (filter isOpt aos)
	where
	unpack (TexOpt o) = o


--																					}}}
