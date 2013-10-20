module TeXPrep.Core
	(Config (..), TexElem (..), Procsr, Writer
	,readTex
	,procTex
	,writeFodt, writeOpts, writeParBeg, writeParEnd
	,isArg, isCmd, isCmnt, isOpt, isTxt, unpackArgs, unpackOpts
	) where


--																					{{{
import Control.Monad (mplus)
import Data.List (intercalate)
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
	,styles		:: [String]				-- ^ User-specified Fodt styles.
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
	return $ (unpackArgs aos, unpackOpts aos)


-- | Read anything that begins with '\', including special chars, hyphens, linebreaks, and environments, but not switches.
readCmd :: Parser [TexElem]
readCmd = do
	char '\\'
	cmd <- choice [readCmdEnv, readCmdHyph, readCmdPar, readCmdSpec, readCmdAny]
	return $ cmd

-- | Fallback to read undefined commands.
readCmdAny :: Parser [TexElem]
readCmdAny = do
	name <- many1 $ letter <|> oneOf "@*"
	optional $ many (string " ")
	(args, opts) <- readArgOpts
	return $ [TexCmd name opts args []]

-- | Read environment into a single command.
readCmdEnv :: Parser [TexElem]
readCmdEnv = do
	try $ string "begin{"
	name <- manyTill (letter <|> oneOf "@*") (char '}')
	aos <- many $ readArg <|> readOpt
	let args = unpackArgs aos
	let opts = unpackOpts aos
	env <- manyTill readElem (try $ string ("\\end{" ++ name ++ "}"))
	return $ [TexCmd name opts (args ++ concat env) []]

-- | Ignore soft hyphens.
readCmdHyph :: Parser [TexElem]
readCmdHyph = do
	char '-'
	hyph <- option ' ' (char '/')
	if (hyph == '/')
		then return $ [TexTxt "-"]
		else return $ []

-- | Read forced line breaks.
readCmdPar :: Parser [TexElem]
readCmdPar = do
	char '\\'
	spaces
	return $ [TexTxt "\n"]

-- | Read special characters (#, $, %, &c.)
readCmdSpec :: Parser [TexElem]
readCmdSpec = do
	cmd <- count 1 (oneOf "#$%^&_{} ")
	return $ [TexTxt cmd]


-- | Read comments.
readCmnt :: Parser [TexElem]
readCmnt = do
	char '%'
	cmnt <- manyTill (noneOf "\n") newline
	return $ [TexCmnt cmnt, TexTxt "\n"]


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
	return $ [TexTxt "\n"]


-- | Read switch into a single command.
readSwitch :: Parser [TexElem]
readSwitch = do
	cont <- fmap (unpackArgs . (:[])) readArg
	if (null cont)
		then return $ [TexTxt "<>"]
		else do
			let fstCmd = head cont
			if (isCmd fstCmd)
				then return $ [fstCmd {args = args fstCmd ++ tail cont}]
				else return $ [TexCmd "{" [] cont []]


-- | The main parser.
readTex :: String -> Either ParseError [TexElem]
readTex = parse (fmap concat $ many readElem) ""


-- | Read text outside of commands.
readTxt :: Parser [TexElem]
readTxt = do
	text <- many1 $ noneOf "%\\{}[]\n"
	return $ [TexTxt text]


--																					}}}
-- = postprocessing ================================================================{{{==


-- | Resolve references.
procRefs :: [TexElem] -> Procsr
procRefs aux tex = procRefsRepl (procRefsCollect aux) tex

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
	| isCmd t	= if (name t == "ref")
					then repl t : procRefsRepl aux ts
					else t {args = procRefsRepl aux (args t)} : procRefsRepl aux ts
	| otherwise	= t : procRefsRepl aux ts
	where
	repl r = case (lookup (text . head . args $ r) aux) of
		Just t	-> TexTxt t
		Nothing	-> r


-- | Process special characters.
-- Fodt will not accept unescaped "&", "<", ">".
procSpec :: Procsr
procSpec [] = []
procSpec (t:ts)
	| isCmnt t || isTxt t
				= t {text = replace' '>' "&gt;" . replace' '<' "&lt;" . replace' '&' "&amp;"
							$ (text t)} : procSpec ts
	| otherwise	= t {args = procSpec (args t)} : procSpec ts


-- | Postprocess after imperfect parsing.
-- Fix special characters, and whatever defined in the config.
procTex ::	Config -> ([TexElem],[TexElem]) -> [TexElem]
procTex cfg (aux,tex) = foldr1 (.) procsrs' $ tex
	where
	procsrs' = [procSpec . procRefs aux] ++ procsrs cfg


--																					}}}
-- = writing ======================================================================={{{==


-- | The ending of a fodt document.
writeColoph :: String
writeColoph = "\n\n\t\t</office:text>\n\t</office:body>\n</office:document>"


-- | Write any TexElem.
-- Used as fallback when command not defined in the config.
writeAnyElem :: Writer
writeAnyElem (TexCmd name opts args cmnt) = Just $
								("&lt;" ++ name ++ (writeOpts opts) ++ "&gt;"
								,args
								,"&lt;/" ++ name ++ "&gt;")
writeAnyElem (TexCmnt text)	= Just $
								("&lt;!--" ++ text ++ "--&gt;"
								,[]
								,"")
writeAnyElem (TexTxt text)	= Just $
								if (text == "\n")
									then (writeParEnd, [], writeParBeg)
									else (text, [], "")


-- | Write a TexElem.
-- Use writers defined in the config and fallback to writeAnyElem.
writeElem :: Config -> TexElem -> String
writeElem cfg elem = let writers' = writers cfg ++ [writeAnyElem]
	in case (choose writers' elem) of
	Just (b,i,a)	-> b ++ (concatMap (writeElem cfg) i) ++ a
	Nothing			-> error "Weird, this shouldn’t have happened. Please let kamil.stachowski [at] gmail.com know."


-- | Write opts nicely, XML-style.
writeOpts :: [String] -> String
writeOpts [] = ""
writeOpts opts = " opts=\"" ++ (intercalate "," opts) ++ "\""


-- | Start new paragraph.
writeParBeg :: String
writeParBeg = "\n<text:p text:style-name=\"Standard\">\n"
-- writeParBeg = "<text:p text:style-name=\"Standard\">"


-- | End paragraph.
writeParEnd :: String
writeParEnd = "\n</text:p>\n"
-- writeParEnd = "</text:p>"


-- | The beginning of a fodt document.
writePream :: Config -> String
writePream cfg = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n<office:document xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\" xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\" xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\" xmlns:draw=\"urn:oasis:names:tc:opendocument:xmlns:drawing:1.0\" xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\" xmlns:number=\"urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0\" xmlns:svg=\"urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0\" xmlns:chart=\"urn:oasis:names:tc:opendocument:xmlns:chart:1.0\" xmlns:dr3d=\"urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0\" xmlns:math=\"http://www.w3.org/1998/Math/MathML\" xmlns:form=\"urn:oasis:names:tc:opendocument:xmlns:form:1.0\" xmlns:script=\"urn:oasis:names:tc:opendocument:xmlns:script:1.0\" xmlns:config=\"urn:oasis:names:tc:opendocument:xmlns:config:1.0\" xmlns:ooo=\"http://openoffice.org/2004/office\" xmlns:ooow=\"http://openoffice.org/2004/writer\" xmlns:oooc=\"http://openoffice.org/2004/calc\" xmlns:dom=\"http://www.w3.org/2001/xml-events\" xmlns:xforms=\"http://www.w3.org/2002/xforms\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:rpt=\"http://openoffice.org/2005/report\" xmlns:of=\"urn:oasis:names:tc:opendocument:xmlns:of:1.2\" xmlns:xhtml=\"http://www.w3.org/1999/xhtml\" xmlns:grddl=\"http://www.w3.org/2003/g/data-view#\" xmlns:officeooo=\"http://openoffice.org/2009/office\" xmlns:tableooo=\"http://openoffice.org/2009/table\" xmlns:drawooo=\"http://openoffice.org/2010/draw\" xmlns:calcext=\"urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0\" xmlns:field=\"urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0\" xmlns:formx=\"urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0\" xmlns:css3t=\"http://www.w3.org/TR/css3-text/\" office:version=\"1.2\" office:mimetype=\"application/vnd.oasis.opendocument.text\">\n"
			++ "\t<office:styles>\n"
			++ "\t\t<style:style style:name=\"Standard\" style:family=\"paragraph\" />\n"
			++ (concat $ styles cfg)
			++ "\t</office:styles>\n"
			++ "\t<office:body>\n"
			++ "\t\t<office:text>\n"
			++ "\n"


-- | The main writer.
writeFodt :: Config -> [TexElem] -> String
writeFodt cfg elems = writePream cfg
					++ writeParBeg
					++ (concatMap (writeElem cfg) elems)
					++ writeParEnd
					++ writeColoph


--																					}}}
-- = helpers ======================================================================={{{==


-- | Return the result of the first function that doesn’t fail.
choose :: [(a -> Maybe b)] -> a -> Maybe b
choose fs e = foldr1 mplus (map ($ e) fs)


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


-- | Replace e.g. Chars in a String with Strings.
replace' :: (Eq a) => a -> [a] -> [a] -> [a]
replace' _ _ [] = []
replace' x y (z:zs) = (if z==x then y else [z]) ++ replace' x y zs


-- | Extract args from [TexArgOpt]
unpackArgs :: [TexArgOpt] -> [TexElem]
unpackArgs aos = concat $ map unpack (filter isArg aos)
	where
	unpack (TexArg a) = a


-- | Extract opts from [TexArgOpt]
unpackOpts :: [TexArgOpt] -> [String]
unpackOpts aos = map unpack (filter isOpt aos)
	where
	unpack (TexOpt o) = o


--																					}}}
