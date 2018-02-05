-- | This module takes care of some the specific LaTeX commands.
-- It is essentially a basic config file enabled by default. It only contains commands that do not require access to the .aux file, as these have to be dealt with in the Core module.
-- It exports all of its procsrs and writers, so that users can cherry pick in their own configuration files.
module TeXPrep.Cmd
	(config
	,biS, biP, biW
	,fnS, fnP, fnW
	,secS, secP, secW
	,tldeP
	) where


--																					{{{
import Data.Maybe (fromMaybe)
import TeXPrep.Core
import Text.ParserCombinators.Parsec
import Control.Monad (unless)
import System.Directory (doesFileExist, getHomeDirectory)
--																					}}}


-- = config ========================================================================{{{==


-- | The configuration that enables everything this module has to offer.
-- Contains only the most common commands; anything more specific is left to the user.
config :: Config
config = Config
	{errMsg		= Nothing
	,aliases	= []
	,styles		= [biS, fnS, secS]
	,procsrs	= [biP, fnP, secP, tldeP]
	,writers	= [biW, fnW, secW]}


--																					}}}

-- = bold italic ==================================================================={{{==


-- | Process bolds inside italics and the other way round.
biP :: Procsr
biP [] = []
biP (tex:texs)
	| isCmd tex	= if name tex `elem` ["textbf","textit"]
					then tex {args = biRepl (args tex)} : biP texs
					else tex {args = biP (args tex)} : biP texs
	| otherwise	= tex : biP texs
	where
	biRepl :: [TexElem] -> [TexElem]
	biRepl [] = []
	biRepl (t:ts)
		| isCmd t	= if name t `elem` ["textbf","textit"]
						then t {name = "#textbfit", args = biRepl (args t)} : biRepl ts
						else t : biRepl ts
		| otherwise	= t : biRepl ts


-- | Fodt styles for bold, italic, and bold italic.
biS :: String
biS = "\t\t<style:style style:name=\"bold\" style:family=\"text\">\n"
			++ "\t\t\t<style:text-properties fo:font-weight=\"bold\" style:font-weight-asian=\"bold\" style:font-weight-complex=\"bold\"/>\n"
			++ "\t\t</style:style>\n"
			++ "\t\t<style:style style:name=\"bolditalic\" style:family=\"text\">\n"
			++ "\t\t\t<style:text-properties fo:font-style=\"italic\" fo:font-weight=\"bold\" style:font-style-asian=\"italic\" style:font-weight-asian=\"bold\" style:font-style-complex=\"italic\" style:font-weight-complex=\"bold\"/>\n"
			++ "\t\t</style:style>\n"
			++ "\t\t<style:style style:name=\"italic\" style:family=\"text\">\n"
			++ "\t\t\t<style:text-properties fo:font-style=\"italic\" style:font-style-asian=\"italic\" style:font-style-complex=\"italic\"/>\n"
			++ "\t\t</style:style>\n"


-- | Write a bold, italic, or bold italic as a fodt.
biW :: Writer
biW (TexCmd "textbf" _ args _) = Just
			("<text:span text:style-name=\"bold\">"
			,args
			,"</text:span>")
biW (TexCmd "#textbfit" _ args _) = Just
			("<text:span text:style-name=\"bolditalic\">"
			,args
			,"</text:span>")
biW (TexCmd "textit" _ args _) = Just
			("<text:span text:style-name=\"italic\">"
			,args
			,"</text:span>")
biW _ = Nothing


--																					}}}
-- = footnotes ====================================================================={{{==


-- | Numbers footnotes.
fnP :: Procsr
fnP = helper 1
	where
	helper :: Int -> Procsr
	helper _ [] = []
	helper n (tex:texs)
		| isCmd tex	= if name tex == "footnote"
						then if null (opts tex)
								then tex {cmnt = [show n]} : helper (n+1) texs
								else tex {cmnt = opts tex} : helper n texs
						else tex {args = helper n (args tex)} : helper n texs
		| otherwise	= tex : helper n texs


-- | Fodt style for footnotes.
fnS :: String
fnS = "\t\t<style:style style:name=\"Footnote\" style:family=\"paragraph\" />\n"


-- | Write footnotes as actual fodt footnotes.
fnW :: Writer
fnW (TexCmd "footnote" _ args cmnt) = let fnNr = read (head cmnt) in Just
			("<text:note text:id=\"ftn" ++ show (fnNr-1) ++ "\" text:note-class=\"footnote\">"
				++ "<text:note-citation text:label=\"" ++ show fnNr ++ "\">" ++ show fnNr ++ "</text:note-citation>"
				++ "<text:note-body>"
				++ writeParBeg False
			,args
			,writeParEnd ++ "</text:note-body></text:note>")
fnW _ = Nothing


--																					}}}
-- = sections ======================================================================{{{==


-- | Remove line breaks from before and after sectioning commands.
-- Sections can’t be inside paragraphs in fodt.
secP :: Procsr
secP (x:y:xs)
	| isSec x && isPar y	= secP (x:xs)
	| isSec y && isPar x	= secP (y:xs)
	| isCmd x				= x {args = secP (args x)} : secP (y:xs)
	| otherwise				= x : secP (y:xs)
secP (x:xs)
	| isCmd x				= x {args = secP (args x)} : secP xs
	| otherwise				= x : secP xs
secP [] = []


-- | Section hierarchy in LaTeX.
secs :: [(String,String)]
secs = [ ("section","1"),("subsection","2"),("subsubsection","3")]


-- | Fodt styles for sections.
secS :: String
secS = "\t\t<style:style style:name=\"Heading\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"text\">\n"
			++ "\t\t\t<style:paragraph-properties style:contextual-spacing=\"false\" fo:keep-with-next=\"always\"/>\n"
			++ "\t\t</style:style>\n"
			++ "\t\t<style:style style:name=\"Heading_20_1\" style:display-name=\"Heading 1\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:default-outline-level=\"1\" style:class=\"text\">\n"
			++ "\t\t\t<style:paragraph-properties fo:margin-top=\"5.3mm\" fo:margin-bottom=\"3.48mm\"/>\n"
			++ "\t\t\t<style:text-properties fo:font-size=\"140%\" fo:font-weight=\"bold\" style:font-size-asian=\"140%\" style:font-weight-asian=\"bold\" style:font-size-complex=\"140%\" style:font-weight-complex=\"bold\"/>\n"
			++ "\t\t</style:style>\n"
			++ "\t\t<style:style style:name=\"Heading_20_2\" style:display-name=\"Heading 2\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:default-outline-level=\"2\" style:class=\"text\">\n"
			++ "\t\t\t<style:paragraph-properties fo:margin-top=\"4.92mm\" fo:margin-bottom=\"2.27mm\"/>\n"
			++ "\t\t\t<style:text-properties fo:font-size=\"117%\" fo:font-weight=\"bold\" style:font-size-asian=\"117%\" style:font-weight-asian=\"bold\" style:font-size-complex=\"117%\" style:font-weight-complex=\"bold\"/>\n"
			++ "\t\t</style:style>\n"
			++ "\t\t<style:style style:name=\"Heading_20_3\" style:display-name=\"Heading 3\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:default-outline-level=\"3\" style:class=\"text\">\n"
			++ "\t\t\t<style:paragraph-properties fo:margin-top=\"4.92mm\" fo:margin-bottom=\"2.27mm\"/>\n"
			++ "\t\t\t<style:text-properties fo:font-weight=\"bold\" style:font-weight-asian=\"bold\" style:font-weight-complex=\"bold\"/>\n"
			++ "\t\t</style:style>\n"
			-- ++ "\t\t<style:style style:name=\"Heading_20_4\" style:display-name=\"Heading 4\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:default-outline-level=\"4\" style:class=\"text\">\n"
			-- ++ "\t\t\t<style:paragraph-properties fo:margin-top=\"4.92mm\" fo:margin-bottom=\"1.51mm\"/>\n"
			-- ++ "\t\t\t<style:text-properties fo:font-weight=\"bold\" style:font-weight-asian=\"bold\" style:font-weight-complex=\"bold\"/>\n"
			-- ++ "\t\t</style:style>\n"
			-- ++ "\t\t<style:style style:name=\"Heading_20_5\" style:display-name=\"Heading 5\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:default-outline-level=\"5\" style:class=\"text\">\n"
			-- ++ "\t\t\t<style:paragraph-properties fo:margin-top=\"4.92mm\" fo:margin-bottom=\"1.51mm\"/>\n"
			-- ++ "\t\t\t<style:text-properties fo:font-weight=\"bold\" style:font-weight-asian=\"bold\" style:font-weight-complex=\"bold\"/>\n"
			-- ++ "\t\t</style:style>\n"


-- | Write LaTeX sections as proper fodt sections.
secW :: Writer
secW e = do
	numb <- name' e >>= flip lookup secs
	return	(writeParEnd
				++ "<text:h text:style-name=\"Heading_20_" ++ numb ++ "\" "
				++ "text:outline-level=\"" ++ numb ++ "\">"
			,args e
			,"</text:h>" ++ writeParBeg False)


--																					}}}
-- = tilde ========================================================================={{{==


-- | Turn tildes into non-breaking spaces.
tldeP :: Procsr
tldeP [] = []
tldeP (tex:texs)
	| isCmd tex	= tex {args = tldeP (args tex)} : tldeP texs
	| isTxt tex	= tex {text = tldeRepl (text tex)} : tldeP texs
	| otherwise	= tex : tldeP texs
	where
	tldeRepl = replace '~' ' '


--																					}}}

-- = helpers ======================================================================={{{==


-- | Check if a line break.
isPar :: TexElem -> Bool
isPar e = isCmd e && name e == "#linebreak"


-- | Check if a sectioning command.
isSec :: TexElem -> Bool
isSec e = isCmd e && name e `elem` ["section","subsection","subsubsection","paragraph","subparagraph"]


-- | A safer deconstructor for name in TexCmd.
name' :: TexElem -> Maybe String
name' e = if isCmd e
	then Just (name e)
	else Nothing


-- | Replace a specific value in a list.
replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace x y (z:zs) = (if z==x then y else z) : replace x y zs


--																					}}}
