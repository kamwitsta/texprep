module TeXPrep.Cmd
	(config
	) where


--																					{{{
import TeXPrep.Core
import Text.ParserCombinators.Parsec
--																					}}}


-- = config ========================================================================{{{==


config :: Config
config = Config
	{errMsg		= Nothing
	,styles		= [biStyl, fnStyl]
	,procsrs	= [biProc, fnProc]
	,writers	= [biWrit, fnWrit]}


--																					}}}


-- = bold italic ==================================================================={{{==


-- | Process bolds inside italics and the other way round.
biProc :: Procsr
biProc [] = []
biProc (tex:texs)
	| isCmd tex	= if (name tex `elem` ["textbf","textit"])
					then tex {args = biRepl (args tex)} : biProc texs
					else tex {args = biProc (args tex)} : biProc texs
	| otherwise	= tex : biProc texs
	where
	biRepl :: [TexElem] -> [TexElem]
	biRepl [] = []
	biRepl (t:ts)
		| isCmd t	= if (name t `elem` ["textbf","textit"])
						then t {name = "@textbfit", args = biRepl (args t)} : biRepl ts
						else t : biRepl ts
		| otherwise	= t : biRepl ts


-- | Fodt styles for bold, italic, and bold italic.
biStyl :: String
biStyl = "\t\t<style:style style:name=\"bold\" style:family=\"text\">\n"
			++ "\t\t\t<style:text-properties fo:font-weight=\"bold\" />\n"
			++ "\t\t</style:style>\n"
			++ "\t\t<style:style style:name=\"bolditalic\" style:family=\"text\">\n"
			++ "\t\t\t<style:text-properties fo:font-style=\"italic\" fo:font-weight=\"bold\" />\n"
			++ "\t\t</style:style>\n"
			++ "\t\t<style:style style:name=\"italic\" style:family=\"text\">\n"
			++ "\t\t\t<style:text-properties fo:font-style=\"italic\" />\n"
			++ "\t\t</style:style>\n"


-- | Write a bold, italic, or bold italic as a fodt.
biWrit :: Writer
biWrit (TexCmd "textbf" _ args _) = Just $
			("<text:span text:style-name=\"bold\">"
			,args
			,"</text:span>")
biWrit (TexCmd "@textbfit" _ args _) = Just $
			("<text:span text:style-name=\"bolditalic\">"
			,args
			,"</text:span>")
biWrit (TexCmd "textit" _ args _) = Just $
			("<text:span text:style-name=\"italic\">"
			,args
			,"</text:span>")
biWrit _ = Nothing


--																					}}}
-- = footnotes ====================================================================={{{==


-- | Numbers footnotes.
fnProc :: Procsr
fnProc = helper 1
	where
	helper :: Int -> Procsr
	helper _ [] = []
	helper n (tex:texs)
		| isCmd tex	= if (name tex == "footnote")
						then if (null (opts tex))
								then tex {cmnt = [show n]} : helper (n+1) texs
								else tex {cmnt = opts tex} : helper n texs
						else tex {args = helper n (args tex)} : helper n texs
		| otherwise	= tex : helper n texs


-- | Fodt style for footnotes.
fnStyl :: String
fnStyl = "\t\t<style:style style:name=\"Footnote\" style:family=\"paragraph\" />\n"


-- | Write footnotes as actual fodt footnotes.
fnWrit :: Writer
fnWrit (TexCmd "footnote" _ args cmnt) = let fnNr = (read (head cmnt)) in Just $
			("<text:note text:id=\"ftn" ++ (show (fnNr-1)) ++ "\" text:note-class=\"footnote\">"
				++ "<text:note-citation text:label=\"" ++ (show fnNr) ++ "\">" ++ (show fnNr) ++ "</text:note-citation>"
				++ "<text:note-body>"
				++ writeParBeg
			,args
			,writeParEnd ++ "</text:note-body></text:note>")
fnWrit _ = Nothing


--																					}}}
