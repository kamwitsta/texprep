{-
An example configuration file for TeXPrep.

In fact, this is the one the author of TeXPrep uses himself. You are, of course, free to do all the changes you like to it. Don’t be scared by the fact that it’s written in Haskell. Some things you can do without knowing the first thing about it, and with others I will be happy to help you. Just drop me a line at kamil ⟨dot⟩ stachowski ⟨at⟩ gmail ⟨dot⟩ com.

The config file must be ~/.config/texprep/texprep.hs.

Comments beginning with a “|” or “^” refer to what is after and before them, respectively.

This file contains some functions that are disabled by default, only for illustration. To enable them, add them to the appropriate lists in the “main config” section.
-}


import TeXPrep		-- ^ This one is necessary. Just copy it as is.


-- = main config ==================================================================={{{==


-- | This one is also necessary, but don’t just copy it mindlessly.
-- This is the configuration the author of TeXPrep uses; you might want to adjust it to your needs. Whatever changes you do to the config, must be reflected in here. The file may be full of functions but they will have no effect whatsoever if they are not listed here. See e.g. cmntW, which is included here but disable by default.
main = texprep $ defaultConfig
	-- | The errMsg is required for the configuration system (Dyre) to work.
	-- You can safely leave it as is.
	{errMsg		= Nothing
	-- | TeXPrep can’t figure out which of your custom commands are just aliases.
	-- If you use any, make a list here to have them converted just like the originals.
	-- The format is ("from","to"), comma separated.
	-- E.g. here, “b” is an alias for “textbf”, and so on.
	,aliases	= [("b","textbf"),("i","textit")]
	-- | Here go all the additional styles for the .fodt file.
	-- If you are not sure how to define them, the easiest way is to define a style in LibreOffice, save the file as .fodt, open it in a text editor (Vim, Emacs, &c.), and simply copy the appropriate definition. Don’t forget to escape the quotation marks.
	-- Note that if you want to make full use of TeXPrep, you will need to add (“++”) commands defined here to those already present in “defaultConfig”. You can also choose to not use them completely or cherry pick them. See the html docs.
	,styles		= styles defaultConfig ++ [subS]
	-- | Here you define what to do with certain commands between the parsing of the .tex file, and the writing of the .fodt file.
	-- You may have noticed subscripts only have a style (“subS”) and a writer (“subW”) defined for them. This is because they are easy commands and do not require any particular postprocessing. You don’t always have to define all three a style, and a procsr, and a writer.
	,procsrs	= procsrs defaultConfig ++ []
	-- | Finally, here you define the writers.
	-- Writers are supposed to output a triple, where
	-- – the 1st element is what should go in the .fodt file before the command,
	-- – the 2nd element is the arguments of the command that might be further processed,
	-- – and the 3rd element is what should go in the .fodt file after the command.
	-- See subW, this is a nice, easy example.
	-- Again, custom writers are added to the ones defined in “defaultConfig”.
	,writers	= writers defaultConfig ++ [subW]}


--																					}}}


-- = comments ======================================================================{{{==


-- | Suppress rendering of comments.
-- See subW for more details on how to write custom writers.
cmntW :: Writer
cmntW (TexCmnt _) = Just ("", [], "")
cmntW _ = Nothing


--																					}}}
-- = labels ========================================================================{{{==


-- | Suppress rendering of labels.
-- See subW for more details on how to write custom writers.
labW :: Writer
labW (TexCmd "label" _ _ _) = Just ("", [], "")
labW _ = Nothing


--																					}}}
-- = sub- and superscript =========================================================={{{==


-- | Fodt styles for sub- and superscripts.
-- The “\n” and “\t”’s are just to make it look tidier in the .fodt.
-- You don’t have to split the definition at every linebreak. I only did it here to make the whole easier to read for humans.
subS :: String
subS = "\t\t<style:style style:name=\"subscript\" style:family=\"text\">\n"
		++ "\t\t\t<style:text-properties style:text-position=\"sub 58%\"/>\n"
		++ "\t\t</style:style>\n"
		++ "\t\t<style:style style:name=\"superscript\" style:family=\"text\">\n"
		++ "\t\t\t<style:text-properties style:text-position=\"super 58%\"/>\n"
		++ "\t\t</style:style>\n"


-- | Write sub- and superscripts in the .fodt file.
-- All writers must output a triple (before, args, after), where “before” is whatever the .fodt file must contain before the actual arguments, “args” are the actual arguments, and “after” is what goes after them.
-- E.g., subscripts need to be written so in the .fodt file: “<text:span text:style-name="subscript">subscripted text</text:span>”. The first bit in the angle brackets is what goes before the actual subscripted text (“before”), the last bit in the brackets is “after”, and “subscripted text” is “args”.
-- Why all this can’t be one simple string is because commands can be nested in LaTeX. You might write, f.ex. “\sub{some text \textit{in italics} or not}”. Then, “\sub{” gets converted to “before”, the final “}” to “after”, and the whole thing in the middle must still be further processed so that the bit in italics gets rendered as actual italics rather than “some text <textit>in italics</textit> or not”.
-- Note that writers must only work for specific commands. Here, for commands “sub” and “sup”, “Just” the triple is returned, and for everything else (“_”), “Nothing”.
subW :: Writer
subW (TexCmd "sub" _ args _ ) = Just
			("<text:span text:style-name=\"subscript\">"
			,args
			,"</text:span>")
subW (TexCmd "sup" _ args _ ) = Just
			("<text:span text:style-name=\"superscript\">"
			,args
			,"</text:span>")
subW _ = Nothing


--																					}}}
