module TeXPrep
	(module TeXPrep.Core
	,defaultConfig
	) where


--																					{{{
import TeXPrep.Cmd
import TeXPrep.Core
--																					}}}


-- = default config ================================================================{{{==


defaultConfig :: Config
defaultConfig = TeXPrep.Cmd.config


--																					}}}
