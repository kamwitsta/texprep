-- | The main module.
-- Exports all the submodules, and what is necessary to run TeXPrep, and to write a configuration file for it. See “texprep.conf.hs” for the details.
module TeXPrep
	(module TeXPrep.Cmd, module TeXPrep.Core
	,defaultConfig, texprep
	,debugMain,realMain
	) where


--																					{{{
import qualified Config.Dyre as Dyre
import Control.Monad (unless)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import System.IO (readFile)
import TeXPrep.Cmd
import TeXPrep.Core
--																					}}}


-- = default config ================================================================{{{==


-- | The default configuration.
-- Simply the one exported by TeXPrep.Cmd.
defaultConfig :: Config
defaultConfig = TeXPrep.Cmd.config


--																					}}}
-- = main =========================================================================={{{==


-- | A little trick required by Dyre.
-- This is the function that needs to be run in “main” in the config file.
texprep = Dyre.wrapMain
		Dyre.defaultParams
			{Dyre.projectName	= "texprep"
			,Dyre.realMain		= realMain
			,Dyre.showError		= showCfgErr
			,Dyre.configDir		= Just (fmap (++"/.config/texprep/") getHomeDirectory)}


-- | The real main.
-- Reads the .tex and .aux files (accepts exactly one cl argument) and outputs the converted .fodt to stdout.
realMain :: Config -> IO ()
realMain cfg = do
	-- command line args
	args <- getArgs
	-- assert args ok
	unless (length args == 1) (error $ "ERROR: wrong command line arguments.\n\n" ++ usageInfo)
	-- get file names
	let fName = dropExtension (head args)
	let fNames@(fAux,fTex) = (fName ++ ".aux", fName ++ ".tex")
	-- assert files exist
	fExist <- mapTupleM doesFileExist fNames
	unless (fst fExist) (error $ "\nERROR: file " ++ fAux ++ " does not exist.")
	unless (snd fExist) (error $ "\nERROR: file " ++ fTex ++ " does not exist.")
	-- read files
	files <- mapTupleM readFile fNames
	let fRead = mapTupleM readTex files
	case fRead of
		Left l	-> error $ show l
		Right r	-> putStrLn . writeFodt cfg . procTex cfg $ r

debugMain = do
	let fNames = ("/home/kamil/Tmp/testy/b.aux","/home/kamil/Tmp/testy/b.tex")
	files <- mapTupleM readFile fNames
	let fRead = mapTupleM readTex files
	case fRead of
		Left l	-> error $ show l
		Right r	-> putStrLn . writeFodt defaultConfig . procTex defaultConfig $ r


--																					}}}
-- = helpers ======================================================================={{{==


-- | MapM for tuples.
mapTupleM :: (Monad m) => (a -> m b) -> (a,a) -> m (b,b)
mapTupleM f (x,y) = do
	x' <- f x
	y' <- f y
	return (x',y')


-- | Required by Dyre.
showCfgErr :: Config -> String -> Config
showCfgErr cfg msg = cfg {errMsg = Just msg}


-- | Information on the usage.
usageInfo :: String
usageInfo = "TeXPrep: slightly automatic preparation of LaTeX for OO/InD/…\n"
			++ "Usage: texprep FILENAME\n\n"
			++ "See the documentation for more information."


--																					}}}
