{-|
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Module		: TeXPrep
-- License		: GPL3+
-- Author		: Kamil Stachowski <kamil.stachowski “at” gmail.com>
-- Stability	: unstable
-- Portability	: unportable
-- TeXPrep: slightly automatic preparation of LaTeX for OO/InD/…
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-}


module Main where


--																					{{{
import qualified Config.Dyre as Dyre
import Control.Monad (liftM, unless)
import System.Console.GetOpt
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FilePath (dropExtension)
import System.IO (readFile)
import TeXPrep
--																					}}}

-- = main =========================================================================={{{==


-- | A little trick required by Dyre.
main = Dyre.wrapMain
		Dyre.defaultParams
			{Dyre.projectName	= "texprep"
			,Dyre.realMain		= realMain
			,Dyre.showError		= showCfgErr
			,Dyre.configDir		= Just (fmap (++"/.config/texprep/") getHomeDirectory)}
		defaultConfig


-- | Accepts exactly one cl argument and outputs to stdout
realMain :: Config -> IO ()
realMain cfg = do
	-- command line args
	clArgs <- getArgs
	let (clFlags, clNonOpts, clErrors) = getOpt Permute clOptions clArgs
	clOpts <- foldl (>>=) (return clOptionsStart) clFlags
	let CLOptions {} = clOpts
	-- assert args ok
	unless (length clNonOpts == 1) (error $ usageInfo clHeader clOptions)
	-- get file names
	let fName = dropExtension (head clNonOpts)
	let fNames@(fAux,fTex) = (fName ++ ".aux", fName ++ ".tex")
	-- assert files exist
	fExist <- mapTupleM doesFileExist fNames
	unless (fst fExist) (error $ "\nFatal error: file " ++ fAux ++ " does not exist.")
	unless (snd fExist) (error $ "\nFatal error: file " ++ fTex ++ " does not exist.")
	-- read files
	files <- mapTupleM readFile fNames
	let fRead = mapTupleM readTex files
	case fRead of
		Left l	-> error $ show l
		Right r	-> putStrLn . writeFodt cfg . procTex cfg $ r
		-- Right r	-> print (fst r)


--																					}}}
-- = cli args ======================================================================{{{==


-- | Information about the program.
clHeader = "slightly automatic preparation of LaTeX for OO/InD/…\n\n"
			++ "Usage: texprep [OPTIONS] FILE"


-- | Default cl options.
clOptionsStart = CLOptions {}


-- | For storing cl options.
data CLOptions = CLOptions {}


-- | List of available cl options.
clOptions :: [OptDescr (CLOptions -> IO CLOptions)]
clOptions = [
	Option "h" ["help"] (NoArg
		(\_ -> do
			putStrLn (usageInfo clHeader clOptions)
			exitSuccess))
		"display this information"
	]


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

--																					}}}
