{-# LANGUAGE LambdaCase, DeriveGeneric #-}

import qualified Data.ByteString.Lazy as BS
import Text.Printf
import System.Directory
import System.Exit
import System.FilePath
import Data.Foldable
import Data.List hiding (union, intersect)
import Options.Applicative
import Control.Monad
import Data.Monoid ((<>))
import Data.Yaml (decodeFileEither, encodeFile, prettyPrintParseException, ToJSON, FromJSON)
import Data.Bifunctor
import GHC.Generics (Generic)
import System.Console.Haskeline
import Data.Function
import Control.Monad.IO.Class
import Data.Int
import Data.Time
import Data.Hashable
import Math.NumberTheory.Logarithms
import Data.Char
import System.Process
import Prelude hiding (subtract)
import qualified System.Console.Terminal.Size

import Intervals
import Braille

-- Core idea: The intervals to try to zero out

intervalsToTry :: Offset -> [Intervals]
intervalsToTry len =
    [ mkInterval b (b+size)
    | level <- toZero big
    , let size = 2^level
    , let shift = if odd (level - big) then size`div`2 else 0
    , let upper = len - size - shift
    , b <- [upper, upper - size .. 0]
    ]
  where big = integerLog2 (fromIntegral len)
        toZero n = [n,(n-1)..0]

-- Data storage

data Log = Log
    { lgFileSize :: Offset
    , lgFileHash :: Int
    , lgLogs :: [LogEntry]
    }
    deriving (Show,Generic)

data LogEntry = LogEntry
    { leDate :: ZonedTime
    , leOk :: Bool
    , leZeroed :: Intervals
    }
    deriving (Show,Generic)

instance FromJSON Log
instance ToJSON Log
instance FromJSON LogEntry
instance ToJSON LogEntry

initLog :: BS.ByteString -> Log
initLog input = Log
    { lgFileSize = BS.length input
    , lgFileHash = hash input
    , lgLogs = []
    }

checkLog :: Log -> BS.ByteString -> InputT IO Log
checkLog log input
    | lgFileSize log /= BS.length input = do
        outputStrLn $ printf "ERROR: Log input file size was %i, current input file is %i. Aborting" (lgFileSize log) (BS.length input)
        liftIO $ exitFailure
    | lgFileHash log /= hash input = do
        outputStrLn "WARN: Log input file hash differns from actual input file hash."
        outputStrLn "Do you want to continue?"
        getInputChar "Y/N?" >>= \case
            Just 'Y' -> return log
            _ -> do
                outputStrLn "Goodbye!"
                liftIO $ exitFailure
    | otherwise = do
        outputStrLn $ printf "Loaded log file with %d previous attempts." (length (lgLogs log))
        return log

-- | Aggregate, minmal knowledge of which parts of the file are needed
--
data Digest = Digest
    { conservative :: Intervals
        -- ^ These bits can safely be zeroes
    , needed :: [Intervals]
        -- ^ At least one byte in each of these is needed.
        --   (Invariant: These are disjoint from the conservative ones)
    }
    deriving (Show,Generic)

digestLog :: Log -> Digest
digestLog log = Digest conservative needed
  where
    (okEntries, badEntries) =
        bimap (map leZeroed) (map leZeroed) $
        partition leOk (lgLogs log)

    conservative = foldl' union nullInterval okEntries
    needed = prune (map (`subtract` conservative) badEntries)

    -- could remove subsumed entries here
    prune = id

-- | It is pointless trying to zero an interval if it is a subset of what we
-- already know can be zeroes, or if any failed case in the past shows that
-- is a subset of this.
pointless :: Intervals -> Digest -> Bool
pointless try digest =
    try `subSetOf` conservative digest ||
    any (`subSetOf` try) (needed digest)

-- The main code

work :: FilePath -> FilePath -> Maybe String -> IO ()
work inputFP outputFP commandM = runInputT defaultSettings $ do
    input <- liftIO $ BS.readFile inputFP
    let len = BS.length input

    when (len == 0) $ do
        outputStrLn $ printf "%s is empty." inputFP
        liftIO $ exitSuccess

    let logFile = outputFP <.> "bisect.log"

    let revert digest = do
            statusText (conservative digest) nullInterval True >>= outputStrLn
            liftIO $ BS.writeFile outputFP $ setZeros input (conservative digest)
            outputStrLn $ printf "Reverted %s to last known good output." outputFP

        run_cmd = liftIO $ for_ commandM $ \cmd -> do
            ph <- spawnCommand cmd
            waitForProcess ph

        -- A single run
        test zeros = do
            liftIO $ BS.writeFile outputFP $ setZeros input zeros
            run_cmd

        ask log msg = fix $ \loop -> do
            getInputChar msg >>= pure . fmap toUpper >>= \case
                Just 'Y' -> do
                    return True
                Just 'N' -> do
                    return False
                Just 'Q' -> do
                    revert (digestLog log)
                    liftIO $ exitSuccess
                Just 'R' -> do
                    run_cmd
                    loop
                Just 'U' -> do
                    let log' = log { lgLogs = init (lgLogs log) }
                    steps log'
                    -- code smell
                    liftIO $ exitSuccess
                Just '?' -> do
                    outputStrLn "Keys: Y: good. N: bad. R: rerun command. U: Undo. Q: Quit"
                    loop
                _ -> loop

        statusText :: MonadIO m => Intervals -> Intervals -> Bool -> m String
        statusText conservative toTry done = liftIO $ do
            w <- getWidth
            let barw = w - 57
            let zeroPerc = 100 * fromIntegral (size conservative) / fromIntegral len
            let nonZeroBytes = len - size conservative
            return $ printf "%4.1f%% zeroes  %12s  %7dB left  %s%s"
                (zeroPerc :: Double)
                (if done then "" else printf "%7dB new" (size toTry))
                nonZeroBytes
                (if barw > 5 then braille barw len conservative toTry else "")
                (if done then "" else " [YNRUQ?] ")


        -- Single step of the main loop
        step log toTry
            | pointless toTry digest = return log
            | otherwise = do
                let zeros = conservative digest `union` toTry
                test zeros
                result <- statusText (conservative digest) toTry False >>= ask log
                stamp <- liftIO $ getZonedTime
                let entry = LogEntry { leDate = stamp, leOk = result, leZeroed = zeros }
                let log' = log { lgLogs = lgLogs log ++ [entry] }
                liftIO $ encodeFile logFile log'
                return $ log'
          where
            digest = digestLog log

        -- Main loop
        steps log = do
            foldM_ step log (intervalsToTry len)
            outputStrLn $ printf "Done!"
            -- TODO: What now?


    -- Initialization
    initialLog <- liftIO (doesFileExist logFile) >>= \case
        False -> do
            outputStrLn $ printf "Cannot find %s, starting from scratch." logFile
            return (initLog input)
        True -> do
           liftIO (decodeFileEither logFile) >>= \case
                Left error -> do
                    outputStrLn $ printf "ERROR: Cannot parse %s:"
                    outputStrLn $ prettyPrintParseException error
                    liftIO $ exitFailure
                Right log -> do
                    outputStrLn $ printf "Loading log file %s." logFile
                    checkLog log input

    steps initialLog

-- Argument handling

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "Binary file bissector"
  <> progDesc "Fills a file with as much zeroes as possible"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> strOption
            (  long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "input file"
            )
        <*> strOption
            (  long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "output file"
            )
        <*> optional (strOption
            (  long "command"
            <> short 'c'
            <> metavar "COMMAND"
            <> help "command to run"
            ))

-- Pretty progress bar using braille symbols:

data Cover = NoCover | SomeCover | FullCover
    deriving (Eq, Ord)

braille :: Int -> Offset -> Intervals -> Intervals -> String
braille width len lower upper = "[" ++ bar ++ "]"
  where
    bar = dotsToBrailleBar (map (toBits . go) parts)

    parts :: [Intervals] -- (width-2)*2 intervals
    parts = [mkInterval n (min (n + step) len) |  n <- [0,step..len-1] ]
        where step = len `div` fromIntegral (2*(width - 2))

    descOverlap :: Intervals -> Intervals -> Cover
    descOverlap big small
        | small `subSetOf`   big = FullCover
        | small `intersects` big = SomeCover
        | otherwise              = NoCover

    go :: Intervals -> (Cover, Cover)
    go i = (lower `descOverlap` i, upper `descOverlap` i)

    toBits :: (Cover, Cover) -> Int
    toBits (c1, c2) = sum
        [ 1 * fromEnum (c1 >= SomeCover)
        , 2 * fromEnum (c1 >= FullCover)
        , 4 * fromEnum (c2 >= FullCover)
        , 8 * fromEnum (c2 >= SomeCover)
        ]


getWidth :: IO Int
getWidth = maybe 80 System.Console.Terminal.Size.width <$>
    System.Console.Terminal.Size.size

