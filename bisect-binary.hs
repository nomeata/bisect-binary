{-# LANGUAGE LambdaCase, DeriveGeneric #-}

import qualified Data.ByteString.Lazy as BS
import Text.Printf
import System.Directory
import System.Exit
import System.FilePath
import Data.List hiding (union, subSetOf, intersect)
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
import Prelude hiding (subtract)
import qualified System.Console.Terminal.Size
import Data.Bits

type Offset = Int64

data Interval = I { from :: Offset, to :: Offset }
    deriving (Show,Generic)

newtype Intervals = Intervals [Interval]
    deriving (Show,Generic)

mkInterval :: Offset -> Offset -> Intervals
mkInterval f t | f < t     = Intervals [I f t]
               | otherwise = Intervals []

size :: Intervals -> Offset
size (Intervals is) = sum [ t - f | I f t <- is ]

isEmpty :: Intervals -> Bool
isEmpty (Intervals is) = null is

subSetOf :: Intervals -> Intervals -> Bool
subSetOf a b = isEmpty (a `subtract` b)

intersects :: Intervals -> Intervals -> Bool
intersects a b = not $ isEmpty (a `intersect` b)

intersect :: Intervals -> Intervals -> Intervals
intersect (Intervals is1) (Intervals is2) = Intervals $ go is1 is2
  where
    go is [] = []
    go [] is = []
    go (i1:is1) (i2:is2)
        -- reorder for symmetry
        | to i1 < to i2 = go (i2:is2) (i1:is1)
        -- disjoint
        | from i1 >= to i2 = go (i1:is1) is2
        -- overlapping
        | otherwise = I f' (to i2) : go (i1 { from = to i2} : is1) is2
            where f' = max (from i1) (from i2)


union :: Intervals -> Intervals -> Intervals
union (Intervals is1) (Intervals is2) = Intervals $ go is1 is2
  where
    go is [] = is
    go [] is = is
    go (i1:is1) (i2:is2)
        -- reorder for symmetry
        | to i1 < to i2 = go (i2:is2) (i1:is1)
        -- disjoint
        | from i1 > to i2 = i2 : go (i1:is1) is2
        -- overlapping
        | otherwise  = go (i1 { from = f'} : is1) is2
            where f' = min (from i1) (from i2)

subtract :: Intervals -> Intervals -> Intervals
subtract (Intervals is1) (Intervals is2) = Intervals $ go is1 is2
  where
    go is [] = is
    go [] _  = []
    go (i1:is1) (i2:is2)
        -- i2 past i1
        | to i1 <= from i2 = i1 : go is1 (i2:is2)
        -- i1 past i2
        | to i2 <= from i1 = go (i1:is1) is2
        -- i1 contained in i2
        | from i2 <= from i1 , to i1 <= to i2 = go is1 (i2:is2)
        -- i2 covers beginning of i1
        | from i1 >= from i2 = i1 { from = to i2} : go is1 is2
        -- i2 covers end of i1
        | to i1 <= to i2     = i1 { to = from i2} : go is1 (i2:is2)
        -- i2 in the middle of i1
        | otherwise = I (from i1) (from i2) :
                      I (to i2)   (to i1) : go is1 is2

data Cover = NoCover | SomeCover | FullCover
    deriving (Eq, Ord)

{-
todo: braille progress bar
(showing sure zeroes in the lower two lines, and the currently tried area in the upper two)
https://de.wikipedia.org/wiki/unicodeblock_braille-zeichen
-}
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

dotsToBrailleBar :: [Int] -> String
dotsToBrailleBar [] = ""
dotsToBrailleBar [x] = [dotsToBrailleChar x 0]
dotsToBrailleBar (x:y:xs) = dotsToBrailleChar x y : dotsToBrailleBar xs

dotsToBrailleChar :: Int -> Int -> Char
dotsToBrailleChar n m =
    chr $ 0x2800 + sum
        [ 2^0 * fromEnum (testBit n 3)
        , 2^1 * fromEnum (testBit n 2)
        , 2^2 * fromEnum (testBit n 1)
        , 2^3 * fromEnum (testBit m 3)
        , 2^4 * fromEnum (testBit m 2)
        , 2^5 * fromEnum (testBit m 1)
        , 2^6 * fromEnum (testBit n 0)
        , 2^7 * fromEnum (testBit m 0)
        ]


fullIntervals :: Offset -> Intervals
fullIntervals len = Intervals [I 0 len]

nullInterval :: Intervals
nullInterval = Intervals []

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

-- | Aggregate, minmal knowledge of which parts of the file are needed
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

setZeros :: BS.ByteString -> Intervals -> BS.ByteString
setZeros s (Intervals is) = foldl' go s is
  where
    go s (I f t) = prefix <> zeroes <> postfix
      where
        (tmp, postfix)     = BS.splitAt t s
        (prefix, _discard) = BS.splitAt f tmp
        zeroes = BS.replicate (t-f) 0

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

instance FromJSON Interval
instance ToJSON Interval
instance FromJSON Log
instance ToJSON Log
instance FromJSON LogEntry
instance ToJSON LogEntry
instance FromJSON Intervals
instance ToJSON Intervals
instance FromJSON Digest
instance ToJSON Digest

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

ppInterval :: Interval -> String
ppInterval (I f t) = printf "0x%04X-0x%04X" f t

ppIntervals :: Intervals -> String
ppIntervals (Intervals xs) = intercalate " " (map ppInterval xs)

ppDigest :: Digest -> String
ppDigest bnds = unlines $
    [ "At least: " ++ ppIntervals (conservative bnds) ] ++
    [ " but not: " ++ ppIntervals x | x <- needed bnds ]

getWidth :: IO Int
getWidth = maybe 80 System.Console.Terminal.Size.width <$>
    System.Console.Terminal.Size.size

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

        -- A single run
        test zeros = do
            liftIO $ BS.writeFile outputFP $ setZeros input zeros
            -- Run command here

        ask log msg = fix $ \loop -> do
            getInputChar msg >>= pure . fmap toUpper >>= \case
                Just 'Y' -> do
                    return True
                Just 'N' -> do
                    return False
                Just 'Q' -> do
                    revert (digestLog log)
                    liftIO $ exitSuccess
                Just '?' -> do
                    outputStrLn "Keys: Y: good. N: bad. R: rerun command. Q: Quit"
                    loop
                _ -> loop

        statusText :: MonadIO m => Intervals -> Intervals -> Bool -> m String
        statusText conservative toTry done = liftIO $ do
            w <- getWidth
            let barw = w - 5 -2 -6 -2 -5 -1 -10 -2 -9 -2 -3
            let zeroPerc = 100 * fromIntegral (size conservative) / fromIntegral len
            let nonZeroBytes = len - size conservative
            return $ printf "%4.1f%%  zeroes  %5d bytes left  %s%s"
                (zeroPerc :: Double) nonZeroBytes
                (if barw > 5 then braille barw len conservative toTry else "")
                (if done then "" else " [YNRQX?] ")


        -- Single step of the main loop
        step log toTry
            | pointless toTry digest = return log
            | otherwise = do
                let zeros = conservative digest `union` toTry
                test zeros
                result <- statusText (conservative digest) toTry False >>= ask log
                stamp <- liftIO $ getZonedTime
                let entry = LogEntry stamp result zeros
                let log' = log { lgLogs = lgLogs log ++ [entry] }
                liftIO $ encodeFile logFile log'
                return $ log'
          where
            digest = digestLog log

        -- Main loop
        steps log todo = do
            foldM step log todo
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

    steps initialLog (intervalsToTry len)

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



