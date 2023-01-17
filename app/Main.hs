import System.Environment
import System.IO           (hPutStrLn, isEOF, stderr)
import Control.Monad       (liftM, ap, when)
import Data.Maybe          (fromMaybe)
import Data.List           (find)
import System.Console.GetOpt
import System.Exit (exitSuccess)

data Cmd 
  = JumpBegin 
  | JumpEnd
  | IncrementPtr
  | DecrementPtr
  | Increment
  | Decrement 
  | Put
  | Get deriving (Enum, Bounded)

showCmd                  :: Cmd -> String -> String
showCmd JumpBegin s    = '[' : s
showCmd JumpEnd s      = ']' : s
showCmd IncrementPtr s = '>' : s
showCmd DecrementPtr s = '<' : s
showCmd Increment s    = '+' : s
showCmd Decrement s    = '-' : s
showCmd Put s          = '.' : s
showCmd Get s          = ',' : s

isStep             :: Cmd -> Bool
isStep JumpBegin = False
isStep JumpEnd   = False
isStep _           = True

cmdFromChar :: Char -> Maybe Cmd
cmdFromChar c = find ((==c:"") . show) ([minBound..] :: [Cmd])

isCmd :: Char -> Bool
isCmd c = case cmdFromChar c of
               Nothing -> False
               (Just _) -> True

readsCmd :: String -> [(Cmd, String)]
readsCmd "" = []
readsCmd (c:s) = case cmdFromChar c of
    Nothing -> readsCmd s
    Just x -> [(x, s)]

instance Show Cmd where
    showsPrec _ = showCmd

instance Read Cmd where
    readsPrec _ = readsCmd

data Program = Empty | Step Cmd Program | Loop Program Program

showsProgram :: Program -> ShowS
showsProgram Empty = ("(Empty)"++)
showsProgram (Step c rs) = ("(Step "++) . shows c . (' ':) . showsProgram rs . (')':)
showsProgram (Loop p rs) = ("(Loop "++) . showsProgram p . (' ':) . showsProgram rs . (')':)

instance Show Program where
    showsPrec _ = showsProgram

-- very inefficient parser
readsProgram :: ReadS Program
readsProgram "" = [ (Empty, "") ]
readsProgram s  = [(Step c r, u) |
                     (c, t) <- reads s, isStep c, (r, u) <- readsProgram t]
                  ++
                  [ (Loop p rs, w) | (JumpBegin, t) <- reads s,
                                     (p, u)         <- readsProgram t,
                                     (JumpEnd, v)   <- reads u,
                                     (rs, w)        <- readsProgram v ]
                  ++
                  case s of
                     (']':_) -> [ (Empty, s) ]
                     _ -> []

-- a little bit better but still n**2 complexity
readsProgram2 :: String -> Int -> Int -> Program
readsProgram2 s l r =
  if r < l then Empty else
  case s !! l of
    ']' -> error "unmatched brackets2"
    '[' -> let r'' = matchClose (0::Int) (l+1) in Loop (readsProgram2 s (l+1) (r''-1)) (readsProgram2 s (r''+1) r)
                    where matchClose skip r'
                            | r' > r = error "unmatched brackets"
                            | (s !! r') == '[' = matchClose (skip+1) (r'+1)
                            | (s !! r') == ']' = if skip == 0 then r' else matchClose (skip-1) (r'+1)
                            | otherwise = matchClose skip (r'+1)
    c -> case cmdFromChar c of
                   Nothing -> readsProgram2 s (l+1) r
                   Just v -> Step v (readsProgram2 s (l+1) r)


instance Read Program where
    readsPrec _ = readsProgram

data State = State { statePtr :: Int,
                     stateMem :: [(Int, Int)],
                     stateWaitingInput, stateWaitingOutput :: Bool,
                     stateInput, stateOutput :: Char,
                     stateMaxCell, stateMaxValue :: Int,
                     stateNoCellWrap, stateNoValueWrap :: Bool } deriving (Show)

getV :: State -> Int
getV s = let p = statePtr s in
         let m = stateMem s in
             fromMaybe 0 (lookup p m)

valueClamper :: State -> Int -> Int
valueClamper s x = if stateNoValueWrap s
                   then x
                   else (`mod` stateMaxValue s) . (+ stateMaxValue s) $ x

updateV :: (Int -> Int) -> State -> State
updateV f s = let curValue = getV s in
              let newValue = valueClamper s . f $ curValue in
              let p = statePtr s in
              let newMem = ((p, newValue) :) . filter ((/=p) . fst) $ stateMem s in
                  s{stateMem = newMem}

cellClamper :: State -> Int -> Int
cellClamper s x = if stateNoCellWrap s
                  then x
                  else (`mod` stateMaxCell s) . (+ stateMaxCell s) $ x

updateP :: (Int -> Int) -> State -> State
updateP f s = let newPtr = cellClamper s . f $ statePtr s in
                  s{statePtr = newPtr}


newtype Pause s a = Pause { runPause :: s -> (PauseResult s a, s) }

data PauseResult s a
    = Done a
    | Suspend (Pause s a)

instance Functor (Pause s) where
    fmap = liftM

instance Applicative (Pause s) where
    pure a = Pause (Done a, )
    (<*>) = ap

instance Monad (Pause s) where
    return = pure
    m >>= k = Pause $ \s ->
        case runPause m s of
            (Done a, s') -> runPause (k a) s'
            (Suspend m', s') -> (Suspend (m' >>= k), s')

get :: Pause s s
get = Pause (\s -> (Done s, s))

put :: s -> Pause s ()
put s = Pause (const (Done (), s))

yield :: Pause s ()
yield = Pause (Suspend (return()), )

step :: Pause s () -> s -> (Maybe (Pause s()), s)
step m s =
    case runPause m s of
        (Done _, s') -> (Nothing, s')
        (Suspend m', s') -> (Just m', s')

evalP :: Cmd -> Pause State ()
evalP Increment = do s <- get
                     put $ updateV (+1) s
evalP Decrement = do s <- get
                     put $ updateV (+ (-1)) s
evalP IncrementPtr = do s <- get
                        put $ updateP (+1) s
evalP DecrementPtr = do s <- get
                        put $ updateP (+ (-1)) s
evalP Get = do s <- get
               put $ s{stateWaitingInput = True}
               yield
               s' <- get
               put $ updateV (\_ -> fromEnum $ stateInput s') s
evalP Put = do s <- get
               let curValue = getV s in
                 do put $ s{stateWaitingOutput = True, stateOutput = toEnum curValue}
                    yield
                    put s
evalP JumpBegin = error "incorrectly parsed program"
evalP JumpEnd = error "incorrectly parsed program"


execP :: Program -> Pause State ()
execP Empty = return ()
execP (Step c rs) = evalP c >> execP rs
execP l@(Loop p rs) = do s <- get
                         let v = getV s in
                             if v > 0 then execP p >> execP l
                             else execP rs

charOrZero :: IO Char
charOrZero = do done <- isEOF
                if done then return (toEnum 0 :: Char) else getChar

recInterpretP :: State -> Pause State () -> IO State
recInterpretP s p = case step p s of
                        (Nothing, r) -> return r
                        (Just cont, s') -> if stateWaitingInput s' then
                                             do c <- charOrZero
                                                recInterpretP s'{stateInput = c} cont
                                           else
                                             let c = stateOutput s' in
                                                 do putChar c
                                                    recInterpretP s' cont

interpretP :: State -> Program -> IO State
interpretP s p = recInterpretP s (execP p)

state :: [Flag] -> State
state [] = State 0 [] False False 'a' 'a' 30000 256 False False
state ((Cells c):fs) = (state fs){stateMaxCell=c}
state (NoCellWrap:fs) = (state fs){stateNoCellWrap=True}
state (NoValueWrap:fs) = (state fs){stateNoValueWrap=True}
state ((MaxValue v):fs) = (state fs){stateMaxValue=v}
state (_:fs) = state fs

data Flag
    = Bang             -- -b
    | Cells Int        -- -c
    | NoCellWrap       -- --no-cell-wrap
    | MaxValue Int     -- -v
    | NoValueWrap      -- --no-value-wrap
    | DumpState        -- --dump-state
    | Help             -- -h --help
    deriving (Show, Eq)

flags :: [OptDescr Flag]
flags =
    [ Option ['b'] []             (NoArg Bang)
        "TODO: Program file contains its input before a ! character."
    , Option ['c'] []             (OptArg cells "30000")
        "Number of cells available to the program. Ignored when --no-cell-wrap is set."
    , Option [] ["no-cell-wrap"]  (NoArg NoCellWrap)
        "Disables cell wrapping. Virtually infinite memory."
    , Option ['m'] []             (OptArg maxv "256")
        "Maximum value of a cell. Ignored when --no-value-wrap is set."
    , Option [] ["no-value-wrap"] (NoArg NoValueWrap)
        "Disables value wrapping, i.e allows negative values on memory"
    , Option ['h'] ["help"]       (NoArg Help)
        "Shows this message"
    , Option ['d'] ["dump-state"] (NoArg DumpState)
        "Dump interpreter state at the end of execution"
    ]

cells, maxv :: Maybe String -> Flag
cells = Cells . read . fromMaybe "30000"
maxv = MaxValue . read . fromMaybe "8"

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
    case getOpt Permute flags argv of
        (o,fs,[]) -> 
            if null fs 
                then do 
                        hPutStrLn stderr (usageInfo header flags)
                        exitSuccess
            else
                return (o, fs)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header flags))
    where header = "Usage: bfhs [opts] file"

main :: IO ()
main = do (opts, fs) <- getArgs >>= parseOpts
          let s0 = state opts in
            mapM_ (\s -> do inp <- readFile s
                            let sp = prepareInput inp
                                p = readsProgram2 sp 0 (length sp - 1) in
                                do s' <- interpretP s0 p
                                   when (DumpState `elem` opts)
                                        (putStrLn "" >> print s')) fs

prepareInput :: String -> String
prepareInput = concatMap(filter isCmd . takeWhile (/='#')) . lines
