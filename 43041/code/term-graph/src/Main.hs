{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main where
  import Prelude hiding (lookup)

  import GHC.TypeNats                  (natVal)
  import Data.Mod.Word                 (Mod,unMod)

  -- Copied from http://hackage.haskell.org/package/quickcheck-arbitrary-adt-0.3.1.0/docs/Test-QuickCheck-Arbitrary-ADT.html
  import Data.Proxy
  import GHC.Generics
  import Test.QuickCheck
  import Test.QuickCheck.Arbitrary.ADT

  import Control.Monad.State           (runState,State,state)
  import Data.Map                      (fromList,insert,lookup,Map,size)
  import Data.Graph                    (edges,Graph,Vertex,vertices)
  import Data.Maybe                    (fromJust,isNothing)
  import Data.Array                    (array,(!))
  import System.Environment            (getArgs,getProgName)
  import System.Exit                   (ExitCode(..),exitWith)

  --
  type NVars = 3
  nvars :: Int
  nvars = fromInteger . toInteger . natVal $ (Proxy :: Proxy NVars)

  fromInt :: Int -> Mod NVars
  fromInt = fromInteger . toInteger

  toInt :: Mod NVars -> Int
  toInt = fromInteger . toInteger . unMod

  instance Arbitrary (Mod NVars) where
    arbitrary = do
      x <- genericArbitrary
      return $ fromInteger . toInteger . unMod $ x

  --
  data Term
    = Var (Mod NVars)
    | Fun0
    | Fun1 Term
    | Fun2 Term Term
    | Fun3 Term Term Term
    deriving (Eq,Generic,Ord)

  instance Show Term where
    show (Var i)      = "x" ++ show (unMod i)
    show  Fun0        = "c"
    show (Fun1 t)     = "f(" ++ show t ++ ")"
    show (Fun2 t s)   = "g(" ++ show t ++ "," ++ show s ++ ")"
    show (Fun3 t s r) = "h(" ++ show t ++ "," ++ show s ++ "," ++ show r ++ ")"

  instance Arbitrary Term where
    arbitrary = genericArbitrary

  instance ToADTArbitrary Term

  --
  data MyState = MyState {
      dict :: Map Term Vertex,
      grph :: [(Vertex,[Vertex])]
    } deriving (Show)

  initMyState :: MyState
  initMyState = MyState {
      dict = fromList [ (Var $ fromInt i, i)  | i <- [0 .. (nvars - 1)] ],
      grph =          [               (i, []) | i <- [0 .. (nvars - 1)] ]
    }

  sizeMyState = size . dict

  --
  lookupTermM :: Term -> State MyState (Maybe Vertex)
  lookupTermM t = state $ \s -> (lookup t (dict s), s)

  insertTermM :: Term -> [Vertex] -> State MyState Vertex
  insertTermM t vs = state $ \s -> let
    v = size (dict s)
    in (v, MyState {
        dict = insert t v (dict s),
        grph = (v,vs):(grph s)
      })

  --
  toArrayM :: Term -> State MyState Vertex
  toArrayM t = do
    m <- lookupTermM t
    case m of
      Just v -> return v
      Nothing -> case t of
        Fun3 s r q -> do { v <- toArrayM s ; u <- toArrayM r ; w <- toArrayM q ; insertTermM t [v,u,w] }
        Fun2 s r   -> do { v <- toArrayM s ; u <- toArrayM r ;                   insertTermM t [v,u] }
        Fun1 s     -> do { v <- toArrayM s ;                                     insertTermM t [v] }
        _          ->                                                            insertTermM t []

  toGraph :: Term -> (Graph,Vertex)
  toGraph t = (array (0, sizeMyState s - 1) (grph s), v)
    where (v,s) = runState (toArrayM t) initMyState

  --
  toTermM :: Graph -> Vertex -> Maybe Term
  toTermM g v = let
    ms = map (toTermM g) (g ! v)
    in if any isNothing ms then Nothing else let
      ts = map fromJust ms
      in case (length ts) of
        0 -> Just $ if v < nvars then Var (fromInt v) else Fun0
        1 -> let [t]     = ts in Just $ Fun1 t
        2 -> let [t,s]   = ts in Just $ Fun2 t s
        3 -> let [t,s,r] = ts in Just $ Fun3 t s r
        _ -> Nothing

  toTerm :: (Graph,Vertex) -> Term
  toTerm (g,r) = fromJust $ toTermM g r

  --
  prop_toTerm_retract_toGraph_section :: Term -> Bool
  prop_toTerm_retract_toGraph_section t = t == (toTerm . toGraph) t

  --
  main :: IO ()
  main = getArgs >>= parse >>= mainLoop

  parse ["-t"] = quickCheck (withMaxSuccess 10000 prop_toTerm_retract_toGraph_section) >> exitWith ExitSuccess
  parse [v,e]  = return (read v, read e)
  parse []     = return (8,16)
  parse _      = getProgName >>= usage >> exitWith ExitSuccess

  usage progName = putStrLn $ "Usage: " ++ progName ++ " [-t | #vertices #edges]"

  mainLoop (v,e) = do
    t <- generate (arbitrary :: Gen Term)
    let
      (g,_) = toGraph t
      dV = abs $ v - length (vertices g)
      dE = abs $ e - length (edges g)
      in if dV < 2 && dE < 4
        then do
          putStrLn $ "; " ++ show t
          putStr $ foldr1 (++) [ show i ++ " -> " ++ show j ++ "\n" | (i,j) <- edges g ]
        else
          mainLoop (v,e)
