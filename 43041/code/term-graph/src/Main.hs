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

  --
  type NVars = 3
  nvars :: Int
  nvars = fromInteger . toInteger . natVal $ (Proxy :: Proxy NVars)

  type FVars = Mod NVars

  fromInt :: Int -> FVars
  fromInt = fromInteger . toInteger

  toInt :: FVars -> Int
  toInt = fromInteger . toInteger . unMod

  instance Arbitrary FVars where
    arbitrary = do
      x <- genericArbitrary
      return $ fromInteger . toInteger . unMod $ x

  --
  data Term
    = Var FVars
    | Fun0
    | Fun1 Term
    | Fun2 Term Term
    deriving (Eq,Generic,Ord)

  instance Show Term where
    show (Var i)    = "x" ++ show (unMod i)
    show  Fun0      = "c"
    show (Fun1 t)   = "f(" ++ show t ++ ")"
    show (Fun2 t s) = "g(" ++ show t ++ "," ++ show s ++ ")"

  instance Arbitrary Term where
    arbitrary = genericArbitrary

  instance ToADTArbitrary Term

  --
  data MyState = MyState {
      dict :: Map Term Vertex,
      grph :: [(Vertex,[Vertex])]
    } deriving (Show)

  initMyState :: MyState
  initMyState = let
    fvars = [0 .. (nvars - 1)]
    in MyState {
        dict = fromList $ map (\i -> (Var $ fromInt i, i)) fvars,
        grph = zip fvars (repeat [])
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
        Fun2 s r -> do { v <- toArrayM s ; u <- toArrayM r ; insertTermM t [v,u] }
        Fun1 s   -> do { v <- toArrayM s ;                   insertTermM t [v] }
        _        ->                                          insertTermM t []

  toGraph :: Term -> (Graph,Vertex)
  toGraph t = let
    (v,s) = runState (toArrayM t) initMyState
    in (array (0, sizeMyState s - 1) (grph s), v)

  --
  toTermM :: Graph -> Vertex -> Maybe Term
  toTermM g v = let
    ms = map (toTermM g) (g ! v)
    in if any isNothing ms then Nothing else let
      ts = map fromJust ms
      in case (length ts) of
        0 -> Just $ if v < nvars then Var (fromInt v) else Fun0
        1 -> let [t]   = ts  in Just $ Fun1 t
        2 -> let [t,s] = ts  in Just $ Fun2 t s
        _ -> Nothing

  toTerm :: (Graph,Vertex) -> Term
  toTerm (g,r) = fromJust $ toTermM g r

  --
  prop_toTerm_retract_toGraph_section :: Term -> Bool
  prop_toTerm_retract_toGraph_section t = t == (toTerm . toGraph) t

  --
  main :: IO ()
  main = do
    -- quickCheck prop_toTerm_retract_toGraph_section
    t <- generate (arbitrary :: Gen Term)
    let
      (g,r) = toGraph t
      nE = length $ edges g
      nV = length $ vertices g
      in if nV < nE && nE < 12
        then
          putStrLn $ foldr1 (++) $ map (\(i,j) -> show i ++ " -> " ++ show j ++ "\n") (edges g)
        else
          main
