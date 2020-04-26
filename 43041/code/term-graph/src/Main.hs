{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Main where
  import Prelude hiding (lookup)

  import GHC.TypeNats                  (natVal)
  import Data.Mod.Word                 (Mod,unMod)
  import Data.Tuple                    (fst,snd,swap)

  -- Copied from http://hackage.haskell.org/package/quickcheck-arbitrary-adt-0.3.1.0/docs/Test-QuickCheck-Arbitrary-ADT.html
  import Data.Proxy
  import GHC.Generics
  import Test.QuickCheck
  import Test.QuickCheck.Arbitrary.ADT

  import Control.Monad.State           (runState,State,state)
  import Data.Map                      (fromList,insert,lookup,Map,size)
  import Data.Graph                    (dff,edges,Graph,Tree(..),Vertex,vertices)
  import Data.Array                    (array,bounds,elems,indices,listArray,(!))
  import Data.Permute                  (at,listPermute,Permute)
  import Data.Maybe                    (fromJust,isNothing)
  import System.Environment            (getArgs,getProgName)
  import System.Exit                   (ExitCode(..),exitWith)

  --
  type NVars = 3
  nvars :: Int
  nvars = fromInteger $ toInteger $ natVal (Proxy :: Proxy NVars)

  fromInt :: Int -> Mod NVars
  fromInt = fromInteger . toInteger

  toInt :: Mod NVars -> Int
  toInt = fromInteger . toInteger . unMod

  instance Read (Mod NVars) where
    readsPrec _ = map (swap . fmap fromInt . swap) . reads

  instance Arbitrary (Mod NVars) where
    arbitrary = do
      x <- genericArbitrary
      return $ fromInteger $ toInteger $ unMod x

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

  instance Read Term where
    readsPrec p ('x':s) = [ (Var i, s') | (i,s') <- reads s ]
    readsPrec p str0    =
      [ (Fun0,       str1) | ("c", str1) <-         lex str0 ] ++
      [ (Fun1 t,     str4) | ("f", str1) <-         lex str0,
                             ("(", str2) <-         lex str1,
                               (t, str3) <- readsPrec p str2,
                             (")", str4) <-         lex str3 ] ++
      [ (Fun2 t s,   str6) | ("g", str1) <-         lex str0,
                             ("(", str2) <-         lex str1,
                               (t, str3) <- readsPrec p str2,
                              (",",str4) <-         lex str3,
                               (s, str5) <- readsPrec p str4,
                             (")", str6) <-         lex str5 ] ++
      [ (Fun3 t s r, str8) | ("h", str1) <-         lex str0,
                             ("(", str2) <-         lex str1,
                               (t, str3) <- readsPrec p str2,
                              (",",str4) <-         lex str3,
                               (s, str5) <- readsPrec p str4,
                              (",",str6) <-         lex str5,
                               (r, str7) <- readsPrec p str6,
                             (")", str8) <-         lex str7 ]

  instance Arbitrary Term where
    arbitrary = genericArbitrary

  instance ToADTArbitrary Term

  sizeTerm :: Term -> Int
  sizeTerm (Var _)      = 1
  sizeTerm  Fun0        = 1
  sizeTerm (Fun1 t)     = 1 + sizeTerm t
  sizeTerm (Fun2 t s)   = 1 + sizeTerm t + sizeTerm s
  sizeTerm (Fun3 t s r) = 1 + sizeTerm t + sizeTerm s + sizeTerm r

  --
  newtype TermDag = TermDag { unTermDag :: (Graph,Vertex) } deriving (Show)
  data IsoTermDag = IsoTermDag { term :: Term, termDag :: TermDag, iso :: Permute } deriving (Show)

  sizeIsoTermDag = (+) 1 . snd . bounds . fst . unTermDag . termDag

  instance Arbitrary IsoTermDag where
    arbitrary = do
      t <- arbitrary :: Gen Term
      arbitraryIso t

  arbitraryIso :: Term -> Gen IsoTermDag
  arbitraryIso t = do
    let tdag = toTermDag t
    let (_,m) = bounds $ fst $ unTermDag tdag
    l <- shuffle [nvars .. m]
    let p = listPermute (m + 1) ([0 .. nvars - 1] ++ l)
    return $ IsoTermDag { term = t, termDag = tdag, iso = p }

  applyIso :: IsoTermDag -> TermDag
  applyIso phi = TermDag (g',v') where
    (g,v) = unTermDag $ termDag phi
    f = at $ iso phi
    idx = map f (indices g)
    elm = map (map f) (elems g)
    g' = array (bounds g) (zip idx elm)
    v' = f v

  --
  data TGState = TGState {
      dict :: Map Term Vertex,
      grph :: [[Vertex]]
    } deriving (Show)

  initTGState :: TGState
  initTGState = TGState {
      dict = fromList [ (Var $ fromInt i, i) | i <- [0 .. nvars - 1] ],
      grph = replicate nvars []
    }

  lookupTermM :: Term -> State TGState (Maybe Vertex)
  lookupTermM t = state $ \s -> (lookup t (dict s), s)

  insertTermM :: Term -> [Vertex] -> State TGState Vertex
  insertTermM t vs = state $ \s -> let
    v = size $ dict s
    dict' = insert t v (dict s)
    grph' = vs:(grph s)
    in (v, TGState { dict = dict', grph = grph' })

  --
  toArrayM :: Term -> State TGState Vertex
  toArrayM t = do
    m <- lookupTermM t
    case m of
      Just v -> return v
      Nothing -> case t of
        Fun3 s r q -> do { v <- toArrayM s ; u <- toArrayM r ; w <- toArrayM q ; insertTermM t [v,u,w] }
        Fun2 s r   -> do { v <- toArrayM s ; u <- toArrayM r ;                   insertTermM t [v,u] }
        Fun1 s     -> do { v <- toArrayM s ;                                     insertTermM t [v] }
        _          ->                                                            insertTermM t []

  toTermDag :: Term -> TermDag
  toTermDag t = TermDag (g,v) where
    (v,s) = runState (toArrayM t) initTGState
    g = listArray (0, length (grph s) - 1) (reverse $ grph s)

  --
  toTermM :: Graph -> Vertex -> Maybe Term
  toTermM g v = let
    ms = map (toTermM g) (g ! v)
    in if any isNothing ms then Nothing else let
      ts = map fromJust ms
      in case (length ts) of
        0 ->                     Just $ if v < nvars then Var $ fromInt v else Fun0
        1 -> let [t]     = ts in Just $ Fun1 t
        2 -> let [t,s]   = ts in Just $ Fun2 t s
        3 -> let [t,s,r] = ts in Just $ Fun3 t s r
        _ -> Nothing

  toTerm :: TermDag -> Term
  toTerm tdag = fromJust $ toTermM g v where (g,v) = unTermDag tdag

  --
  prop_Read_Show_Term t = label l p where
    l = "size = " ++ show (sizeTerm t)
    p = t == (read . show) t

  prop_toTerm_isaRetractOf_toTermDag t = label l p where
    l = "size = " ++ show (sizeTerm t)
    p = t == (toTerm . toTermDag) t

  prop_IsoTermDag phi = label l p where
    l = "size = " ++ show (sizeIsoTermDag phi)
    p = (toTerm $ termDag phi) == (toTerm $ applyIso phi)

  prop_TermDag_dff t = label l p where
    l = "size = " ++ show (sizeTerm t)
    p = elem v (map root $ dff g) where
      (g,v) = unTermDag $ toTermDag t
      root (Node r _) = r

  return []

  --
  main :: IO ()
  main = getArgs >>= parse >>= mainLoop

  parse ["-t"] = $forAllProperties (quickCheckWithResult $ stdArgs { maxSuccess = 10000 }) >> exitWith ExitSuccess
  parse [str]  = readTerm str >> exitWith ExitSuccess
  parse [v,e]  = return (read v, read e)
  parse []     = return (8,16)
  parse _      = getProgName >>= usage >> exitWith ExitSuccess

  readTerm str = do
    phi <- generate $ arbitraryIso $ read str
    putTermDag $ applyIso phi

  mainLoop (nV,nE) = do
    phi <- generate (arbitrary :: Gen IsoTermDag)
    let t = term phi
    let (g,v) = unTermDag $ termDag phi
    let dV = abs $ nV - length (vertices g)
    let dE = abs $ nE - length (edges g)
    if dV < 2 && dE < 4
      then do
        putStrLn $ "; " ++ show t
        putTermDag $ applyIso phi
      else
        mainLoop (nV,nE)

  putTermDag tdag = do
    let (g,v) = unTermDag tdag
    putStrLn $ "; " ++ show v
    putStr $ foldr (++) "" [ show i ++ " -> " ++ show j ++ "\n" | (i,j) <- edges g ]

  usage progName = putStrLn $ "Usage: " ++ progName ++ " [-t | term | #vertices #edges]"
