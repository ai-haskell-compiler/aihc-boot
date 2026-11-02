-- Data types, newtypes, Maybe, Either and type synonyms.
import Data.Either (either, lefts, rights)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isJust)

data Color = Red | Green | Blue

data Expr
  = Num Int
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr

newtype Wrapper = Wrap Int

type Env = [(Int, Int)]

data Pair a b = Pair a b

infixl 6 :+:
data Complex = Int :+: Int

colorName :: Color -> String
colorName Red = "red"
colorName Green = "green"
colorName Blue = "blue"

next :: Color -> Color
next Red = Green
next Green = Blue
next Blue = Red

eval :: Expr -> Int
eval (Num n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Neg a) = negate (eval a)

render :: Expr -> String
render (Num n) = show n
render (Add a b) = "(" ++ render a ++ " + " ++ render b ++ ")"
render (Mul a b) = render a ++ " * " ++ render b
render (Neg a) = "-" ++ render a

unwrap :: Wrapper -> Int
unwrap (Wrap n) = n

safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "division by zero"
safeDiv a b = Right (a `div` b)

lookupEnv :: Env -> Int -> Maybe Int
lookupEnv env k = lookup k env

pairSum :: Pair Int Int -> Int
pairSum (Pair a b) = a + b

re :: Complex -> Int
re (a :+: _) = a

main :: IO ()
main = do
  putStrLn (unwords (map colorName [Red, next Red, next (next Red), next Blue]))
  let e = Add (Num 2) (Mul (Num 3) (Neg (Num 4)))
  putStrLn (render e ++ " = " ++ show (eval e))
  print (unwrap (Wrap 41) + 1)
  print (map unwrap [Wrap 1, Wrap 2] !! 1)
  putStrLn (either id show (safeDiv 10 2))
  putStrLn (either id show (safeDiv 10 0))
  print (length (lefts [safeDiv 1 0, safeDiv 4 2, safeDiv 2 0]))
  print (sum (rights [safeDiv 1 0, safeDiv 4 2, safeDiv 9 3]))
  let env = [(1, 10), (2, 20)]
  print (fromMaybe 0 (lookupEnv env 2) + fromMaybe 0 (lookupEnv env 3))
  print (if isJust (lookupEnv env 1) then 1 else 0)
  print (sum (catMaybes [Just 1, Nothing, Just 2]))
  print (sum (mapMaybe (lookupEnv env) [1, 2, 3]))
  print (pairSum (Pair 3 4))
  print (re (5 :+: 6))
  print (case Pair Red (Wrap 7) of Pair Red w -> unwrap w; Pair _ _ -> 0)
  print (sum (map (\c -> case c of Red -> 1; Green -> 10; Blue -> 100) [Red, Blue, Blue]))
