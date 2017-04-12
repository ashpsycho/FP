{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import ExprT as T
import Parser
import StackVM as S
import Data.Functor

eval :: ExprT -> Integer
eval (Lit x) = x
eval (T.Mul a b) = (eval a)*(eval b)
eval (T.Add a b) = (eval a)+(eval b)

evalStr :: String -> Maybe Integer
evalStr s= eval <$> (parseExp Lit T.Add T.Mul s) 

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
	lit :: Integer -> a
	mul :: a -> a -> a
	add :: a -> a -> a

instance Expr ExprT where
	lit = Lit
	add = T.Add
	mul = T.Mul

instance Expr Integer where
	lit x = x
	add a b = (a+b)
	mul a b = (a*b)

instance Expr Bool where
	lit x = (x>0)
	add a b  = a||b
	mul a b  = a&&b

instance Expr MinMax where
	lit x = MinMax x
	add (MinMax a) (MinMax b) = if ( a > b) then MinMax a else MinMax b
	mul (MinMax a) (MinMax b) = if ( a > b) then MinMax a else MinMax b
 
instance Expr Mod7 where
	lit x = Mod7 x
	add (Mod7 a) (Mod7 b) = Mod7 (7+((a+b) `mod` 7) `mod` 7)
	mul (Mod7 a) (Mod7 b) = Mod7 (7+((a*b) `mod` 7) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr Program where
	lit x = [PushI x] 
	add pA pB = pA ++ pB ++ [S.Add]
	mul pA pB = pA ++ pB ++ [S.Mul]

compile:: String -> Maybe Program
compile = parseExp lit add mul