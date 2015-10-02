import Control.Monad
import Control.Monad.State
import Data.Maybe

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm ExpList

type Id = String
type Number = Int

data ExpList = PairExpList Exp ExpList
             | LastExpList Exp

data Exp = IdExp Id
         | NumExp Number
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp

data BinOp = Plus
           | Minus
           | Times
           | Div


a :: Exp
a = IdExp "a"

b :: Exp
b = IdExp "b"

one :: Exp
one = NumExp 1

three :: Exp
three = NumExp 3

five :: Exp
five = NumExp 5

ten :: Exp
ten = NumExp 10

printb :: Stm
printb = PrintStm $ LastExpList b

printa :: Stm
printa = PrintStm $ PairExpList a $ LastExpList $ OpExp a Minus one

assigna :: Stm
assigna = AssignStm "a" $ OpExp five Plus three

assignb :: Stm
assignb = AssignStm "b" $ EseqExp printa $ OpExp ten Times a

prog :: Stm
prog = CompoundStm assigna $ CompoundStm assignb printb

-- Part 1
maxargsExp :: Exp -> Int
maxargsExp (OpExp left _ right) = max (maxargsExp left) (maxargsExp right)
maxargsExp (EseqExp stm exp) = max (maxargs stm) (maxargsExp exp)
maxargsExp _ = 0

maxargsExpList :: ExpList -> Int
maxargsExpList (PairExpList exp list) = max (maxargsExp exp) (maxargsExpList list)
maxargsExpList (LastExpList exp) = maxargsExp exp

lengthExpList :: ExpList -> Int
lengthExpList (PairExpList _ rest) = 1 + lengthExpList rest
lengthExpList (LastExpList _) = 1

maxargs :: Stm -> Int
maxargs (CompoundStm s1 s2) = max (maxargs s1) (maxargs s2)
maxargs (AssignStm _ exp) = maxargsExp exp
maxargs (PrintStm exps) = max (lengthExpList exps) (maxargsExpList exps)

-- Part 2
type Bindings = [(Id, Number)]
interpExp :: Exp -> StateT Bindings IO Number
interpExp (IdExp id) = liftM fromJust $ gets (lookup id)
interpExp (NumExp n) = return n
interpExp (OpExp left op right) =
  do l <- interpExp left
     r <- interpExp right
     return $ apply op l r
  where apply Plus = (+)
        apply Minus = (-)
        apply Times = (*)
        apply Div = div
interpExp (EseqExp stm exp) = do interp stm; interpExp exp

printExp :: Exp -> StateT Bindings IO ()
printExp exp = do v <- interpExp exp; lift $ putStr $ show v ++ " "

printExpList :: ExpList -> StateT Bindings IO ()
printExpList (PairExpList exp list) = do printExp exp; printExpList list
printExpList (LastExpList exp) = do printExp exp; lift $ putStr "\n"

interp :: Stm -> StateT Bindings IO ()
interp (CompoundStm s1 s2) = do interp s1; interp s2
interp (AssignStm id exp) = do v <- interpExp exp; modify ((id, v) :)
interp (PrintStm exps) = printExpList exps
