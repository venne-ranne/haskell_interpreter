Initial code for COMP304 Assignment 3, 2017.

Author: Lindsay Groves, VUW, 2017.

This is an interpreter for a simple while program language, as presented in
lectures.  The assignment asks you to make several extensions to the language.

You run a program using the run function, which takes a program and an
initial store and returns the store resulting from executing the program, if
it executes successfully.

There are some example programs and example stores at the end of this file, so
you can try running some simple tests, e.g. run p1 s1 runs program p1 with
store s1 (which doesn't do much!).


Map is used to implement the store.

> import Map

Variable names are assumed to be single characters.

> type Var = Char

Value are assumed to be integers.

> type Val = Integer

A program is just a list of statements

> type Prog = [Stmt]

A statement can be a skip, assignment, if or do statement.

> data Stmt = Skip | Asgn Var Exp | If Cond Prog Prog | 
>             Do Cond Prog
>             deriving (Show)

An expression can be a constant, a variable, or a binary operator applied to
two expressions

> data Exp = Const Val | Var Var | Bin Op Exp Exp
>            deriving (Show)

An operation is +, -, *, / or ^

> data Op = Plus | Minus | Times | Div | Power
>           deriving (Eq, Show)

A condition is a relational operator applied to two expresions

> data Cond = Cond RelOp Exp Exp
>             deriving (Show)

A relational operation is =, /=, <, <=, > or >=

> data RelOp = Eq | Ne | Lt | Le | Gt | Ge
>              deriving (Eq, Show)

A store is a map from variables to values

> type Store = Map Var Val

To run a program with a given initial store, we just pass the program and
store to exec.

> run :: Prog -> Store -> Store

> run prog input = exec prog input

To execute a program, we just execute each statement in turn, passing the
resulting state to the next statement at each step.

> exec :: Prog -> Store -> Store

> exec [] store = store

> exec (stmt : rest) store = exec rest (exec' stmt store)

Execute a single statement, according to its semantics

> exec' :: Stmt -> Store -> Store

> exec' Skip store = store

> exec' (Asgn var exp) store =
>       setVal var (eval exp store) store

> exec' (If cond thenPart elsePart) store = 
>       if evalc cond store
>       then exec thenPart store
>       else exec elsePart store

> exec' (Do cond body) store =
>       if not (evalc cond store) then store
>       else exec' (Do cond body) (exec body store)

Evaluate an expression, according to its type

> eval :: Exp -> Store -> Val

> eval (Const n) _ = n

> eval (Var v) s = getVal v s

> eval (Bin op x y) s = apply op (eval x s) (eval y s)

Apply an arithmetic operator

> apply :: Op -> Val -> Val -> Val
> apply Plus x y = x + y
> apply Minus x y = x - y
> apply Times x y = x * y
> apply Div x y = x `div` y

> evalc :: Cond -> Store -> Bool
> evalc (Cond rel x y) s = applyc rel (eval x s) (eval y s)

applyc is similar to apply, but applies relations:

> applyc :: RelOp -> Val -> Val -> Bool
> applyc Eq x y = x == y
> applyc Ne x y = x /= y
> applyc Lt x y = x < y
> applyc Le x y = x <= y
> applyc Gt x y = x > y
> applyc Ge x y = x >= y

Some sample expressions

> e0 = Const 0
> e1 = Const 1
> e2 = Const 2
> e3 = Var 'x'
> e4 = Bin Plus e3 e1
> e5 = Bin Plus (Var 'i') e1

Some sample stores

> s1 = []
> s2 = [('x',1)]
> s3 = [('x',1), ('y',2)]

Some sample programs

> p1 = [Skip]
> p2 = [Skip, Skip]
> p3 = [Asgn 'x' e1]
> p4 = [Asgn 'x' (Var 'x')]
> p5 = [Asgn 'x' (Var 'y')]
> p6 = [Asgn 'x' (Bin Plus (Var 'x') (Const 1))]
> p7 = [Asgn 'x' e1, Asgn 'y' e2,
>	(If (Cond Eq (Var 'x') (Var 'y')) [Asgn 'z' e1] [Asgn 'z' e2])]
> p8 = [Asgn 'i' e1, Asgn 's' e0,
>	(Do (Cond Lt (Var 'i') (Var 'n')) 
>	    [Asgn 's' (Bin Plus (Var 's') e1), Asgn 'i' e5])]
