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

> module While1 where

Map is used to implement the store.

> import Map

Variable names are assumed to be single characters.

> type Var = Char

Value are assumed to be integers or booleans.

> data Val = Int Int | Bool Bool
>             deriving (Show, Eq)

A program is consist of a list of variable declarations and a list of statements

> type Prog = ([Var], [Stmt])

A statement can be a skip, assignment, if or do statement.

> data Stmt = Skip | Asgn Var Exp | If Exp Prog Prog |
>             Do Exp Prog
>             deriving (Show)

An expression can be a constant, a variable, a binary operator,
or a logical operator applied to two expressions

> data Exp = Const Val | Var Var | Bin Op Exp Exp | LogNot Op Exp
>            deriving (Show)

An operation is +, -, *, / , ^, =, /=, <, <=, >, >=, &&, || or not

> data Op = Plus | Minus | Times | Div | Power | Eq | Ne | Lt | Le | Gt | Ge | And | Or | Not
>           deriving (Eq, Show)

A store is a map from variables to values

> type Store = Map Var Val

To run a program with a given initial store, we just pass the program and
store to exec.

> run :: Prog -> Store -> Store

> run prog store
>    | varsOk prog = exec prog store
>    | otherwise = error ("Undeclared variables.")

Check statically that all variables used are declared before executing the program.
If none, returns True, otherwise False.

> varsOk :: Prog -> Bool
> varsOk (_, []) = True
> varsOk (vars, stmt : stmts) =
>     varsOk' (vars, stmt) && varsOk (vars, stmts)

> varsOk' :: ([Var], Stmt) -> Bool
> varsOk' (vars, Skip) = True
> varsOk' (vars, Asgn v e) = elem v vars && varsOk'' (vars, e)
> varsOk' (vars, If e prog1 prog2) = varsOk'' (vars, e) && varsOk prog1 && varsOk prog2
> varsOk' (vars, Do e prog) = varsOk'' (vars, e) && varsOk prog

> varsOk'' :: ([Var], Exp) -> Bool
> varsOk'' (vars, (Var v))
>    | elem v vars = True
>    | otherwise = error ("Undeclared variable: " ++ [v])
> varsOk'' (vars, (Const c)) = True
> varsOk'' (vars, (Bin op x y)) = varsOk'' (vars, x) && varsOk'' (vars, y)
> varsOk'' (vars, (LogNot op x)) = varsOk'' (vars, x)

To execute a program, we just execute each statement in turn, passing the
resulting state to the next statement at each step.

> exec :: Prog -> Store -> Store
> exec (vars, []) store = store
> exec (vars, (stmt : rest)) store = exec (vars, rest) (exec' stmt store)

Execute a single statement, according to its semantics

> exec' :: Stmt -> Store -> Store

> exec' Skip store = store

> exec' (Asgn var exp) store =
>       setVal var (eval exp store) store

> exec' (If cond thenPart elsePart) store =
>       if b
>       then exec thenPart store
>       else exec elsePart store
>       where (Bool b) = eval cond store

> exec' (Do cond body) store =
>       if not b then store
>       else exec' (Do cond body) (exec body store)
>       where (Bool b) = eval cond store

Evaluate an expression, according to its type

> eval :: Exp -> Store -> Val
> eval (Const n) _ = n
> eval (Bin op x y) s = apply op (eval x s) (eval y s)
> eval (LogNot op x) s = applyNot op (eval x s)
> eval (Var v) s
>    | hasKey v s = getVal v s
>    | otherwise = error ("Undefined variable" ++ [v])

Apply an arithmetic, relational, or logical operator

> apply :: Op -> Val -> Val -> Val
> apply Plus (Int x) (Int y) = Int (x + y)
> apply Minus (Int x) (Int y) = Int (x - y)
> apply Times (Int x) (Int y) = Int (x * y)
> apply Div (Int x) (Int y) = Int (x `div` y)
> apply Eq (Int x) (Int y) = Bool (x == y)
> apply Ne (Int x) (Int y) = Bool (x /= y)
> apply Lt (Int x) (Int y) = Bool (x < y)
> apply Le (Int x) (Int y) = Bool (x <= y)
> apply Gt (Int x) (Int y) = Bool (x > y)
> apply Ge (Int x) (Int y) = Bool (x >= y)
> apply And (Bool x) (Bool y) = Bool (x && y)
> apply Or (Bool x) (Bool y) = Bool (x || y)
> apply op x y = error("Illegal operator: " ++(show op))

Apply a "not" logical operator

> applyNot :: Op -> Val -> Val
> applyNot Not (Bool x) = Bool (not x)
> applyNot op x = error("Illegal operator: " ++(show op))

Some sample expressions

> e0 = Const (Int 0)
> e1 = Const (Int 1)
> e2 = Const (Int 2)
> e3 = Var 'x'
> e4 = Bin Plus e3 e1
> e5 = Bin Plus (Var 'i') e1
> e6 = Const (Bool True)
> e7 = Const (Bool False)
> e8 = Bin Or e6 e7

Some sample stores

> s1 = []
> s2 = [('x', Int 1)]
> s3 = [('x', Int 1), ('y', Int 2)]
> s4 = [('x',Bool True), ('y', Int 2)]

Some sample programs

> p1 = (['x'], [Skip])
> p2 = (['x', 'y'], [Skip, Skip])
> p3 = (['x'], [Asgn 'x' e1])
> p4 = (['x'], [Asgn 'x' (Var 'x')])
> p5 = (['x', 'y'], [Asgn 'x' (Var 'y')])
> p6 = (['x', 'y'], [Asgn 'x' (Bin Plus (Var 'x') (Const (Int 1)))])
> p7 = (['x', 'y', 'z'], [Asgn 'x' e1, Asgn 'y' e2,
>   (If (Bin Eq (Var 'x') (Var 'y')) (['z'], [Asgn 'z' e1]) (['z'], [Asgn 'z' e2]))])
> p8 = (['i', 's'],
>        [Asgn 'i' e1, Asgn 's' e0,
>         (Do (Bin Lt (Var 'i') (Var 'n')) (['s', 'i'], [Asgn 's' (Bin Plus (Var 's') e1), Asgn 'i' e5]))])

> test1 = if checkResult (run prog store) output
>        then "Test 1: passed"
>        else "### Failure in: Test 1: variable test"
>        where prog = p1
>              store = s2
>              output = s2

> test2 = if checkResult (run prog store) output
>        then "Test 2: passed"
>        else "### Failure in: Test 2: variable test"
>        where prog = (['y'], [Asgn 'y' e1])
>              store = [('x', Int 1), ('y', Int 2)]
>              output = [('x', Int 1), ('y', Int 1)]

> test3 = if checkResult (run prog store) output
>        then "Test 3: passed"
>        else "### Failure in: Test 3: variable test"
>        where prog = (['x'], [Asgn 'x' e7])
>              store = [('x', Bool True), ('y', Bool True)]
>              output = [('x', Bool False), ('y', Bool True)]

> test4 = if checkResult (run prog store) output
>        then "Test 4: passed"
>        else "### Failure in: Test 4: binary operator test"
>        where prog = (['y'], [Asgn 'y' (Bin Plus e2 e1)])
>              store = [('x', Int 1), ('y', Int 2)]
>              output = [('x', Int 1), ('y', Int 3)]

> test5 = if checkResult (run prog store) output
>        then "Test 5: passed"
>        else "### Failure in: Test 5: assignment variable test"
>        where prog = (['x', 'y'], [Asgn 'x' (Var 'y')])
>              store = [('x', Int 1), ('y', Int 2)]
>              output = [('x', Int 2), ('y', Int 2)]

> test6 = run prog store
>        where prog = (['x'], [Asgn 'x' (Var 'y')])
>              store = [('x', Int 1), ('y', Int 2)]


> getAllTests = test1 ++"\n"++ test2 ++"\n"++ test3 ++ "\n"++
>               test4 ++"\n"++ test5

> tests = putStrLn getAllTests

> printTests :: IO ()
> printTests = tests

> checkResult :: Store -> Store -> Bool
> checkResult [] _ = True
> checkResult (x:xs) store
>    | contains x store = checkResult xs store
>    | otherwise = False

> contains :: (Var, Val) -> Store -> Bool
> contains exp [] = False
> contains exp (x: xs)
>    | exp == x = True
>    | otherwise = contains exp xs


To check if a store contains uninitialised variable or not.

 initVars :: [Declaration] -> [Stmt] -> Store -> Bool
 initVars [] _ = True
 initVars ((name, ty):rest) store
     | hasKey name store = initVars rest store
     | otherwise = error ("Variable uninitialised: " ++ show name)

To check if a list of statements contains an assignment statement for variable given.

 isVarInit :: Var -> [Stmt] -> Bool
