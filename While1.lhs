COMP304 Assignment 3, 2017

Initial code Author: Lindsay Groves, VUW, 2017.
Modified by: Vivienne Yapp.

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

Value are assumed to be integers.

> data Type = IntT | BoolT
>             deriving (Show, Eq)

Variable names are assumed to be single characters.

> type Var = Char

Value are assumed to be integers or booleans.

> data Val = Int Int | Bool Bool
>             deriving (Show, Eq)

A program is consist of a list of variable declarations and a list of statements

> type Prog = ([Declaration], [Stmt])

A declared variable consist of a name and a type.

> type Declaration = (Var, Type)

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

> run (vars, stmts) store
>    | varsOk prog && noRepeated = exec prog store
>    | otherwise = error ("Undeclared variables.")
>    where prog = (vars, stmts)
>          noRepeated = noRepeatVars vars []

Added function: To check if the list of declarations contains duplicated names.

> noRepeatVars :: [(Var, Type)] -> [Var] -> Bool
> noRepeatVars [] _ = True
> noRepeatVars ((v, ty):rest) names
>     | elem v names = error ("Duplicated variable name: "++[v])
>     | otherwise = noRepeatVars rest (names++[v])

Added function: Check statically that all variables used are declared before
executing the program. If none, returns True, otherwise False.

> varsOk :: Prog -> Bool
> varsOk (_, []) = True
> varsOk (vars, stmt : stmts) =
>     varsOk' (vars, stmt) && varsOk (vars, stmts)

> varsOk' :: ([(Var, Type)], Stmt) -> Bool
> varsOk' (vars, Skip) = True
> varsOk' (vars, Asgn v e) = elems v vars && varsOk'' (vars, e)
> varsOk' (vars, If e prog1 prog2) = varsOk'' (vars, e) && varsOk prog1 && varsOk prog2
> varsOk' (vars, Do e prog) = varsOk'' (vars, e) && varsOk prog

> elems :: Var -> [(Var, Type)] -> Bool
> elems var [] = False
> elems var ((v, ty):xs) | var == v = True
>                        | otherwise = elems var xs

> varsOk'' :: ([(Var, Type)], Exp) -> Bool
> varsOk'' (vars, (Var v))
>    | elems v vars = True
>    | otherwise = error ("Undeclared variable: " ++ [v])
> varsOk'' (vars, (Const c)) = True
> varsOk'' (vars, (Bin op x y)) = varsOk'' (vars, x) && varsOk'' (vars, y)
> varsOk'' (vars, (LogNot op x)) = varsOk'' (vars, x)

To execute a program, we just execute each statement in turn, passing the
resulting state to the next statement at each step.

> exec :: Prog -> Store -> Store
> exec (vars, []) store = store
> exec (vars, (stmt : rest)) store = exec (vars, rest) (exec' vars stmt store)

Execute a single statement, according to its semantics

> exec' :: [Declaration] -> Stmt -> Store -> Store

> exec' _ Skip store = store

> exec' vars (Asgn var exp) store
>     | assignTypeOk newV declaredV = setVal var newV store
>     | otherwise = error msg
>     where newV = (eval exp store)
>           declaredV = getDeclaredVar var vars
>           typeV = snd declaredV
>           msg = "Variable assignment error: Couldn't match value '" ++ show newV ++ "' with type '" ++ show typeV++"'"

> exec' _ (If cond thenPart elsePart) store =
>       if b
>       then exec thenPart store
>       else exec elsePart store
>       where (Bool b) = eval cond store

> exec' vars (Do cond body) store =
>       if not b then store
>       else exec' vars (Do cond body) (exec body store)
>       where (Bool b) = eval cond store

Evaluate an expression, according to its type

> eval :: Exp -> Store -> Val
> eval (Const n) _ = n
> eval (Bin op x y) s = apply op (eval x s) (eval y s)
> eval (LogNot op x) s = applyNot op (eval x s)
> eval (Var v) s
>    | hasKey v s = getVal v s
>    | otherwise = error ("Uninitialised variable: " ++ [v])

Added function: To get declared variable from a list of variable declarations.

> getDeclaredVar :: Var -> [Declaration] -> Declaration
> getDeclaredVar var [] = error ("Undeclared variable: " ++ [var])
> getDeclaredVar var ((v, ty):xs)
>    | var == v = (v, ty)
>    | otherwise = getDeclaredVar var xs

Added function: To check if the type of a value match the type of the declared variable.

> assignTypeOk :: Val -> Declaration -> Bool
> assignTypeOk (Int _) (_, IntT) = True
> assignTypeOk (Bool _) (_, BoolT) = True
> assignTypeOk _ _ = False

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

Added function: Apply a "not" logical operator

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
> s4 = [('x', Bool True), ('y', Int 2)]

Some sample programs

> p1 = ([('x', IntT)], [Skip])
> p2 = ([('x', IntT), ('y', IntT)], [Skip, Skip])
> p3 = ([('x', IntT)], [Asgn 'x' e1])
> p4 = ([('x', IntT)], [Asgn 'x' (Var 'x')])
> p5 = ([('x', IntT), ('y', IntT)], [Asgn 'x' (Var 'y')])
> p6 = ([('x', IntT), ('y', IntT)], [Asgn 'x' (Bin Plus (Var 'x') (Const (Int 1)))])
> p7 = ([('x', IntT), ('y', IntT), ('z', IntT)], [Asgn 'x' e1, Asgn 'y' e2,
>   (If (Bin Eq (Var 'x') (Var 'y')) ([('z', IntT)], [Asgn 'z' e1]) ([('z', IntT)], [Asgn 'z' e2]))])
> p8 = ([('i', IntT), ('s', IntT)],
>        [Asgn 'i' e1, Asgn 's' e0,
>         (Do (Bin Lt (Var 'i') (Var 'n')) ([('s', IntT), ('i', IntT)], [Asgn 's' (Bin Plus (Var 's') e1), Asgn 'i' e5]))])


The following is some examples of error-reporting when illegal action is performed.

Error 1: When try to assign a boolean type const to an integer type variable.

< > *While1> run ([('y', IntT)],[Asgn 'y' (Const (Bool True))]) [('y', Int 90)]
< > *** Exception: Variable assignment error: Couldn't match value 'Bool True' with type 'IntT'
< > CallStack (from HasCallStack):
< >   error, called at While1.lhs:126:21 in main:While1

Error 2: When assign an undeclared variable to a variable

< > *While1> run ([('y', IntT)],[Asgn 'y' (Var 'x')]) [('y', Int 90)]
< > *** Exception: Undeclared variable: x
< > CallStack (from HasCallStack):
< >   error, called at While1.lhs:106:20 in main:While1

Error 3: When assign an uninitialised variable to other variable

< > *While1> run ([('x', IntT), ('y', IntT)],[Asgn 'x' (Var 'y')]) [('x', Int 90)]
< > *** Exception: Uninitialised variable: y
< > CallStack (from HasCallStack):
< >   error, called at While1.lhs:151:20 in main:While1

Error 4: Duplicated variable names

< > *While1> run ([('x', IntT), ('x', IntT), ('y', IntT)],[Asgn 'x' (Var 'y')]) [('x', Int 90)]
< > *** Exception: Duplicated variable name: x
< > CallStack (from HasCallStack):
< >   error, called at While1.lhs:79:24 in main:While1


TESTING CASES: To run all the tests, type in "printTests".

> test1 = if checkResult (run prog store) output
>        then "Test 1: passed - Assign a value (Int type) to a variable."
>        else "### Failure in: Test 1: ssign a value (Int type) to a variable"
>        where prog = ([('y', IntT)], [Asgn 'y' e1])
>              store = [('x', Int 1), ('y', Int 2)]
>              output = [('x', Int 1), ('y', Int 1)]

> test2 = if checkResult (run prog store) output
>        then "Test 2: passed - Assign a value (Bool type) to a variable."
>        else "### Failure in: Test 2: passed - Assign a value (Int type) to a variable."
>        where prog = ([('x', BoolT)], [Asgn 'x' (Const (Bool False))])
>              store = [('y', Bool True)]
>              output = [('x', Bool False), ('y', Bool True)]

> test3 = if checkResult (run prog store) output
>        then "Test 3: passed - Assign a value (adding two integers) into a variable."
>        else "### Failure in: Test 3: Assign a value (adding two integers) into a variable."
>        where prog = ([('y', IntT)], [Asgn 'y' (Bin Plus e2 e1)])
>              store = [('x', Int 1), ('y', Int 2)]
>              output = [('x', Int 1), ('y', Int 3)]

> test4 = if checkResult (run prog store) output
>        then "Test 4: passed - Do expression to increment 'x' till x > y is false."
>        else "### Failure in: Test 4: Do expression to increment 'x' till x > y is false."
>        where prog = ([('x', IntT), ('y', IntT)], [Do (Bin Gt (Var 'x') (Var 'y')) ([('x', IntT), ('y', IntT)], [Asgn 'x' (Bin Minus (Var 'x') (e1)), Asgn 'y' (Const (Int 188))])])
>              store = [('x', Int 1000), ('y', Int 500)]
>              output = [('x', Int 188), ('y', Int 188)]

> test5 = if checkResult (run prog store) output
>        then "Test 5: passed - OR operator in IF statement"
>        else "### Failure in: Test 5: OR operator in IF statement"
>        where prog = ([('x', BoolT), ('y', BoolT)], [If (Bin Or (Var 'x') (Var 'y')) ([('x', BoolT)], [Asgn 'x' (Const (Bool False))]) p1])
>              store = [('x', Bool True), ('y', Bool False)]
>              output = [('x', Bool False), ('y', Bool False)]

> test6 = if checkResult (run prog store) output
>        then "Test 6: passed - Equal operator expression in an IF statement"
>        else "### Failure in: Test 6: Equal operator expression (check if two variables are equal) in an IF statement"
>        where prog = ([('x', IntT), ('y', IntT)], [If (Bin Eq (Var 'x') (Var 'y')) ([('x', IntT)], [Asgn 'x' e0]) p1])
>              store = [('x', Int 1), ('y', Int 1)]
>              output = [('x', Int 0), ('y', Int 1)]

> getAllTests = test1 ++"\n"++ test2 ++"\n"++ test3 ++ "\n"++
>               test4 ++"\n"++ test5 ++"\n"++ test6

> tests = putStrLn getAllTests

> printTests :: IO ()
> printTests = tests

The following functions are for testing purpose only. To check if an output matches with expected values.

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


DISCUSSION:

Part 1: Adding Boolean and Declarations

I done everything on Part 1. To extend the language to allow Boolean variable,
what I did first is I changed the "type Val" to "data Val" so that the program
store Boolean and Integer values. Maintain the distinction with a union type
using a tag, e.g. data Val = Int Int | Bool Bool. To represent an integer, we write Int 99.
As for the declarations, I modified the "type prog" so it also included a list declarations
and a list of statements, e.g. ([Declaration], [Stmt]) ([(Var 'x', IntT)], [Asgn 'x' (Const (Int 88))]).

Other modification included combined the apply and applyc function together to ensure
that operations are applied to values of the correct type, and returns a value of
appropriate type. To extend the OR, AND and NOT operators, I extended the "data Op"
and since NOT operator is not :: Bool -> Bool, I have to add a new expression (LogNot Op Exp)
which only take one expression.

For the error conditions, I chose to do static checking on the variable declarations and
duplicated variables. Because it is easier to maintain and implement, as I don't have
to pass the list of variable names as an extra argument to exec, exec' and eval functions.
The runtime error checking included uninitialised variable, assignment variable (data type
do not match). To detect an uninitialised variable, I chose to do the runtime checking as
I only have to raise an error in the eval function. We could do it statically too, but we
will have to pass an extra argument (store) to VarsOK function and execute the hasKey
function in Map in order to do the checking.

The test cases are mostly checking on the evaluation of the expressions and also check
whether the functions above return the correct values. If the test is passed, it will
prints a passed test message. Otherwise it will print an error message by stating
which function that not passed the tests.
