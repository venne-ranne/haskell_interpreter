varOkCOMP304 Assignment 3, 2017.

Initial code from Author: Lindsay Groves, VUW, 2017.
Author: Vivienne Yapp

This is an interpreter for a simple while program language, as presented in
lectures.  The assignment asks you to make several extensions to the language.

You run a program using the run function, which takes a program and an
initial store and returns the store resulting from executing the program, if
it executes successfully.

There are some example programs and example stores at the end of this file, so
you can try running some simple tests, e.g. run p1 s1 runs program p1 with
store s1 (which doesn't do much!).

> module While2 where

Map is used to implement the store.

> import Map

An arrray type consist the element type and the size.

> type AInfo = (Type, Int)

Value are assumed to be integers.

> data Type = IntT | BoolT | ArrayT AInfo
>             deriving (Show, Eq)

Variable names are assumed to be single characters.

> type Var = Char

Value are assumed to be integers or booleans.

> data Val = Int Int | Bool Bool | Array [(Int, Val)]
>             deriving (Show, Eq)

A program is consist of a list of variable declarations and a list of statements

> type Prog = ([Declaration], [Stmt])

A declared variable consist of a name and a type.

> type Declaration = (Var, Type)

A statement can be a skip, assignment, if or do statement.

> data Stmt = Skip | Asgn Var Exp | If Exp Prog Prog |
>             Do Exp Prog | AsgnE Int Var Exp
>             deriving (Show)

An expression can be a constant, a variable, a binary operator,
or a logical operator applied to two expressions

> data Exp = Const Val | Var Var | Bin Op Exp Exp | LogNot Op Exp | Item Int Exp
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
>    | otherwise = error ("Variables uninitialised or undeclared.")
>    where prog = (vars, stmts)
>          noRepeated = noRepeatVars vars []

To check if a list of variable declarations contains duplicated name or not.
If none, returns True, otherwise raise an error.

> noRepeatVars :: [(Var, Type)] -> [Var] -> Bool
> noRepeatVars [] _ = True
> noRepeatVars ((v, ty):rest) names
>     | elem v names = error ("Duplicated variable name: "++[v])
>     | otherwise = noRepeatVars rest (names++[v])

Check statically that all variables used are declared before executing the program.
If none, returns True, otherwise False.

> varsOk :: Prog -> Bool
> varsOk (_, []) = True
> varsOk (vars, stmt : stmts) =
>     varsOk' (vars, stmt) && varsOk (vars, stmts)

> varsOk' :: ([(Var, Type)], Stmt) -> Bool
> varsOk' (vars, Skip) = True
> varsOk' (vars, Asgn v e) = varOk v vars && varsOk'' (vars, e)
> varsOk' (vars, If e prog1 prog2) = varsOk'' (vars, e) && varsOk prog1 && varsOk prog2
> varsOk' (vars, Do e prog) = varsOk'' (vars, e) && varsOk prog
> varsOk' (vars, AsgnE idx v e) = varOk v vars && varsOk'' (vars, e)

> varsOk'' :: ([(Var, Type)], Exp) -> Bool
> varsOk'' (vars, (Const c)) = True
> varsOk'' (vars, (Bin op x y)) = varsOk'' (vars, x) && varsOk'' (vars, y)
> varsOk'' (vars, (LogNot _ x)) = varsOk'' (vars, x)
> varsOk'' (vars, (Item _ x)) = varsOk'' (vars, x)
> varsOk'' (vars, (Var v))
>    | varOk v vars = True
>    | otherwise = error ("Undeclared variable: " ++ [v])

To check if a variable consist in a list of variable declarations.

> varOk :: Var -> [Declaration] -> Bool
> varOk var [] = error ("Undeclared variable: " ++ [var])
> varOk var ((v, ty):xs) | var == v = True
>                        | otherwise = varOk var xs

To get declared variable from a list of variable declarations.

> getDeclaredVar :: Var -> [Declaration] -> Declaration
> getDeclaredVar var [] = error ("Undeclared variable: " ++ [var])
> getDeclaredVar var ((v, ty):xs)
>    | var == v = (v, ty)
>    | otherwise = getDeclaredVar var xs

To check the type of an expression.

> expType :: Exp -> [Declaration] -> Type
> expType (Const (Int _)) _ = IntT
> expType (Const (Bool _)) _ = BoolT
> expType (LogNot op _ ) _ = BoolT
> expType (Bin op _ _) _
>     | elem op [Eq,Ne,Lt, Le, Gt, Ge, And, Or] = BoolT
>     | otherwise = IntT
> expType (Var v) vars
>     | (length filtered == 1) = findType
>     | otherwise = error ("Undeclared variable: "++[v])
>     where filtered = filter (\(name, ty) -> name == v) vars
>           findType = snd (head (filtered))

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
>     where newV = (eval exp store vars)
>           declaredV = getDeclaredVar var vars
>           typeV = snd declaredV
>           msg = "Variable assignment error: Couldn't match value '" ++ show newV ++ "' with type '" ++ show typeV++"'"

> exec' vars (If cond thenPart elsePart) store =
>       if b
>       then exec thenPart store
>       else exec elsePart store
>       where (Bool b) = eval cond store vars

> exec' vars (Do cond body) store =
>       if not b then store
>       else exec' vars (Do cond body) (exec body store)
>       where (Bool b) = eval cond store vars

> exec' vars (AsgnE idx var exp) store
>      | assignTypeOk newV declaredV && hasKey var store = setVal var newArray store
>      | otherwise = error ("Assignment to array '"++ show var++" at index "++show idx ++ " failed.")
>      where newV = (eval exp store vars)
>            declaredV = getDeclaredVar var vars
>            arr = getVal var store
>            newArray = setE idx newV arr

Replaces the element at the specified position in this array with the specified element.

> setE :: Int -> Val -> Val -> Val
> setE idx v (Int _) = error ("Cannot set an element into Int type.")
> setE idx v (Bool _) = error ("Cannot set an element into Bool type.")
> setE idx v (Array a) = setE' idx v a []

> setE' :: Int -> Val -> [(Int, Val)] -> [(Int, Val)] -> Val
> setE' idx v [] newArray = (Array newArray)
> setE' idx v ((i, val):xs) newArray
>        | idx == i = Array (newArray ++ xs ++ [(idx, v)])
>        | otherwise = setE' idx v xs (newArray++[(i, val)])

To check if the type of a value match the type of the declared variable.

> assignTypeOk :: Val -> Declaration -> Bool
> assignTypeOk (Int _) (_, IntT) = True
> assignTypeOk (Bool _) (_, BoolT) = True
> assignTypeOk (Int _) (_, (ArrayT (IntT, _))) = True
> assignTypeOk (Bool _) (_, (ArrayT (BoolT, _))) = True
> assignTypeOk (Array _) (_, (ArrayT _)) = True
> assignTypeOk _ _ = False

Evaluate an expression, according to its type

> eval :: Exp -> Store -> [Declaration] -> Val
> eval (Const n) _ _ = n
> eval (Bin op x y) s vars = apply op (eval x s vars) (eval y s vars)
> eval (LogNot op x) s vars = applyNot op (eval x s vars)
> eval (Item idx x) s vars = getItem idx (eval x s vars)
> eval (Var v) s vars
>    | hasKey v s = getVal v s
>    | otherwise = error ("Uninitialised variable: " ++ [v])
>    where ty = expType (Var v) vars

Retrieve an element from an array. Raise an error if the given value is not an array.

> getItem :: Int -> Val -> Val
> getItem _ (Int _) = error "Couldn't match type 'IntT' with an array type."
> getItem _ (Bool _) = error "Couldn't match type 'BoolT' with an array type."
> getItem idx (Array arr) = getItem' idx arr
>     where
>         getItem' idx [] = error ("There is no element at index: " ++ show idx)
>         getItem' idx ((i, val):xs) | idx == i = val
>                                    | otherwise = getItem' idx xs

Retrieve a value of a variable. The return value type must match
the type of the given variable.

> getVar :: Type -> Var -> Store -> Val
> getVar ty v s
>     | typeOk = var
>     | otherwise = error msg
>     where var = getVal v s
>           typeOk = matchType var ty
>           msg = "Couldn't match value '" ++ show var ++ "' with type '" ++ show ty++""

Returns true if this value's type matches with the given type, else raise an error.

> matchType :: Val -> Type -> Bool
> matchType (Int v) IntT = True
> matchType (Bool v) BoolT = True
> matchType (Array a) (ArrayT _) = True
> matchType _ _ = False

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

> test1 = if checkResult (run prog store) output
>        then "Test 1: passed - Assign an element from an array to a variable."
>        else "### Failure in: Test 1: assign an element from an array to a variable"
>        where prog = ([('x', (ArrayT (IntT, 3))), ('y', IntT)], [Asgn 'y' (Item 3 (Var 'x'))])
>              store = [('x', Array [(2, Int 33), (3, Int 43)]), ('y', Int 10)]
>              output = [('x', Array [(2, Int 33), (3, Int 43)]), ('y', Int 43)]

> test2 = if checkResult (run prog store) output
>         then "Test 2: passed - Assign a value of a variable to other variable."
>         else "### Failure in: Test 2: Assign a value of a variable to other variable."
>         where prog = ([('x', IntT), ('y', IntT)], [Asgn 'y' (Var 'x'), Asgn 'x' (Const (Int 3))])
>               store = [('x', Int 24), ('y', Int 10)]
>               output = [('x', Int 3), ('y', Int 24)]

> test3 = if (setE index newV arr) == output
>        then "Test 3: passed - SetE function test."
>        else "### Failure in: Test 3: SetE function test."
>        where index = 2
>              newV = (Int 333)
>              arr = Array [(2, Int 3)]
>              output = Array [(2, Int 333)]

> test4 = if checkResult (run prog store) output
>        then "Test 4: passed - Replaces an element (Int) in an array."
>        else "### Failure in: Test 4: Replaces an element (Int) in an array."
>        where prog = ([('x', ArrayT (IntT, 5))], [AsgnE 1 'x' (Const (Int 55))])
>              store = [('x', Array [(1, Int 99), (2, Int 22)])]
>              output = [('x', Array [(2, Int 22), (1, Int 55)])]

> test5 = if checkResult (run prog store) output
>        then "Test 5: passed - Replaces an element (Bool) in an array."
>        else "### Failure in: Test 5: Replaces an element (Bool) in an array."
>        where prog = ([('x', ArrayT (BoolT, 5))], [AsgnE 2 'x' (Const (Bool True))])
>              store = [('x', Array [(1, Bool False), (2, Bool False), (3, Bool True)])]
>              output = [('x', Array [(1, Bool False), (3, Bool True), (2, Bool True)])]

> test6 = if checkResult (run prog store) output
>        then "Test 6: passed - Replaces an element in an array with a value of Int variable."
>        else "### Failure in: Test 6: Replaces an element in an array with a value of Int variable."
>        where prog = ([('x', ArrayT (BoolT, 5)), ('y', BoolT)], [AsgnE 2 'x' (Var 'y')])
>              store = [('x', Array [(1, Bool False), (2, Bool True), (3, Bool False)]), ('y', Bool False)]
>              output = [('x', Array [(1, Bool False), (3, Bool False), (2, Bool False)]), ('y', Bool False)]

> test0 = run prog store
>        where prog = ([('x', IntT)], [Asgn 'x' (Var 'y')])
>              store = [('x', Int 1), ('y', Int 2)]


> getAllTests = test1 ++"\n"++ test2 ++"\n"++ test3 ++ "\n"++
>               test4 ++"\n"++ test5 ++"\n"++ test6

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

run ([('x', ArrayT (IntT, 1))], [Asgn 'x' (Var 'x')]) [('x', Array [(1, (Int 1))])]
run ([('x', IntT)], [Skip, (Asgn 'y' (Var 'y'))]) [('y', Int 5)]



matchType (Int 5) BoolT

getItem 2 (Array [(1, Int 2), (3, Int 4)])

run ([('x', IntT), ('y', BoolT)], [Skip, (Asgn 'x' (Var 'y'))]) [('y', Int 7)]

> test88 = run ([('x', IntT), ('y', (ArrayT (BoolT, 4)))], [Skip, (Asgn 'x' (Item 1 (Var 'y')))]) [('y', Array [(1, (Bool True))])]
