COMP304 Assignment 3, 2017.

Initial code from Author: Lindsay Groves, VUW, 2017.
Modified by: Vivienne Yapp

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

> data Val = Int Int | Bool Bool | Array Array
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

A store is a map from variables to values

> type Array = Map Int Val

To run a program with a given initial store, we just pass the program and
store to exec.

> run :: Prog -> Store -> Store
> run (vars, stmts) store
>    | varsOk prog && noRepeated = exec prog store
>    | otherwise = error ("Variables uninitialised or undeclared.")
>    where prog = (vars, stmts)
>          noRepeated = noRepeatVars vars []

Added function: To check if a list of variable declarations contains duplicated name or not.
If none, returns True, otherwise raise an error.

> noRepeatVars :: [(Var, Type)] -> [Var] -> Bool
> noRepeatVars [] _ = True
> noRepeatVars ((v, ty):rest) names
>     | elem v names = error ("Duplicated variable name: "++[v])
>     | otherwise = noRepeatVars rest (names++[v])

Added function: Check statically that all variables used are declared
before executing the program. If none, returns True, otherwise False.

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

Added function: To check if a variable consist in a list of variable declarations.

> varOk :: Var -> [Declaration] -> Bool
> varOk var [] = error ("Undeclared variable: " ++ [var])
> varOk var ((v, ty):xs) | var == v = True
>                        | otherwise = varOk var xs

To execute a program, we just execute each statement in turn,
passing the resulting state to the next statement at each step.

> exec :: Prog -> Store -> Store
> exec (vars, []) store = store
> exec (vars, (stmt : rest)) store = exec (vars, rest) (exec' vars stmt store)

Added function: Execute a single statement, according to its semantics

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
>      | otherwise = error ("Assignment to array "++ show var++" at index "++show idx ++ " failed.")
>      where newV = (eval exp store vars)
>            declaredV = getDeclaredVar var vars
>            arr = getVal var store
>            newArray = (setItem idx newV declaredV arr)

            newArray = (setVal idx newV arr)

Added function: Casting a value into an array type. An error will raise
if the given value is Boolean or Integer type.

> getArray :: Val -> Array
> getArray (Bool _) = error("Value is not an array. Actual type: Bool")
> getArray (Int _) = error("Value is not an array. Actual type: Int")
> getArray (Array a) = a

Added function: Replaces the element at the specified position in this array with
the specified element.

> setItem :: Int -> Val -> Declaration -> Val -> Val
> setItem idx newV (_, (ArrayT (_, size))) arr
>     | idx < 0 || idx > size = error "Array index out of bounds."
>     | otherwise = Array (setVal idx newV (getArray arr))

Added function: Replaces the element at the specified position in this array with
the specified element. This value is not longer need as an array is store as Map
and update action on array can be done using the function in Map.

> setE :: Int -> Val -> Val -> Declaration -> Val
> setE idx v (Int _) _ = error ("Cannot set an element into an Int type.")
> setE idx v (Bool _) _ = error ("Cannot set an element into a Bool type.")
> setE idx v (Array a) (_, (ArrayT (_, size)))
>     | idx < 0 || idx > size = error "Array index out of bounds."
>     | otherwise = setE' idx v a []

> setE' :: Int -> Val -> Array -> Array -> Val
> setE' idx v [] newArray = Array (newArray++[(idx, v)])
> setE' idx v ((i, val):xs) newArray
>        | idx == i = Array (newArray ++ xs ++ [(idx, v)])
>        | otherwise = setE' idx v xs (newArray++[(i, val)])

Added function: To check if the type of a value match the type of the declared variable.

> assignTypeOk :: Val -> Declaration -> Bool
> assignTypeOk (Int _) (_, IntT) = True
> assignTypeOk (Bool _) (_, BoolT) = True
> assignTypeOk (Int _) (_, (ArrayT (IntT, _))) = True
> assignTypeOk (Bool _) (_, (ArrayT (BoolT, _))) = True
> assignTypeOk (Array _) (_, (ArrayT _)) = True
> assignTypeOk v (_, ty) = error ("Couldn't match value '" ++ show v ++ "' with type '" ++ show ty)

Added function: Evaluate an expression, according to its type

> eval :: Exp -> Store -> [Declaration] -> Val
> eval (Const n) _ _ = n
> eval (Bin op x y) s vars = apply op (eval x s vars) (eval y s vars)
> eval (LogNot op x) s vars = applyNot op (eval x s vars)
> eval (Item idx x) s vars = getItem idx (eval x s vars)
> eval (Var v) s vars
>    | hasKey v s = getVal v s
>    | otherwise = error ("Uninitialised variable: " ++ [v])
>    where ty = expType (Var v) vars

Added function: Retrieve an element from an array. Raise an error if
the given value is not an array.

> getItem :: Int -> Val -> Val
> getItem _ (Int _) = error "Couldn't match type 'IntT' with an array type."
> getItem _ (Bool _) = error "Couldn't match type 'BoolT' with an array type."
> getItem idx (Array arr)
>       | hasKey idx array = getVal idx array
>       | otherwise = error ("There is no element at index: " ++ show idx)
>       where array = getArray (Array arr)

Added function: To get declared variable from a list of variable declarations.

> getDeclaredVar :: Var -> [Declaration] -> Declaration
> getDeclaredVar var [] = error ("Undeclared variable: " ++ [var])
> getDeclaredVar var ((v, ty):xs)
>    | var == v = (v, ty)
>    | otherwise = getDeclaredVar var xs

Added function: To check the type of an expression.

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

Added function: Returns true if this value's type matches with the given type, else raise an error.

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


The following is some examples of error-reporting when illegal action is performed.

Error 1: When try to assign a boolean type value to an integer type array.

< > *While2> run ([('x', IntT), ('y', (ArrayT (BoolT, 4)))], [Skip, (Asgn 'x' (Item 1 (Var 'y')))]) [('y', Array [(1, (Bool True))])]
< > *** Exception: Variable assignment error: Couldn't match value 'Bool True' with type 'IntT'
< > CallStack (from HasCallStack):
< >  error, called at While2.lhs:139:21 in main:While2

Error 2: When try to assign a boolean variable to an array variable.

< > *While2> run ([('x', IntT), ('y', (ArrayT (BoolT, 4)))], [(Asgn 'y' (Var 'x'))]) [('y', Array [(1, (Bool True))]), ('x', Int 88)]
< > *** Exception: Variable assignment error: Couldn't match value 'Int 88' with type 'ArrayT (BoolT,4)'
< > CallStack (from HasCallStack):
< >   error, called at While2.lhs:139:21 in main:While2

Error 3: When try to update an element of Boolean type array to an Integer value.

< > *While2> run ([('x', IntT), ('y', (ArrayT (BoolT, 4)))], [(AsgnE 1 'y' (Var 'x'))]) [('y', Array [(1, (Bool True))]), ('x', Int 88)]
< > *** Exception: Couldn't match value 'Int 88' with type 'ArrayT (BoolT,4)
< > CallStack (from HasCallStack):
< >  error, called at While2.lhs:197:28 in main:While2

Error 4: When try retrive an element at given index that do not contain in the array.

< > *While2> run ([('x', IntT), ('y', (ArrayT (BoolT, 4)))], [(AsgnE 1 'y' (Item 2 (Var 'y')))]) [('y', Array [(1, (Bool True))])]
< > *** Exception: There is no element at index: 2
< > CallStack (from HasCallStack):
< >  error, called at While2.lhs:222:23 in main:While2

Error 5: Update an element of array when the given variable is not an array.

< > *While2> run ([('x', ArrayT (IntT, 5)), ('y', IntT)], [AsgnE 2 'y' (Item 1 (Var 'x'))]) [('x', Array [(1, Int 99), (2, Int 22)]), ('y', Int 11)]
< > [('y',Array [(2,Int 99)*** Exception: Value is not an array. Actual type: Int
< > CallStack (from HasCallStack):
< >   error, called at While2.lhs:169:22 in main:While2

Error 6: When try retrive an element at index that larger than the size of an array

< > *While2> run ([('x', IntT), ('y', (ArrayT (BoolT, 4)))], [(AsgnE 6 'y' (Item 1 (Var 'y')))]) [('y', Array [(1, (Bool True))])]
< > [('y',*** Exception: Array index out of bounds.
< > CallStack (from HasCallStack):
< >  error, called at While2.lhs:176:33 in main:While2


TESTING:

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

> test3 = if (setE index newV arr dclr) == output
>         then "Test 3: passed - SetE function test."
>         else "### Failure in: Test 3: SetE function test."
>         where index = 2
>               newV = (Int 333)
>               arr = Array [(2, Int 3)]
>               output = Array [(2, Int 333)]
>               dclr = ('x', (ArrayT (IntT, 5)))

> test4 = if checkResult (run prog store) output
>         then "Test 4: passed - Replaces an element (Int) in an array."
>         else "### Failure in: Test 4: Replaces an element (Int) in an array."
>         where prog = ([('x', ArrayT (IntT, 5))], [AsgnE 1 'x' (Const (Int 55))])
>               store = [('x', Array [(1, Int 99), (2, Int 22)])]
>               output = [('x', Array [(1, Int 55), (2, Int 22)])]

> test5 = if checkResult (run prog store) output
>         then "Test 5: passed - Replaces an element (Bool) in an array."
>         else "### Failure in: Test 5: Replaces an element (Bool) in an array."
>         where prog = ([('x', ArrayT (BoolT, 5))], [AsgnE 2 'x' (Const (Bool True))])
>               store = [('x', Array [(1, Bool False), (2, Bool False), (3, Bool True)])]
>               output = [('x', Array [(2, Bool True), (1, Bool False), (3, Bool True)])]

> test6 = if checkResult (run prog store) output
>         then "Test 6: passed - Replaces an element in an array with a value of Int variable."
>         else "### Failure in: Test 6: Replaces an element in an array with a value of Int variable."
>         where prog = ([('x', ArrayT (BoolT, 5)), ('y', BoolT)], [AsgnE 2 'x' (Var 'y')])
>               store = [('x', Array [(1, Bool False), (2, Bool True), (3, Bool False)]), ('y', Bool False)]
>               output = [('x', Array [(2, Bool False), (1, Bool False), (3, Bool False)]), ('y', Bool False)]

> test7 = if checkResult (run prog store) output
>         then "Test 7: passed - Update a variable using an element of an array."
>         else "### Failure in: Test 7: Update a variable using an element of an array."
>         where prog = ([('x', ArrayT (IntT, 5)), ('y', IntT)], [Asgn 'y' (Item 1 (Var 'x'))])
>               store = [('x', Array [(1, Int 99), (2, Int 22)]), ('y', Int 11)]
>               output = [('y', Int 99), ('x', Array [(1, Int 99), (2, Int 22)])]

> test8 = if checkResult (run prog store) output
>         then "Test 8: passed - Set an element at given index."
>         else "### Failure in: Test 8: Set an element at given index."
>         where prog = ([('x', ArrayT (IntT, 5)), ('y', IntT)], [AsgnE 3 'x' (Var 'y')])
>               store = [('x', Array [(1, Int 99), (2, Int 22)]), ('y', Int 11)]
>               output = [('x', Array [(3,Int 11), (1, Int 99), (2, Int 22)]), ('y',Int 11)]

> getAllTests = test1 ++"\n"++ test2 ++"\n"++ test3 ++ "\n"++
>               test4 ++"\n"++ test5 ++"\n"++ test6 ++"\n"++ test7 ++"\n"++ test8

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

Examples of how data represent in a store:

Integer => Int 99
Boolean => Boolean True
Array => Array [(1, Int 88), (3, Int 55)]

Examples of variable declaration:

Integer => ('x', IntT)
Boolean => ('x', BoolT)
Array => ('x', ArrayT (IntT, 5))

DISCUSSION:

Part 2: Adding Arrays

To extend the array, I chose to use the Map module provided - the key as an index and
the value as an element of an array reference. To declare an array, we have to write it
as (var_name, ArrayT (element_type, size)) -> ('x', ArrayT (IntT, 4)). To use an array
in the store, we have to write Array [(1, Int 55), (2, Int 44)].

I implemented it this way so we could retrieve an element of the array using the index
expression and also update an element easily using the function in the Map module.
I could use represent an array using lists but then I have to write extra functions
for retrieve, update or other operation of an array. Also, I have to have a counter
as an extra argument to go through the list from the head till the matched index.

To update an element of an array, I added it as a statement "AsgnE Int Var Exp" since we need to
know the index of the element that need to be update. To retrieve an element, I added an expression
"Item Int Exp" which Int is the index and Exp must be an array variable. The given index must be
within the size of the array and an error will raise if there is not element at given index.

Note: Above contains error reports and test cases.
