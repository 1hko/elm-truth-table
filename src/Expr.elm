module Expr exposing (Expr(..), eval, toString, vars)

{-| Represent propositional expressions. Can be used to automatically generate a truth table.


# Definition

@docs Expr, toString


# Advanced

@docs eval, vars

-}

import Env exposing (Env)
import Set exposing (Set)


{-| A propositional expression.

    -- ¬P
    Not (Var "P")

    -- ¬P ∨ Q
    Or (Not (Var "P")) (Var "Q")

    -- P ⇒ Q
    Implies (Var "P")  (Var "Q")

-}
type Expr
    = Or Expr Expr
    | And Expr Expr
    | Xor Expr Expr
    | Implies Expr Expr
    | Equiv Expr Expr
    | Not Expr
    | Var String


{-| Transform an `Expr` to a `String`

    Var "P" |> Expr.toString
        == "P"

    And (Var "P") (Var "Q") |> Expr.toString
        == "P ∧ Q"

    Or (Not (Var "P")) (Var "Q") |> Expr.toString
        == "¬P ∨ Q"

-}
toString : Expr -> String
toString expr =
    case expr of
        And a b ->
            toString a ++ " ∧  " ++ toString b

        Or a b ->
            toString a ++ " ∨ " ++ toString b

        Xor a b ->
            toString a ++ " ⊕ " ++ toString b

        Implies a b ->
            toString a ++ " ⇒ " ++ toString b

        Equiv a b ->
            toString a ++ " ⇔ " ++ toString b

        Not a ->
            "¬" ++ toString a

        Var a ->
            a


{-| Determine the variables used in an expression.

    Xor (And (Var "P") (Var "Q")) (Or (Var "P") (Var "Q"))
        == Set.fromList [ "P", "Q" ]

-}
vars : Expr -> Set String
vars expr =
    case expr of
        Or a b ->
            Set.union (vars a) (vars b)

        Xor a b ->
            Set.union (vars a) (vars b)

        And a b ->
            Set.union (vars a) (vars b)

        Implies a b ->
            Set.union (vars a) (vars b)

        Equiv a b ->
            Set.union (vars a) (vars b)

        Not a ->
            vars a

        Var a ->
            Set.singleton a


{-| Evaluate an expression with the specified environment.

    And (Var "P") (Var "Q")
        |> eval (Env.make [ "P", "Q" ], [ True, False ])
        == Ok False

-}
eval : Env -> Expr -> Result String Bool
eval env expr =
    case expr of
        And a b ->
            Result.map2 (&&) (eval env a) (eval env b)

        Or a b ->
            Result.map2 (||) (eval env a) (eval env b)

        Xor a b ->
            Result.map2 xor (eval env a) (eval env b)

        Implies a b ->
            Result.map2 implies (eval env a) (eval env b)

        Equiv a b ->
            Result.map2 equiv (eval env a) (eval env b)

        Not a ->
            Result.map not (eval env a)

        Var a ->
            Env.get a env
                |> Result.fromMaybe ("unbound identifier: " ++ Basics.toString a)


implies : Bool -> Bool -> Bool
implies a b =
    not a || b


equiv : Bool -> Bool -> Bool
equiv a b =
    not (xor a b)
