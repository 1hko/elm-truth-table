module TruthTable exposing (fromExprs)

{-| This library generates [truth tables](https://en.wikipedia.org/wiki/Truth_table) for use with
propositional logic.

@docs fromExprs

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Set exposing (Set)
import Env exposing (Env)
import Expr exposing (Expr(..))


{-| Generate a truth table from a list of expressions.

    import TruthTable
    import TruthTable.Expr exposing (Expr(..))

    -- De Morgan's Law
    TruthTable.fromExprs
        [ Not (Or (Var "P") (Var "Q"))
        , And (Not (Var "P")) (Not (Var "Q"))
        ]

A text preview of the generated table.

```plain
P     Q     (¬P) ∧ (¬Q)     ¬(P ∨ Q)
-------------------------------------
T     T     F               F
T     F     F               F
F     T     F               F
F     F     T               T
```

-}
fromExprs : List Expr -> Html msg
fromExprs exprs =
    let
        ids =
            exprs
                |> List.map Expr.vars
                |> List.foldl Set.union Set.empty
                |> Set.toList

        columns =
            ids ++ List.map Expr.toString exprs

        count =
            List.length ids

        rows =
            permute count [ True, False ]
                |> List.map (tableRows (Env.make ids) exprs)
    in
        Html.table [ Attributes.style [ ( "width", "100%" ) ] ]
            [ Html.thead [] [ tableHeader columns ]
            , Html.tbody [] rows
            ]


tableHeader : List String -> Html msg
tableHeader labels =
    labels
        |> List.map (tableCell [ ( "font-weight", "bold" ) ])
        |> Html.tr []


tableRows : (List Bool -> Env) -> List Expr -> List Bool -> Html msg
tableRows makeEnv exprs values =
    let
        env =
            makeEnv values

        results =
            exprs
                |> List.map (Expr.eval env)
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> Result.map ((++) values)
                |> Result.withDefault []

        viewCell truth =
            if truth then
                tableCell [ ( "color", "#19A974" ) ] "T"
            else
                tableCell [ ( "color", "#E7040F" ) ] "F"
    in
        results
            |> List.map viewCell
            |> Html.tr []


tableCell : List ( String, String ) -> String -> Html msg
tableCell style text =
    Html.td [ Attributes.style style ] [ Html.text text ]


permute : Int -> List a -> List (List a)
permute n list =
    let
        loop acc n xs =
            if n == 0 then
                [ acc ]
            else
                list
                    |> List.concatMap (\x -> loop (acc ++ [ x ]) (n - 1) xs)
    in
        loop [] n list
