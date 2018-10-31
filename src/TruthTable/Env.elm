module TruthTable.Env exposing (Env, make, get)

import Dict exposing (Dict)


type Env
    = Env (Dict String Bool)


make : List String -> List Bool -> Env
make ids values =
    values
        |> List.map2 toTuple ids
        |> Dict.fromList
        |> Env


get : String -> Env -> Maybe Bool
get key (Env d) =
    Dict.get key d

toTuple : a -> b -> (a,b)
toTuple a b = (a,b)