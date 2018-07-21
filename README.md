# TruthTable

A library for generating truth tables.

## Example

```elm
import TruthTable
import TruthTable.Expr exposing (Expr(..))

-- De Morgan's Law
TruthTable.fromExprs
    [ Not (Or (Var "P") (Var "Q"))
    , And (Not (Var "P")) (Not (Var "Q"))
    ]
```
