type operator =
        | Plus
        | Minus
        | Multiply
        | Divide
[@@deriving show, eq]

type expr =
        | Int of int
        | Var of string
        | Binary of expr * operator * expr
        | Let of string * expr
        | Print of expr
[@@deriving show, eq]

type program = expr list
[@@ deriving show, eq]
