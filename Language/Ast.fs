module Bsqf.Ast

type Identifier = string

type AstStatement =
    | Expression of AstExpression
    | For of condition: AstStatement list * from: string * to_: string * step: string * code: AstStatement list
    | If of condition: AstExpression * then_: AstStatement list * else_: AstStatement list
    | While of condition: AstExpression * then_: AstStatement list
    | Break of with_: AstStatement list
    | Continue of with_: AstStatement list
    | Exit of with_: AstStatement list
and AstExpression =
    | Number of string
    | String of string
    | Identifier of Identifier
    | Binary of identifier: Identifier * left: AstExpression * right: AstExpression
    | Unary of identifier: Identifier * value: AstExpression
    | List of AstList
    | Function of AstStatement list
and AstList =
    | List of AstExpression list

type Ast = AstStatement list

type Name = {
    Identifier: string;
    HasReceiver: bool;
    IsFunction: bool;
}

type Function = {
    Imports: Name seq;
    Code: Ast;
}
