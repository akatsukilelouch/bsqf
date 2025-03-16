module Bsqf.Ast
open FParsec

type Identifier = string

type AstStatementValue =
    | Expression of AstExpression
    | For of name: Identifier * from: string option * to_: string option * step: string option * code: AstStatement list
    | While of condition: AstExpression * then_: AstStatement list
    | Break of with_: AstExpression option
    | Continue of with_: AstExpression option
    | Exit of with_: AstExpression option
    | Set of name: Identifier * value: AstExpression
and AstStatement = Position * AstStatementValue
and AstExpressionValue =
    | Number of string
    | String of string
    | Identifier of Identifier
    | ForEach of name: Identifier * of_: AstExpression * code_: AstExpression
    | If of condition: AstExpression * then_: AstExpression * else_: AstExpression option
    | List of AstExpression list
    | Block of AstStatement list
    | Call of identifier: Identifier * args: AstExpression list
and AstExpression = Position * AstExpressionValue

type AstTree = AstStatement list

type AstDeclare =
    | Function of name: Identifier
    | Operator of name: Identifier * hasReceiver: bool

/// Intermediary type for better handling. These are spewed out by the parser.
/// They exists so the code for processing imports and other codegen stuff is put here.
type AstTopLevel =
    | Module of identifier: Identifier
    | Import of name: string * identifier: Identifier list
    | Declare of AstDeclare
    | Definition of identifier: Identifier * tree: AstTree
    | Do of tree: AstTree
