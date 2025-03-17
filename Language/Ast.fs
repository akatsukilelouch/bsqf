module Bsqf.Ast
open FParsec

type IdentifierOrRawValue = Position * string

type AstStatementValue =
    | Expression of AstExpression
    | For of name: IdentifierOrRawValue * from: IdentifierOrRawValue option * to_: IdentifierOrRawValue option * step: IdentifierOrRawValue option * code: AstStatement list
    | While of condition: AstExpression * then_: AstStatement list
    | Break of with_: AstExpression option
    | Continue of with_: AstExpression option
    | Exit of with_: AstExpression option
    | Set of name: IdentifierOrRawValue * value: AstExpression
    | AnonymousSet of name: string * value: AstExpression // `_x` binding in forEach, used later when compiling
and AstStatement = Position * AstStatementValue
and AstExpressionValue =
    | Number of string
    | String of string
    | Identifier of IdentifierOrRawValue
    | AnonymousIdentifier of name: string // `_x` binding in forEach, used later when compiling
    | ForEach of name: IdentifierOrRawValue * of_: AstExpression * code_: AstExpression
    | If of condition: AstExpression * then_: AstExpression * else_: AstExpression option
    | List of AstExpression list
    | Block of AstStatement list
    | Call of identifier: IdentifierOrRawValue * args: AstExpression list
and AstExpression = Position * AstExpressionValue

type AstTree = AstStatement list

type AstDeclare =
    | Function of name: IdentifierOrRawValue
    | Operator of name: IdentifierOrRawValue * hasReceiver: bool

    member this.Name = match this with | Function name | Operator (name, _) -> name

/// Intermediary type for better handling. These are spewed out by the parser.
/// They exists so the code for processing imports and other codegen stuff is put here.
type AstTopLevelValue =
    | Module of identifier: IdentifierOrRawValue
    | Import of name: IdentifierOrRawValue * identifier: IdentifierOrRawValue list
    | Declare of AstDeclare
    | Definition of identifier: IdentifierOrRawValue * args: IdentifierOrRawValue list * tree: AstTree
    | Do of tree: AstTree
type AstTopLevel = Position * AstTopLevelValue
