open Base

type whitespace_control = Trim | White [@@deriving show]

type block_token =
  | StatementStart of whitespace_control
  | StatementEnd of whitespace_control
  | ExpressionStart of whitespace_control
  | ExpressionEnd of whitespace_control
  | LiquidStart
  | RawText of string
[@@deriving show]

type operator = Eq | Gte | Gt | Lte | Lt | Ne | Contains [@@deriving show]

type lex_value =
  | LexBool of bool
  | LexString of string
  | LexNumber of float
  | LexId of string list
  | LexRange of int * int
  | LexNil
  | LexBlank
[@@deriving show]

type lex_combiner = LexAnd | LexOr [@@deriving show]

type lex_token =
  | If
  | EndIf
  | Unless
  | EndUnless
  | Case
  | EndCase
  | LexFor
  | LexEndFor
  | Capture
  | EndCapture
  | Paginate
  | EndPaginate
  | TableRow
  | EndTableRow
  | Raw
  | EndRaw
  | ElseIf
  | Else
  | When
  | LexForm
  | LexStyle
  | LexEndForm
  | LexEndStyle
  | LexInclude
  | LexRender
  | LexLayout
  | LexSection
  | LexBreak
  | LexContinue
  | Cycle
  | In
  | By
  | LexWith
  | LexAs
  | Assign
  | Increment
  | Decrement
  | Pipe
  | Colon
  | Equals
  | Comma
  | LexNone
  | Space
  | Newline
  | Operator of operator
  | LexText of string
  | LexCombiner of lex_combiner
  | LexValue of lex_value
  | LexExpression of lex_token list
  | EOS
[@@deriving show]
