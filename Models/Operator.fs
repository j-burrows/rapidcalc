namespace RapidCalc.Models
(*
(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Abstract Class: Operator
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    An abstract class which can do arthemetic operations on operands. Has a 
 |              binary expression, a unary expression, and a bool for if the unary is valid.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
[<AbstractClass>]
type Operator() =
    inherit Token()
    abstract member expression: Operand * Operand -> Operand
    abstract member hasUnary:bool
    abstract member unary: Operand -> Operand
*)