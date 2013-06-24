namespace RapidCalc.Models
(*
(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Abstract Class: Parenthesis
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    Empty abstract class for cleanly identifying open or closed parenthesises.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
[<AbstractClass>]
type Parenthesis() =
    inherit Token()

(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      OpenP
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    A token that changes the presedence of infix operations.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type OpenP() =
    inherit Parenthesis()
    override this.toString() = "("
    override this.precedence = -1           ///Lowest possible precedence

(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      CloseP
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    A token that changes the presedence of infix operations.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type CloseP() =
    inherit Parenthesis()
    override this.toString() = ")"
    override this.precedence = 999          ///Highest possible precedence
*)