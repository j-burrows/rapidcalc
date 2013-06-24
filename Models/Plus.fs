namespace RapidCalc.Models
(*
(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      Plus
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    An operator class which performs floating point addition on operands. Unary
 |              operation is valid, but does not change the value.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type Plus() =
    inherit Operator()
    override this.expression (x:Operand,y:Operand) =
        ///The appropriate decimal place is chosen
        let createDP =
            if x.decPlaces > y.decPlaces 
                then x.decPlaces
                else y.decPlaces
        ///The value with the lowest decimal place is shifted by the difference and the two
        ///values are summed together
        let createValue:BigInteger =
            if x.decPlaces < y.decPlaces
                then (x.value * (BigInteger(10)**(y.decPlaces-x.decPlaces))) + y.value
            elif y.decPlaces < x.decPlaces
                then (y.value * (BigInteger(10)**(x.decPlaces-y.decPlaces))) + x.value
            else
                x.value+y.value
        Operand(createValue, createDP)
    override this.precedence = 2
    override this.toString() = "+"
    override this.hasUnary = true
    override this.unary(x:Operand) = x      //Returns the unedited operand
*)