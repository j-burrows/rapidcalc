namespace RapidCalc.Models
(*

(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      Minus
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    An operator class which performs floating point negation on operands. Unary
 |              operation is valid, negating the value.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type Minus() =
    inherit Operator()
    override this.expression(x:Operand, y:Operand) =
        ///The appropriate decimal place is chosen
        let createDP =
            if x.decPlaces > y.decPlaces
                then x.decPlaces
                else y.decPlaces
        ///The value with the lowest decimal place is shifted by the difference and the two
        ///values are negated
        let createValue:BigInteger =
            if x.decPlaces < y.decPlaces
                then (x.value * (BigInteger(10)**(y.decPlaces-x.decPlaces))) - y.value
            elif y.decPlaces < x.decPlaces
                then x.value - (y.value * (BigInteger(10)**(x.decPlaces-y.decPlaces)))
            else
                x.value-y.value
        Operand(createValue, createDP)
    override this.precedence = 2
    override this.toString() = "-"
    override this.hasUnary = true
    ///Returns an operand with the given operands value negated
    override this.unary(x:Operand) = Operand((BigInteger(-1) * x.value),x.decPlaces)

*)