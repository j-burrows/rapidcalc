namespace RapidCalc.Models
(*
(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      Divide
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    An operator class which performs floating point division on operands.
 |              Unary operations are not valid. Precision addeds decimal places to the
 |              expression before operations to prevent precision loss.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type Divide(?precision) =
    inherit Operator()
    //Added decimal places to prevent precision loss
    let addedDP =
        match precision with
        | Some value -> value
        | None -> 32
    let addedOffset =  BigInteger(10) ** addedDP

    override this.expression(x:Operand, y:Operand) =
        let divider:Operand =
            if y.value = BigInteger.Zero then Operand(BigInteger(1),addedDP)
            else y
        ///The decimal places of the second value is negated from the first 
        ///(with additional dps)
        let mutable createDP = x.decPlaces - divider.decPlaces + addedDP
        let mutable createValue:BigInteger = addedOffset * x.value / divider.value

        //Any ending zeros are removed
        while (createDP > 0) && (createValue % BigInteger(10) = BigInteger(0)) do
            createValue <- createValue / BigInteger(10)
            createDP <- createDP - 1

        Operand(createValue, createDP)
    override this.precedence = 4
    override this.toString() = "/"
    override this.hasUnary = false
    override this.unary(x:Operand) = x      //Placeholder, should not be evaluated.
    *)