namespace RapidCalc.Models
(*
open System.Numerics
(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      Operand
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    A value which can be operated on, can be arbitrary large, and supports
 |              floating point arthemetic by having a decimal place position.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type Operand(value:BigInteger, decPlaces:int) =
    inherit Token()
    member this.value = value               //The value without decimal places
    member this.decPlaces = decPlaces       //The position from right of the decimal place.
    
    ///Returns a string that has the string value of the values left of the decimal place,
    ///and, if there are decimal places: a decimal place, and right of the decimal place
    override this.toString() =
        let isNegative = this.value < BigInteger.Zero
        let isFloating = this.decPlaces > 0
        let decimalOffset = BigInteger(10) ** this.decPlaces

        let _value = 
            if isNegative then BigInteger(-1)*this.value
            else this.value
        
        let nameBuilder = new StringBuilder()

        if isNegative then do
            nameBuilder.Append("-") |> ignore

        let naturalValues = (_value / decimalOffset).ToString()
        nameBuilder.Append(naturalValues) |> ignore

        if isFloating then do
            let decimalValues = (_value % decimalOffset).ToString()
            nameBuilder.Append(".") |> ignore
            nameBuilder.Append(decimalValues) |> ignore

        nameBuilder.ToString()
        

    override this.precedence = 0            //Operands have no priority in expressions.
    
    ///An overloaded constructor for created an operatand from a string representation.
    new (stringRep:string) = 
        ///Validility check to see if empty string, creates zero value if so
        if stringRep.Length = 0 then 
            Operand(BigInteger(0),0)
        else
            ///Adds a leading zero if one is not there
            let validStringRep =
                if stringRep.ToCharArray().[0] = '.' then
                    "0" + stringRep
                else stringRep
            ///The string is valid
            let parts = validStringRep.Split([|'.'|]) |> Array.map (fun s -> s.Trim())
            if parts.Length = 1 || parts.[1].Length = 0 then
                ///There are no decimal places to be had
                Operand(BigInteger.Parse(parts.[0]),0)
            else
                ///There are decimal places that must be accounted for
                let mutable toBeExpanded = BigInteger.Parse(parts.[0])
                let decimalPlaces = parts.[1].Length
                ///The value is shifted left by n-decimal place values
                toBeExpanded <- toBeExpanded * (BigInteger(10)**decimalPlaces)
                toBeExpanded <- toBeExpanded + BigInteger.Parse(parts.[1])
                Operand(toBeExpanded, parts.[1].Length)

    //Overloaded constructor for creating operands from integer numbers.
    //new (intRep:int) =
    //    Operand(BigInteger(intRep),0)
*)