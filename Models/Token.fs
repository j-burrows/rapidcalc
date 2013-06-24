(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Filename:   Token.fs
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    Provide the tokens that will be used in expression trees and lines.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Author:     Jonathan Burrows
 |  Date:       May 31st, 2013
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
namespace RapidCalc.Models

open System
open System.Numerics
open System.Text

(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Abstract Class: Token
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    Provide a general class that can be used in lists and trees, with children
 |              that can either operate or be operated on.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
[<AbstractClass>]
type Token() =
    abstract member toString: unit -> string
    abstract member precedence:int

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
        let mutable repList = stringRep.ToCharArray() |> Seq.toList
        let mutable _decPlaces = 0

        //Removes signs and marks the value as positive or negative
        let isNegative = repList.Head = '-'
        if isNegative then repList <- repList.Tail

        if not isNegative && repList.Head = '+' then repList <- repList.Tail

        //Removes all leading zeros
        while repList <> [] && repList.Head = '0' do repList <- repList.Tail

        //If the last leading zero proceeded a decimal place, replace that zero
        if repList <> [] && repList.Head = '.' then repList <- '0' :: repList

        if repList = [] then repList <- ['0']

        let isFloat = ((repList |> List.tryFind(fun x -> x = '.')) <> None)

        if isFloat then
            ///Reverses the list to format the decimal places
            let mutable revList = repList |> List.rev
            ///All trailing zeros are removed
            while revList <> [] && revList.Head = '0' do revList <- revList.Tail
            ///If the last zero removed was trailing the decimal place, readd the zero
            if revList.Head = '.' then revList <- '0' :: revList

            let mutable decCountList = revList
            while decCountList <> [] && decCountList.Head <> '.' do
                _decPlaces <- _decPlaces + 1
                decCountList <- decCountList.Tail

            ///Special case of 0.0 being replaced with just zero
            if revList.Length = 3 && revList.[0] = '0' && revList.[1] = '.' && revList.[2] = '0' then
                revList <- ['0']
                _decPlaces <- 0

            repList <- revList |> List.filter(fun x -> x <> '.') |> List.rev

        ///Constructs a string from the list of characters
        (*
        let sb = StringBuilder()
        while repList <> [] do
            sb.Append(repList.ToString()) |> ignore
            repList <- repList.Tail
        *)
        let mutable stringValue = ""
        while repList <> [] do
            stringValue <- stringValue + repList.Head.ToString()
            repList <- repList.Tail

        let value = 
            if isNegative then BigInteger(-1) * BigInteger.Parse(stringValue)
            else BigInteger.Parse(stringValue)

        Operand(value, _decPlaces)
      
    //new (intRep:int) =
    //    Operand(BigInteger(intRep),0)

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

(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      Multiply
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    An operator class which performs floating point mulitication on operands.
 |              Unary operations are not supported. Precision addeds decimal places to the
 |              expression before operations to prevent precision loss.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type Multiply(?precision) =
    inherit Operator()
    //Added decimal places to prevent precision loss
    let addedDP =
        match precision with
        | Some value -> value
        | None -> 32
    let addedOffset:BigInteger = BigInteger(10) ** addedDP

    override this.expression(x:Operand, y:Operand) =
        ///The decimal places of the two values are added together (with additional dps)
        let mutable createDP = x.decPlaces + y.decPlaces + addedDP
        let mutable createValue:BigInteger = addedOffset * x.value * y.value

        //Any ending zeros are removed
        while (createDP > 0) && (createValue % BigInteger(10) = BigInteger(0)) do
            createValue <- createValue / BigInteger(10)
            createDP <- createDP - 1

        Operand(createValue, createDP)
    override this.precedence = 4
    override this.toString() = "*"
    override this.hasUnary = false
    override this.unary(x:Operand) = x      //Placeholder, should not be evaluated.

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

(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      Exponent
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    A token that changes the presedence of infix operations.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type Exponent(?nthOrder) =
    inherit Operator()
    
    let _nthOrder = 
        match nthOrder with
        | Some order -> order
        | None -> 6

    let rec factorial(degree:int) = if degree = 1 then 1 else degree * factorial(degree - 1) 

    let binomial(given:Operand, order:int) =
        let minus = Minus()
        let multiply = Multiply()
        let mutable result = given

        for i in 1..(order-1) do
            let offset = Operand(BigInteger(i),0)
            let stepValue = minus.expression(given, offset)
            result <- multiply.expression(result, stepValue)
        result

    let partialExponent(x:Operand, y:Operand) =
        let multiply = Multiply()
        let plus = Plus()
        let minus = Minus()
        let divide = Divide()
        let one = Operand(BigInteger(1),0)
        let shiftedX = minus.expression(x,one)
        let mutable result = Operand(BigInteger(1),0)
        let mutable i = 1

        while i <= _nthOrder do
            let binStepValue = binomial(y, i)
            let facDivisor = Operand(BigInteger(factorial(i)),0)
            let mutable xScale = Operand(BigInteger(1),0)
            let mutable j = 1
            while j <= i do
                xScale <- multiply.expression(xScale,shiftedX)
                j <- j + 1
            let stepResult = divide.expression(
                                    multiply.expression(binStepValue, xScale),
                                    facDivisor)
            result <- plus.expression(result, stepResult)
            i <- i + 1
        result

    let exponent(x:Operand, y:Operand) =
        let multiply = Multiply()
        let mutable result = Operand(BigInteger(1),0)
        let mutable i = BigInteger(0)
        let upperBound = 
            if y.value < BigInteger.Zero then y.value * BigInteger(-1)
            else y.value

        while i < upperBound do
            result <- multiply.expression(result, x)
            i <- i + BigInteger(1)
        
        result

    override this.expression(x:Operand, y:Operand) =
        if y.value > BigInteger.Zero then
            if y.decPlaces = 0 then
                exponent(x,y)
            else 
                partialExponent(x,y)
        else
            let divide = Divide()
            let positiveY = Operand(BigInteger(-1)*y.value,y.decPlaces)
            let one = Operand(BigInteger(1),0)
            if y.decPlaces = 0 then
                let result = exponent(x,positiveY)
                divide.expression(one, result)
            else
                let result = partialExponent(x,positiveY)
                divide.expression(one, result)

    override this.precedence = 5
    override this.toString() = "^"
    override this.hasUnary = false
    override this.unary(x:Operand) = x


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

 