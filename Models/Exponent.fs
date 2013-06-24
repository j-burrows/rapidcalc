namespace RapidCalc.Models
(*
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
        | None -> 10

    let rec factorial(degree:int) = if degree = 1 then 1 else degree * factorial(degree - 1) 

    let binomial(given:Operand, order:int) =
        let minus = Minus()
        let multiply = Multiply()
        let mutable result = given

        for i in 0..(order-2) do
            let offset = Operand(BigInteger(i),0)
            let stepValue = minus.expression(given, offset)
            result <- multiply.expression(result, given)
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
    *)