namespace RapidCalc.Models

open System.Text

(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      InfixExpression
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    Represents an infix expression, with an association to its postfix 
 |              equivilant and a method to construct one.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type InfixExpression(repr:string) =
    let tokenise(infix:string) =
        ///Helper function for tokenise method
        let isInList(list:List<string>, element:string) =
            let mutable isIn = false
            for i in list do
                if i.Equals(element) then isIn <- true
            isIn

        let mutable tokens:List<Token> = []
        let activeOperators:List<Operator> = [Plus();Minus();Multiply();Divide();Exponent()]
        let activeParenthesis:List<Parenthesis> = [OpenP(); CloseP()]
        let parReps = [ for i in activeParenthesis do yield i.toString()]
        let operatorReps = [ for i in activeOperators do yield i.toString()]
        let numericalReps = "." :: [ for i in 0..9 do yield i.ToString()]
    
        let mutable i:int = 0                   //Loop control
        let mutable j = 0
        while i < infix.Length do
            let mutable head = infix.[i].ToString()
            if isInList(operatorReps, head) then 
                ///Gauranteed to be in active operators
                let m = head;                   //Converted to immutable for lazy methods
                let operator = activeOperators |> Seq.find(fun x -> x.toString() = m) :> Token
                tokens <- operator::tokens
                i <- i + 1
            elif isInList(numericalReps, head) then
                let numberCons = new StringBuilder()
                while isInList(numericalReps, head) && i < infix.Length do
                    numberCons.Append(head) |> ignore
                    i <- i + 1
                    if i < infix.Length then 
                        head <- infix.[i].ToString()
                let operand = Operand(numberCons.ToString()) :> Token
                tokens <- operand :: tokens
            elif isInList(parReps, head) then
                ///Garaunteed to be in active parenthesis
                let m = head                    //Converted to immutable for lazy methods
                let parenthesis = 
                    activeParenthesis |> Seq.find(fun x -> x.toString() = m) :> Token
                tokens <- parenthesis :: tokens
                i <- i + 1
            else i <- i + 1
        ///Returns an immutable list of tokens which has been reversed.
        let _tokens = tokens |> List.rev
        _tokens

    let infixToPostfix(tokens:List<Token>) =
        let mutable inStack:List<Token> = tokens
        let mutable opStack:List<Token> = []
        let mutable outStack:List<Token> = []
        //let mutable current:token

        while inStack <> [] do
            let current:Token = inStack.Head
            inStack <- inStack |> List.tail
            if current :? Operator then
                ///pops the head of the operation stack into the output stack until an operation
                ///of lower presedence is found, or the list becomes empty
                while opStack <> [] && current.precedence <= opStack.Head.precedence do
                    outStack <- opStack.Head :: outStack
                    opStack <- opStack |> List.tail
                opStack <- current :: opStack
        
            elif current :? Operand then
                ///Pushes the value onto the outstack
                outStack <- current :: outStack

            elif current :? Parenthesis then
            
                if (current :? CloseP) then
                    ///Pops the stack until the other parenthesis is found, or list empty
                    while opStack <> [] && not (opStack.Head :? OpenP) do
                        outStack <- opStack.Head :: outStack
                        opStack <- opStack |> List.tail
                    ///The parenthesis are disposed of if possible (not included in postfix)
                    if opStack <> [] then
                        opStack <- opStack |> List.tail
                elif (current :? OpenP) then
                    ///the open parenthesis is added to the stack
                    opStack <- current :: opStack
    
        ///Empties the remaining non parenthesis expressions onto opstack into output
        while opStack <> [] do
            if not (opStack.Head :? Parenthesis) then outStack <- opStack.Head :: outStack
            opStack <- opStack |> List.tail

        ///Returns an immutable output stack which has been reversed
        let _outStack = outStack |> List.rev
        _outStack

    let mutable _repr:string = repr                //Member which will be bound to view

    member this.repr
        with get() = _repr                  //Gets the string representation to expression.
        and set(repr) = _repr <- repr

    member this.tokens:List<Token> = tokenise(_repr)    //Tokenised version of expression.

    ///Generates a postfix expression equivilant to this infix expression.
    member this.postfix = PostfixExpression(infixToPostfix(this.tokens))
