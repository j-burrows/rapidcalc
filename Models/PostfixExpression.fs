namespace RapidCalc.Models

open System.Text
open System.Numerics
open System
open System.ComponentModel
open System.IO
open System.Threading
open RapidCalc.Repository
open System.Linq
open System.Data
open System.Data.Linq
open System.Data.Linq.Mapping
open System.ComponentModel.DataAnnotations

(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      TokenTree
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    A descriminatory union for a tree, which supports imbalance and errors.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type TokenTree =
    | TokenTree of Token * TokenTree * TokenTree * string
    | Ltree of Token * TokenTree * string
    | Leaf of Token * string
    | Error of string

type LockFunctions() =
    member this.readLock(rwlock: ReaderWriterLock) f =
        rwlock.AcquireReaderLock(Timeout.Infinite)
        try f()
        finally rwlock.ReleaseReaderLock()

    member this.writeLock(rwlock: ReaderWriterLock) f =
        rwlock.AcquireWriterLock(Timeout.Infinite)
        try
            f()
            Thread.MemoryBarrier()
        finally
            rwlock.ReleaseWriterLock()

(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      resultWrapper
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    Allows a mutable data structure to be passed to delegates
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type resultWrapper() =
    let mutable _result:Operand = Operand (BigInteger(0),0)
    let mutable _isAnswered:bool = false
    let _resultLock = new ReaderWriterLock()
    let lockers = LockFunctions()
    member this.result 
        with get() = lockers.readLock _resultLock (fun() -> _result)
        and set(result:Operand) = lockers.writeLock _resultLock (fun () -> 
            _result <- result
            _isAnswered <- true)
    member this.isAnswered with get() = lockers.readLock _resultLock (fun()->_isAnswered)

(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Class:      PostfixExpression
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    Represents a postfix expression, also contains functions to construct, 
 |              evaluate, and validate trees. 
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
type PostfixExpression(tokens:List<Token>) =
    let repository = new SqlExpressionRepository()
    ///Helper method to construct a string representation of a given tree.
    let rec treeRep(curr:TokenTree) =
        match curr with
        | TokenTree(value:Token, leftTree:TokenTree, rightTree:TokenTree, repr:string) -> 
            repr
        | Ltree(value:Token, leftTree:TokenTree, repr:string) -> repr
        | Leaf(value:Token, repr:string) -> repr
        | Error(errorMessage:string) -> errorMessage

    ///Fixes the specific case of a node being intended to be its child's right node
    ///for a unary operation. The node is merged with its child if it's token operator 
    ///supports unary operations, it's child is a left tree, which does not support unary.
    let rec fixForUnary(curr:TokenTree ref) =
        match !curr with
        | Ltree(currOp, left, repr) ->
            fixForUnary(ref left)
            ()
        | Error(_) | Leaf(_) -> ()
        | TokenTree(currOp,left,right,currRepr) ->  
            if (currOp :?> Operator).hasUnary then
                match left with
                | Ltree (leftOp,leftArg,leftRep)-> 
                    if not (leftOp :?> Operator).hasUnary then
                        ///There is an error in the postfix tree that must be resolved.
                        ///A new tree is created with the left child's operator as the 
                        ///current value, the current without the left child as the right
                        ///node, and the childs right node as the right
                        let unaryTree = Ltree(currOp, right,"")
                        let reCons = TokenTree(leftOp,leftArg,unaryTree,leftRep+currRepr)
                        curr := reCons
                        match !curr with
                        | TokenTree(cCurr, cLeft, cRight, cRep) ->
                            fixForUnary(ref cLeft)
                            fixForUnary(ref cRight)
                            ()
                        | _ -> ()
                | _ -> ()
            else 
                fixForUnary(ref left)
                fixForUnary(ref right)
                ()

    ///Computes the arthmethic result of a given tree of operators and operands.
    let rec _evaluate (curr:TokenTree) =
        match curr with
        | TokenTree(value:Token, leftTree:TokenTree, rightTree:TokenTree, repr:string) ->
            let treeResult = resultWrapper()
            let compWorker = new BackgroundWorker()
            compWorker.WorkerSupportsCancellation <- true
            compWorker.DoWork.Add(fun args ->
                let casted_op:Operator = value :?> Operator
                let resultOperand = casted_op.expression(_evaluate(leftTree), _evaluate(rightTree))
                let resStringRep = resultOperand.toString()
                //repository.querySubmit(repr, resStringRep)
                treeResult.result <- resultOperand
                )

            let lookup = async{
                let lookupResults = []//repository.queryLookup(repr)
                if lookupResults.Length > 0 then
                    let resultOperand = new Operand(lookupResults.First())
                    treeResult.result <- resultOperand
            }

            let answered = async{
                while not treeResult.isAnswered do
                    do! Async.Sleep(1)
                ()
            }
            
            Async.Start(lookup)
            compWorker.RunWorkerAsync()
            Async.RunSynchronously(answered)
            compWorker.CancelAsync()

            treeResult.result
        | Ltree(value:Token, leftTree:TokenTree, _) ->
            ///The current node is a unary operator, performs its unary operation on the
            ///child tree
            let casted_op:Operator = value :?> Operator
            casted_op.unary(_evaluate(leftTree))
        | Leaf(value:Token, _) ->
            ///The current node is an operand, it is cast as such and returned.
            let casted_val = value :?> Operand
            casted_val
        ///An error is stored in the tree, a zero value is returned.
        | Error(errorMessage:string) -> Operand(BigInteger(0),0)

    ///The tokenized, unevaluated postfix expression
    member this.tokens:List<Token> = tokens

    ///Constructs a representing string from the list of tokens
    member this.repr:string =
        let reprCons = StringBuilder()
        for i in tokens do reprCons.Append(i.toString()) |> ignore
        reprCons.ToString()

    //Returns a string representation of the answer
    member this.expressionTree:TokenTree =
        ///Forest used to bind leafs to operators
        let mutable treeStack:List<TokenTree> = []
        ///Copy of the list to be split and joined to forest
        let mutable _tokens:List<Token> = this.tokens
        let mutable errorFlag = false       //Will be raised if expression invalid.
        let mutable errorMessage:string = "invalid expression line."

        ///All the tokens are added to the tree stack (an error stops)
        while _tokens <> [] && not errorFlag do
            let current = _tokens.Head      ///Used to type check
            _tokens <- _tokens |> List.tail
            if current :? Operand then
                ///The token is a value, which will be added as a new tree
                let leafAdding:TokenTree = Leaf(current, current.toString())
                treeStack <- leafAdding::treeStack

            elif current :? Operator then
                ///The token is an operator, it must be checked if trees can be made
                ///given the stack length, and, if necissary, a unary is allowed
                if treeStack.Length = 1 then
                    ///A unary is asked to be performed, creating a left tree, it is checked
                    ///if the current operator supports unary before creating

                    ///Used to check for specific attributes
                    let curr_op:Operator = current :?> Operator
                    //if curr_op.hasUnary then
                    if true then
                        ///A unary operation is supported, the first tree is popped from the
                        ///tree stack to create the left tree, which is then added to stack
                        let leftLeaf = treeStack.Head
                        treeStack <- treeStack |> List.tail
                        let ltreeCreating = Ltree(current, leftLeaf, treeRep(leftLeaf)+current.toString())
                        treeStack <- ltreeCreating::treeStack
                    else
                        ///Unary operations are not supported, raise the error flag
                        errorFlag <- true
                        errorMessage <- sprintf "The operation \"%s\" does not support unary operations" (current.toString())
                elif treeStack.Length >= 2 then
                    ///A binary expression is asked to be perfromed, creating a complete
                    ///tree.
                    let rightLeaf = treeStack.Head
                    treeStack <- treeStack |> List.tail
                    let leftLeaf = treeStack.Head
                    treeStack <- treeStack |> List.tail
                    let treeCreating:TokenTree = TokenTree(current, leftLeaf, rightLeaf, 
                                                    treeRep(leftLeaf)
                                                    + treeRep(rightLeaf)
                                                    + current.toString())
                    treeStack <- treeCreating::treeStack
                else
                    ///There are no proceeding values to be operated on, error flag raised
                    errorFlag <- true
                    errorMessage <- sprintf "The operation \"%s\" must have an operand to operate on" (current.toString())
            else
                ///An invalid token has been read
                errorFlag <- true
                errorMessage <- sprintf "The entered expression \"%s\" is not a valid value" (current.toString())

        if treeStack.Length <> 1 then
            ///The produced tree is invalid
            errorFlag <- true
            errorMessage <- sprintf "Invalid ratio of operands to operators"

        if not errorFlag then
            ///The produced tree is valid, return the head as the result
            let balanced = ref treeStack.Head
            fixForUnary(balanced)
            !balanced
        else
            Error(errorMessage)

    ///String representation of the evaluated expression, supports errors.
    member this.resultString():string =
        match this.expressionTree with
        | Error(errorMessage) -> errorMessage
        | TokenTree(_) | Ltree(_) | Leaf(_) -> 
            _evaluate(this.expressionTree).toString()
