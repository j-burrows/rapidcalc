(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Filename:   HomeViewModel.fs
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    Provide the class which will be used as reference to the data showed in 
 |              the application.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Author:     Jonathan Burrows
 |  Date:       May 31st, 2013
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
namespace RapidCalc.ViewModels

open RapidCalc.Models
open System.ComponentModel

type HomeViewModel() =
    inherit ViewModelBase()
  
    let mutable _infixExpressionLine:string = ""
    let mutable _previousSolution = _infixExpressionLine
    let mutable _displayedDecimalPlaces = 8
    let infixExpression = InfixExpression(_infixExpressionLine)
    let mutable _solution = "Output"

    member this.infixExpressionLine
        with get() = _infixExpressionLine
        and set(updatedExpression) = 
            _infixExpressionLine <- updatedExpression
            this.OnPropertyChanged "infixExpressionLine"
            infixExpression.repr <- _infixExpressionLine
            

    member this.solution
        with get() = 
            ///Any decimal places exceeding the set display value are truncated.
            let parts = _solution.Split([|'.'|])
            if parts.Length > 1 && parts.[1].Length > _displayedDecimalPlaces - 1 then
                ///Decimal places are required to be removed
                parts.[0] + "." + parts.[1].[..(_displayedDecimalPlaces - 1)]
            else
                _solution
        and set(solution) = _solution <- solution

    member this.displayedDecimalPlaces
        with get() = _displayedDecimalPlaces
        and set(decPlaces) = 
            _displayedDecimalPlaces <- decPlaces
            this.OnPropertyChanged "displayedDecimalPlaces"
            this.OnPropertyChanged "solution"
    
    member this.previousSolution
        with get() = _previousSolution
        and set(previousSolution) = _previousSolution <- previousSolution

    member this.concatButtonText =
        new RelayCommand ((fun canExecute -> true),
            (fun value -> this.infixExpressionLine <- 
                            _infixExpressionLine + (value :?> string)))

    member this.concatPrevAnswer =
        new RelayCommand((fun canExecute -> true),
            (fun value -> this.infixExpressionLine <-
                            _infixExpressionLine + _previousSolution))

    member this.displayAnswer = 
        new RelayCommand((fun canExecute -> true),
            (fun value ->
                _solution <- infixExpression.postfix.resultString()
                this.OnPropertyChanged "solution"
                _previousSolution <- _solution
                _infixExpressionLine <- ""
                this.OnPropertyChanged "infixExpressionLine"))