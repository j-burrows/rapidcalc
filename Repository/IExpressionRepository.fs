(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Filename:   IExpressionRepository.fs
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    An abstract repository to decouple the sql repository for debugging.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Author:     Jonathan Burrows
 |  Date:       May 31st, 2013
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
namespace RapidCalc.Repository

open System
open System.Linq

type IExpressionRepository =
    ///Will contain all the expressions in the repository
    abstract member expressions:IQueryable<Expression>
    ///Adds or edits an expression to the repository
    abstract member saveExpr: Expression -> unit
    ///Removes an expression from the repository
    abstract member removeExpr: Expression -> unit
    ///Submits an expression and result via query
    abstract member querySubmit: String * String -> unit
    ///Submits a query lookup
    abstract member queryLookup: String -> string[]