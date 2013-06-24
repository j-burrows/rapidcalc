(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Filename:   Expression.fs
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    Provide a class which may be used by linq to perform queries on the db.
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Author:     Jonathan Burrows
 |  Date:       May 31st, 2013
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
namespace RapidCalc.Repository

open System
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Text
open System.Data
open System.Data.Linq
open System.Data.Linq.Mapping
open System.ComponentModel.DataAnnotations

[<Table(Name="Expressions")>]
type Expression() =
    [<Column(IsPrimaryKey=true, IsDbGenerated=true, AutoSync=AutoSync.OnInsert)>]
    [<DefaultValue>]
    val mutable expressionID:int

    ///Represents an infix expression line
    [<Column>][<Required>][<DefaultValue>]
    val mutable repr:string

    ///Represents the solution solution to the above infix expression
    [<Column>][<Required>][<DefaultValue>]
    val mutable result:string

    //Overload method that allows for setting of variables in initialisation, required to
    //be overloaded for Table<Expression> types
    new (repr:string, result:string) as this =
        new Expression() then
        this.repr <- repr
        this.result <- result
