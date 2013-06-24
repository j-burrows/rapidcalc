(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Filename:   SqlExpressionRepository.fs
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    Provide a class which implements the repository interface for sql databases.
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
open System.Data.SqlClient
open System.Data.Linq
open System.Data.Linq.Mapping
open System.ComponentModel.DataAnnotations

type SqlExpressionRepository(?connString:string) =
    ///Checks if a connection string was supplied, if not sets to default. 
    let _connString = 
            match connString with
            | Some value -> value
            | None -> @"Server='.\SQLEXPRESS'; Database=RAPIDCALC2;Integrated Security=SSPI;MultipleActiveResultSets=True; Asynchronous Processing=true"
   
    let expressionsTable:Table<Expression> =
        ///Returns the table of expressions stored in the database.
        use conn = new SqlConnection(_connString)
        conn.Open() 
        (new DataContext(_connString)).GetTable<Expression>()

    ///Required workaround for F# to implement interface members
    member this.expressions = (this :> IExpressionRepository).expressions
    member this.saveExpr(toSave:Expression) = (this :> IExpressionRepository).saveExpr(toSave)
    member this.removeExpr(toRemove:Expression) = (this :> IExpressionRepository).removeExpr(toRemove)
    member this.querySubmit(repr:string, result:string) = 
        (this :> IExpressionRepository).querySubmit(repr,result)
    member this.queryLookup(repr:string) =
        (this :> IExpressionRepository).queryLookup(repr)

    interface IExpressionRepository with
        ///Expression table from the database
        member this.expressions
            ///Returns the table cast as a querable object
            with get() =
                expressionsTable :> IQueryable<Expression>
        member this.saveExpr(toSave:Expression) =
            ///Checks if already in database
            if toSave.expressionID = 0 then
                ///This is a new entry to be added to the database
                expressionsTable.InsertOnSubmit(toSave)
            match box expressionsTable.GetOriginalEntityState with
            | null -> 
                ///This is an already made entry to be edited (unlikely to be used), 
                ///the first element with the given id is found and returned
                let mutable updating:Expression 
                    = expressionsTable.FirstOrDefault(fun (x:Expression) -> 
                                                     x.expressionID = toSave.expressionID )
                ///The associated expression class is edited to the above changes.
                updating.repr <- toSave.repr
                updating.result <- toSave.result
            | _ -> 1 |> ignore
         
            expressionsTable.Context.SubmitChanges(ConflictMode.FailOnFirstConflict)


        member this.removeExpr(toRemove:Expression) =
            ///Removes the element from the database (unlikely to be used)
            expressionsTable.DeleteOnSubmit(toRemove)
            expressionsTable.Context.SubmitChanges()

        member this.querySubmit(repr, result) =
            ///Trunaces the given strings if too large.
            let _result = if result.Length > 1023 
                            then result.[..1023]
                          else result
            let _repr = if repr.Length > 1023
                            then repr.[..1023]
                        else repr         
            let query = "INSERT INTO Expressions (repr, result) VALUES ('" + _repr
                        + "','" + _result + "')" 
            use conn = new SqlConnection(_connString)
            conn.Open() 
            use comm = new SqlCommand(query, conn)
            comm.BeginExecuteNonQuery() |> ignore
            conn.Close()

        member this.queryLookup(repr)=
            let lookup() = 
                seq{
                    let query = "SELECT result FROM Expressions WHERE result='" + repr + "'"
                    use conn = new SqlConnection(_connString)
                    conn.Open()
                    use comm = new SqlCommand(query,conn)
                    use reader = comm.ExecuteReader()
                    while reader.Read() do
                        yield (reader.GetString 0)
                }
            let m = lookup()
            m.ToArray()