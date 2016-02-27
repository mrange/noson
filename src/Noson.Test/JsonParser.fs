// ----------------------------------------------------------------------------------------------
// Copyright 2015 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------
module Noson.Test.JsonParser

open System.Collections.Generic
open System.Diagnostics
open System.Text

open Noson

[<RequireQualifiedAccess>]
type Json =
  | Null
  | Bool    of bool
  | Number  of float
  | String  of string
  | Array   of Json []
  | Object  of (string*Json) []

type JsonBuilderVisitor() =
  let expected        = ResizeArray<int*string> ()
  let unexpected      = ResizeArray<int*string> ()
  let context         = Stack<string ref*(Json->unit)*(unit->Json)> ()
  let dummy           = ref ""

  let push (rk, a, d)     =
    context.Push (rk, a, d)
    true
  let pop ()          =
    context.Count > 1 |> Debug.Assert
    let _, _, d  = context.Pop ()
    let j     = d ()
    let _, a, _  = context.Peek ()
    a j
    true
  let add j           =
    context.Count > 0 |> Debug.Assert
    let _, a, _  = context.Peek ()
    a j
    true

  let pushArray () =
      let ab    = ResizeArray<Json> ()
      let a j   = ab.Add j
      let d ()  = ab.ToArray () |> Json.Array
      push (dummy, a, d)

  let pushObject () =
    let rk    = ref ""
    let ab    = ResizeArray<string*Json> ()
    let a j   = ab.Add (!rk, j)
    let d ()  = ab.ToArray () |> Json.Object
    push (rk, a, d)

  let setMemberKey (v : StringBuilder) =
    context.Count > 0 |> Debug.Assert
    let rkey, _, _ = context.Peek ()
    rkey := v.ToString ()
    true

  do
    let rroot = ref Json.Null
    let a j   = rroot := j
    let d ()  = !rroot
    push (dummy, a, d) |> ignore

  member x.Value : Json =
    context.Count = 1 |> Debug.Assert 
    let _, _, d = context.Peek ()
    d ()

  member x.Errors (pos : int) : string [] * string [] =
    let f v = v |> Seq.filter (fun (p, _) -> p = pos) |> Seq.map snd |> Seq.sort |> Seq.distinct |> Seq.toArray
    let e = f expected
    let u = f unexpected
    e, u

  interface IJsonParseVisitor with
    member x.NullValue    ()        : bool = Json.Null |> add
    member x.BoolValue    v         : bool = Json.Bool v |> add
    member x.NumberValue  v         : bool = Json.Number v |> add
    member x.StringValue  v         : bool = Json.String (v.ToString ()) |> add
    member x.ArrayBegin   ()        : bool = pushArray ()
    member x.ArrayEnd     ()        : bool = pop ()
    member x.ObjectBegin  ()        : bool = pushObject ()
    member x.ObjectEnd    ()        : bool = pop ()
    member x.MemberKey    v         : bool = setMemberKey v
    member x.ExpectedChar (pos, e)  : unit = expected.Add (pos, sprintf "'%c'" e)
    member x.Expected     (pos, e)  : unit = expected.Add (pos, e)
    member x.Unexpected   (pos, u)  : unit = unexpected.Add (pos, u)

type ParseJsonResult =
  | Success of Json
  | Failure of int*string*string []*string []

let ParseJson (json : string) : ParseJsonResult =
  let visitor = JsonBuilderVisitor ()
  let parser  = JsonParser (json, visitor)
  if parser.TryParse () then
    visitor.Value |> Success
  else
    let p     = parser.Position
    let e, u  = visitor.Errors p
    Failure (p, "Failed to parse JSON", e, u)

let Roundtrip (json : string) : string option =
  let visitor = JsonWriter false
  let parser  = JsonParser (json, visitor)
  if parser.TryParse () then
    visitor.Json |> Some
  else
    None
  
