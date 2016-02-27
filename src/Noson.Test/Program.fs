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
open System.Collections.Generic
open System.Diagnostics
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
  let context         = Stack<(Json->unit)*(unit->Json)> ()
  let mutable key     = ""

  let push (a, d)     =
    context.Push (a, d)
    true
  let pop ()          =
    context.Count > 1 |> Debug.Assert
    let _, d  = context.Pop ()
    let j     = d ()
    let a, _  = context.Peek ()
    a j
    true
  let add j           =
    context.Count > 0 |> Debug.Assert
    let a, _  = context.Peek ()
    a j
    true

  interface IJsonParseVisitor with
    member x.NullValue    ()        : bool = Json.Null |> add
    member x.BoolValue    v         : bool = Json.Bool v |> add
    member x.NumberValue  v         : bool = Json.Number v |> add
    member x.StringValue  v         : bool = Json.String (v.ToString ()) |> add
    member x.ArrayBegin   ()        : bool =
      let ab    = ResizeArray<Json> ()
      let a j   = ab.Add j
      let d ()  = ab.ToArray () |> Json.Array
      push (a, d)
    member x.ArrayEnd     ()        : bool = pop ()
    member x.ObjectBegin  ()        : bool =
      let ab    = ResizeArray<string*Json> ()
      let a j   = ab.Add (key, j)
      let d ()  = ab.ToArray () |> Json.Object
      push (a, d)
    member x.ObjectEnd    ()        : bool = pop ()
    member x.MemberKey    v         : bool =
      key = "" |> Debug.Assert
      key <- v.ToString ()
      true
    member x.ExpectedChar (pos, e)  : unit =
      ()
    member x.Expected     (pos, e)  : unit =
      ()
    member x.Unexpected   (pos, u)  : unit =
      ()

[<EntryPoint>]
let main argv =
  0
