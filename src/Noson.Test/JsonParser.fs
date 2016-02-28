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

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
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

[<RequireQualifiedAccess>]
type Whitespace =
  | B
  | F
  | N
  | R
  | T
  | S

type Whitespaces = (Whitespace [])*(Whitespace [])

[<RequireQualifiedAccess>]
type WhitespacedJsonValue =
  | Null    of Whitespaces
  | Bool    of Whitespaces*bool
  | Number  of Whitespaces*float
  | String  of Whitespaces*string
  | Array   of Whitespaces*(WhitespacedJsonValue [])
  | Object  of Whitespaces*((Whitespaces*string*WhitespacedJsonValue) [])

// Primarily intended to be used with FsCheck to generate Json Documents
[<RequireQualifiedAccess>]
type WhitespacedJson =
  | Array   of Whitespaces*(WhitespacedJsonValue [])
  | Object  of Whitespaces*((Whitespaces*string*WhitespacedJsonValue) [])

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

  let setMemberKey (v : string) =
    context.Count > 0 |> Debug.Assert
    let rkey, _, _ = context.Peek ()
    rkey := v
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
    let f v =
      v
      |> Seq.filter (fun (p, _) -> p = pos)
      |> Seq.map snd
      |> Seq.sort
      |> Seq.distinct
      |> Seq.toArray
    let e = f expected
    let u = f unexpected
    e, u

  interface IJsonVisitor with
    member x.NullValue    ()                  : bool = Json.Null |> add
    member x.BoolValue    v                   : bool = Json.Bool v |> add
    member x.NumberValue  v                   : bool = Json.Number v |> add
    member x.StringValue  (v : StringBuilder) : bool = Json.String (v.ToString ()) |> add
    member x.StringValue  (v : string)        : bool = Json.String v |> add
    member x.ArrayBegin   ()                  : bool = pushArray ()
    member x.ArrayEnd     ()                  : bool = pop ()
    member x.ObjectBegin  ()                  : bool = pushObject ()
    member x.ObjectEnd    ()                  : bool = pop ()
    member x.MemberKey    (v : StringBuilder) : bool = setMemberKey (v.ToString ())
    member x.MemberKey    (v : string)        : bool = setMemberKey v
    member x.ExpectedChar (pos, e)            : unit = expected.Add (pos, sprintf "'%c'" e)
    member x.Expected     (pos, e)            : unit = expected.Add (pos, e)
    member x.Unexpected   (pos, u)            : unit = unexpected.Add (pos, u)

type ParseJsonResult =
  | Success of Json
  | Failure of int*string*string []*string []

let NonPrintableChars =
  [|
    for i in 0..31 ->
      match char i with
      | '\b'            -> @"\b"
      | '\f'            -> @"\f"
      | '\n'            -> @"\n"
      | '\r'            -> @"\r"
      | '\t'            -> @"\t"
      | ch              -> sprintf "\u%04X" i
  |]

// FsCheck generates null string values
let Coerce (s : string) : string = if s = null then "" else s

let ToJsonValue = function
  | WhitespacedJson.Array (ws, vs)   -> WhitespacedJsonValue.Array (ws, vs)
  | WhitespacedJson.Object (ws, vs)  -> WhitespacedJsonValue.Object (ws, vs)

let ToString (json : WhitespacedJson) : string =
  let sb = StringBuilder ()

  let ch (c : char) : unit =
    sb.Append c |> ignore

  let ws (vs : Whitespace []) : unit =
    for v in vs do
      match v with
      | Whitespace.B -> ch '\b'
      | Whitespace.F -> ch '\f'
      | Whitespace.N -> ch '\n'
      | Whitespace.R -> ch '\r'
      | Whitespace.T -> ch '\t'
      | Whitespace.S -> ch ' '

  let str (s : string) =
    sb.Append s |> ignore

  let estr ((pre, post) : Whitespaces) (s : string) =
    ws pre
    ch '"'
    let s = Coerce s
    let e = s.Length - 1
    for i = 0 to e do
      match s.[i] with
      | '\"'            -> str @"\"""
      | '\\'            -> str @"\\"
      | '/'             -> str @"\/"
      | c when c < ' '  -> str NonPrintableChars.[int c]
      | c               -> ch c
    ch '"'
    ws post

  let wstr ((pre, post) : Whitespaces) (s : string) =
    ws pre
    str s
    ws post

  let rec loop = function
    | WhitespacedJsonValue.Null    ws                -> wstr ws "null"
    | WhitespacedJsonValue.Bool    (ws, true)        -> wstr ws "true"
    | WhitespacedJsonValue.Bool    (ws, false)       -> wstr ws "false"
    | WhitespacedJsonValue.Number  (ws, v)           ->
      if Double.IsInfinity v              then wstr ws @"""Infinity"""
      else if Double.IsNegativeInfinity v then wstr ws @"""-Infinity"""
      else if Double.IsNaN v              then wstr ws @"""NaN"""
      else wstr ws (v.ToString ("g", CultureInfo.InvariantCulture))
    | WhitespacedJsonValue.String  (ws, v)           -> estr ws v
    | WhitespacedJsonValue.Array   ((pre, post), vs) ->
      ws pre
      ch '['
      let mutable prelude = ""
      for v in vs do
        str prelude
        prelude <- ", "
        loop v
      ch ']'
      ws post
    | WhitespacedJsonValue.Object  ((pre, post), vs)          ->
      ws pre
      ch '{'
      let mutable prelude = ""
      for kws, k, v in vs do
        str prelude
        prelude <- ", "
        estr kws k
        ch ':'
        loop v
      ch '}'
      ws post

  let jsonValue = ToJsonValue json
  loop jsonValue

  sb.ToString ()

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
    visitor.Value |> Some
  else
    None

let rec Strip (json : WhitespacedJson) : Json =
  let rec loop = function
    | WhitespacedJsonValue.Null    _       -> Json.Null
    | WhitespacedJsonValue.Bool    (_, v)  -> Json.Bool v
    | WhitespacedJsonValue.Number  (_, v)  ->
      // Not supported by Json
      if Double.IsInfinity v              then Json.String @"Infinity"
      else if Double.IsNegativeInfinity v then Json.String @"-Infinity"
      else if Double.IsNaN v              then Json.String @"NaN"
      else Json.Number v
    | WhitespacedJsonValue.String  (_, v)  -> Json.String (Coerce v)
    | WhitespacedJsonValue.Array   (_, vs) ->
      [| for v in vs -> loop v |] |> Json.Array
    | WhitespacedJsonValue.Object  (_, vs) ->
      [| for (_, k, v) in vs -> Coerce k, loop v |] |> Json.Object
  let jsonValue = ToJsonValue json
  loop jsonValue
