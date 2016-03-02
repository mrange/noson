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
open System.IO
open System.Reflection

open FsCheck

open Noson
open Noson.Test
open Noson.Test.Common
open Noson.Test.JsonParser

let testPositiveTestCases () =
  highlight "testPositiveTestCases"

  let allPositiveTestCases = Array.append TestCases.positiveTestCases TestCases.embeddedPositiveTestCases.Value

  for name, json in TestCases.positiveTestCases do
    let expected  = ReferenceParser.ParseJson json
    let actual    = ParseJson json
    let roundtrip = Roundtrip json
    match expected, actual, roundtrip with
    | Success e, Success a, Some rt when e = a ->
      match ReferenceParser.ParseJson rt with
      | Success prt when e = prt ->
        ()
      | _ ->
        errorf "Expected and roundtrip result doesn't match for %A" name
    | _ , Success _, _ ->
      errorf "Expected and actual parse result doesn't match for %A" name
    | _ , Failure (p, _, e, u), _ ->
      errorf "Failed parsing for %A: Pos: %d, Expected: %A, Unexpected: %A" name p e u

let testNegativeTestCases () =
  highlight "testNegativeTestCases"

  for name, json in TestCases.negativeTestCases do
    let result = ParseJson json
    match result with
    | Success _ ->
      errorf "Expected parse failure for: %A" json
    | Failure (p, _, e, u) ->
      ()

type ParseProperties() =
  static let precheck (wj : WhitespacedJson) =
    true

  static member ``can parse json document`` wj =
    precheck wj ==> fun () ->
      let json      = ToString wj
      let expected  = ToJson wj
      match ParseJson json with
      | Success actual ->
        let result = expected = actual
        if not result then
          errorf "Failed equality property %A: %A <> %A" json expected actual
        result
      | Failure (p, _, e, u) ->
        errorf "Failed parsing %A: Pos: %d, Expected: %A, Unexpected: %A" json p e u
        false


let testParseProperties () =
  highlight "testParseProperties"

  let runner =
    { new IRunner with
      member __.OnStartFixture t =
        highlight (Runner.onStartFixtureToString t)
      member __.OnArguments (ntest, args, every) =
        // info (every ntest args)
        ()
      member __.OnShrink(args, everyShrink) =
        // warning (everyShrink args)
        ()
      member __.OnFinished(name,testResult) =
        let isTrue = match testResult with | TestResult.True _ -> true | _ -> false
        if isTrue then
          success (Runner.onFinishedToString name testResult)
        else
          error (Runner.onFinishedToString name testResult)
    }

  let config =
    {
      Config.Quick with
        MaxTest = 1000
        MaxFail = 10000
        Runner  = runner
    }

  Check.All<ParseProperties> config


[<EntryPoint>]
let main argv =
  try
    testPositiveTestCases ()
    testNegativeTestCases ()
    testParseProperties   ()

    if errors = 0 then
      success "All tests completed successfully"
    else
      errorf "%d tests failed" errors

    0
  with
  | e ->
    errorf "Caught exception: %s" e.Message
    999

