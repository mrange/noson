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
open Noson.Test
open Noson.Test.Common
open Noson.Test.JsonParser

let testPositiveTestCases () =
  highlight "testPositiveTestCases"

  for json in TestCases.positiveTestCases do
    let expected  = ReferenceParser.ParseJson json
    let actual    = ParseJson json
    match expected, actual with
    | Success e, Success a when e = a -> 
      ()
    | _ , Success _ -> 
      errorf "Expected and actual parse result doesn't match for %A: %A <> %A" json expected actual
    | _ , Failure (p, _, e, u) ->
      errorf "Failed parsing %A: Pos: %d, Expected: %A, Unexpected: %A" json p e u

let testNegativeTestCases () =
  highlight "testNegativeTestCases"

  for json in TestCases.negativeTestCases do
    let result = ParseJson json
    match result with
    | Success _ -> 
      errorf "Expected parse failure for: %A" json
    | Failure (p, _, e, u) ->
      successf "Pos: %d, Expected: %A, Unexpected: %A" p e u
      ()

[<EntryPoint>]
let main argv =
  try
    testPositiveTestCases ()
    testNegativeTestCases ()

    if errors = 0 then
      success "All tests completed successfully"
    else
      errorf "%d tests failed" errors

    0
  with
  | e -> 
    errorf "Caught exception: %s" e.Message
    999
  
