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
module Noson.Test.TestCases

open System

let positiveTestCases =
  [|
    """[0]"""
    """[-0]"""
    """[0.125]"""
    """[-0.125]"""
    """[0e2]"""
    """[-0E2]"""
    """[0.125E3]"""
    """[-0.125E2]"""
    """[125]"""
    """[-125]"""
    """[1.25]"""
    """[-12.5]"""
    """[1.25E2]"""
    """[-12.5E-2]"""
    """[-12.5e+2]"""
    """[null]"""
    """[true]"""
    """[false]"""
    """["Hello\r\nThere"]"""
    """["Hello\u004a"]"""
    """["\"\\\/\b\f\n\r\t\u2665"]"""
    """["\u0123\u4567\u89AB\uCDEF"]"""
    """["\u0123\u4567\u89ab\ucdef"]"""
    """[false,true,null]"""
    """ [ false ,true, null ] """
    """[[], null, [true]]"""
    """{"abc":123}"""
    """{"abc" :123}"""
    """{ "abc":123}"""
    """{ "abc" :123}"""
    "\t[\rfalse \r\n, true\n]\t"
    "[\"" + String.Join ("", Array.init 32 id |> Array.map (fun v -> sprintf "\u%04X" v)) + "\"]"
    """[1E-324]"""
    """[1E+309]"""
    """[-1E309]"""
  |]

let negativeTestCases =
  [|
    """null"""
    """true"""
    """false"""
    """0"""
    """-0"""
    """125"""
    """-125"""
    "\"Hello\""
    """[01]"""
    """[-01]"""
    """[0125]"""
    """[+0]"""
    """[+125]"""
    """[.1]"""
    """[-.0]"""
    """[1.]"""
    """[1E]"""
    """[1E+]"""
    """[1E-]"""
    """["Hello]"""
    """["Hello\xThere"]"""
    """["Hello\u"]"""
    """["Hello\u00"]"""
    """["Hello\uPQ"]"""
    """{abc:3}"""
    """{"abc:3}"""
    """{"abc":}"""
    """["Hello this is a somewhat wide errornous string, to demonstrate \ERROR window"]"""
    """["Hello this is a wide errornous string, to demonstrate error window capabilities... The window is around 60 chars so now is the time for \ERROR"]"""
    """["Hello this is a wide errornous string, to demonstrate error window capabilities, here is the \ERROR... The window is around 60 chars"]"""
    """["Early \ERROR, hello this is a wide errornous string, to demonstrate error window capabilities. The window is around 60 chars"]"""
    """[fals"""
    """[t"""
    """["Hello\u"""
    """"""
    """[] []"""
    """["
"]"""
    """[-"""
    """[1.0"""
    """[nul]"""
  |]
