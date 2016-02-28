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
namespace Test
{
  using System;

  using Noson;

  static class Program
  {
    static void Main(string[] args)
    {
      var input = @"{""d"":[1,2,3]}";
      var json  = Json.EmptyJson;
      var pos   = 0;
      var msg   = "";

      if (Json.TryParse (input, true, out pos, out json, out msg))
      {
        var s = Json.ToString (json);

        Console.WriteLine ("Json: {0}", s);

        dynamic a = json;
        dynamic d = a.d;
        var c = d.GetCount ();
        for (var iter = 0; iter < c; ++iter)
        {
          string k  = d.GetKey (iter);
          dynamic v = d[iter];
          bool b    = v;
          Console.WriteLine ("{0}, {1}, {2}", k, v, b ? "true" : "false");
        }
      }
      else
      {
        Console.WriteLine ("Failed to parse '{0}' because: {1}", input, msg);
      }
    }
  }
}
