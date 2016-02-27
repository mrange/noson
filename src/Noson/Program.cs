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
  using System.Text;

  using Noson;
  using System.Runtime.CompilerServices;
  using System.Collections.Generic;
  using System.Dynamic;
  class ConsoleVisitor : IJsonParseVisitor
  {
    void Visited ([CallerMemberName] string caller = null)
    {
      Console.WriteLine ("Visited: {0}", caller);
    }

    void Visited (object v0, [CallerMemberName] string caller = null)
    {
      Console.WriteLine ("Visited: {0} - {1}", caller, v0);
    }

    void Visited (object v0, object v1, [CallerMemberName] string caller = null)
    {
      Console.WriteLine ("Visited: {0} - {1} - {2}", caller, v0, v1);
    }

    public bool ArrayBegin()
    {
      Visited ();
      return true;
    }

    public bool ArrayEnd()
    {
      Visited ();
      return true;
    }

    public bool BoolValue(bool v)
    {
      Visited ((object)v);
      return true;
    }

    public void Expected(int pos, string e)
    {
      Visited ((object)pos, (object)e);
    }

    public void ExpectedChar(int pos, char e)
    {
      Visited ((object)pos, (object)e);
    }

    public bool MemberKey(StringBuilder v)
    {
      Visited ((object)v);
      return true;
    }

    public bool NullValue()
    {
      Visited ();
      return true;
    }

    public bool NumberValue(double v)
    {
      Visited ((object)v);
      return true;
    }

    public bool ObjectBegin()
    {
      Visited ();
      return true;
    }

    public bool ObjectEnd()
    {
      Visited ();
      return true;
    }

    public bool StringValue(StringBuilder v)
    {
      Visited ((object)v);
      return true;
    }

    public void Unexpected(int pos, string u)
    {
      Visited ((object)pos, (object)u);
    }
  }

  class Program
  {
    static void Test (string json)
    {
      var v = new ConsoleVisitor ();
      var p = new JsonParser (json, v);

      Console.WriteLine ("ParseResult: {0}", p.TryParse ());
    }

    void TestCases ()
    {
      Test (@"[""Hello: \u0041""]");
      Test (@"[12.324]");
      Test (@"[12.324E+3]");
      Test (@"[12.324e-2]");
      Test (@"[0.123]");
      Test (@"[-0.123]");
      Test (@"[""Hello: \\\""\/b\f\n\r\t""]");
      Test (@"null");
      Test (@"{""Yello"":12, ""GG"":""Test""}  ");
      Test (@"[""Hello""]");
      Test (@"[-1]");
      Test (@"[12]");
      Test (@"[12.324]");
      Test (@"[0123]");
      Test (@"[true]");
      Test (@"[null]");
      Test (@"[false]");
      Test (@"[0]");
      Test (@"[]");
      Test (@"[null]");
      Test (@"[null,true]");
      Test (@"[null,true,0]");
      Test (@"[null,true,12]");
      Test (@"[null,true");
      Test (@"[gg]");
    }

    static void Main(string[] args)
    {
      dynamic d = new ExpandoObject ();
      IDictionary<string, object> e = d;
      e["a"] = 1;
      e["b"] = 2;

      var x = d.a;
      var y = d.b;

      Console.WriteLine ("{0}, {1}", x, y);
    }
  }
}
