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
namespace Noson.Details
{
  using System;
  using System.Globalization;
  using System.Linq;
  using System.Runtime.CompilerServices;

  static partial class Common
  {
    public const int    DefaultSize               = 64            ;

    public const string ErrorPrelude              = "Failed to parse input as JSON";

    public const string Null                      = "null"        ;
    public const string True                      = "true"        ;
    public const string False                     = "false"       ;
    public const string Char                      = "char"        ;
    public const string Digit                     = "digit"       ;
    public const string HexDigit                  = "hexdigit"    ;
    public const string EOS                       = "EOS"         ;
    public const string NewLine                   = "NEWLINE"     ;
    public const string Escapes                   = "\"\\/bfnrtu" ;
    public const string ValuePreludes             = "\"{[-"       ;
    public const string RootValuePreludes         = "{["          ;

    public const int    MinimumPow10              = -323          ; // Min Exponent is -1022 (binary) but since double supports subnormals effective is even lower
    public const int    MaximumPow10              = 308           ; // 1023 (binary)

    static string [] CreateNonPrintableChars ()
    {
      return Enumerable
        .Range (0, 32)
        .Select (i =>
          {
            switch (i)
            {
              case '\b':  return @"\b";
              case '\f':  return @"\f";
              case '\n':  return @"\n";
              case '\r':  return @"\r";
              case '\t':  return @"\t";
              default:    return @"\u{0:X4}".FormatWith (i);
            }
          })
        .ToArray ()
        ;
    }

    static double [] CreatePow10Table ()
    {
      return Enumerable
        .Range (MinimumPow10, MaximumPow10 - MinimumPow10 + 1)
        .Select (i => Math.Pow (10.0, i))
        .ToArray ()
        ;
    }

    public static readonly string [] NonPrintableChars   = CreateNonPrintableChars ();
    public static readonly double [] Pow10Table          = CreatePow10Table ();

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static double Pow10 (int e)
    {
      if (e < MinimumPow10)
      {
        return 0.0;
      }
      else if (e > MaximumPow10)
      {
        return Double.PositiveInfinity;
      }
      else
      {
        return Pow10Table[e - MinimumPow10];
      }
    }


    public static string FormatWith (this string format, params object[] args)
    {
      return string.Format (CultureInfo.InvariantCulture, format ?? "", args ?? new object[0]);
    }
  }
}

namespace Noson
{
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Globalization;
  using System.Linq;
  using System.Runtime.CompilerServices;
  using System.Text;

  using Noson.Details;

  partial interface IJsonVisitor
  {
    bool NullValue    ();
    bool BoolValue    (bool             v);
    bool NumberValue  (double           v);
    bool StringValue  (StringBuilder    v);
    bool StringValue  (string           v);
    bool ArrayBegin   ();
    bool ArrayEnd     ();
    bool ObjectBegin  ();
    bool ObjectEnd    ();
    bool MemberKey    (StringBuilder    v);
    bool MemberKey    (string           v);
    void ExpectedChar (int pos, char    e);
    void Expected     (int pos, string  e);
    void Unexpected   (int pos, string  u);
  }

  partial interface IJsonHierarchy
  {
    void Apply (IJsonVisitor v);
  }

  static partial class JsonParseVisitor
  {
    public static void ExpectedChars (this IJsonVisitor v, int pos, string e)
    {
      Debug.Assert (v != null);
      Debug.Assert (e != null);

      // TODO: This should be optimized into 1 call
      foreach (var ch in e)
      {
        v.ExpectedChar (pos, ch);
      }
    }
  }

  sealed partial class JsonParser
  {
    readonly string             input         ;
    readonly IJsonVisitor  visitor       ;

    readonly StringBuilder      stringBuilder ;

    int                         position      ;

    public JsonParser (string i, IJsonVisitor v)
    {
      input         = i ?? ""                               ;
      visitor       = v                                     ;
      stringBuilder = new StringBuilder (Common.DefaultSize);

      if (v == null)
      {
        throw new ArgumentException ("v : IParseVisitor must be not be null");
      }
    }

    public int Position
    {
      [MethodImpl (MethodImplOptions.AggressiveInlining)]
      get
      {
        return position;
      }
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool NotEos ()
    {
      // TODO: Faster to prefetch length?
      return position < input.Length;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool Eos ()
    {
      // TODO: Faster to prefetch length?
      return position >= input.Length;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    char Char ()
    {
      Debug.Assert (position < input.Length);
      return input[position];
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    void Advance ()
    {
      Debug.Assert (position < input.Length);
      ++position;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    static bool IsWhiteSpace (char c)
    {
      switch (c)
      {
        case '\b':
        case '\f':
        case '\n':
        case '\r':
        case '\t':
        case ' ':
          return true;
        default:
          return false;
      }
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    static bool IsDigit (char c)
    {
      return '0' <= c && c <= '9';
    }

    bool Raise_Eos ()
    {
      visitor.Unexpected (position, Common.EOS);
      return false;
    }

    bool Raise_Value ()
    {
      visitor.Expected      (position, Common.Null         );
      visitor.Expected      (position, Common.True         );
      visitor.Expected      (position, Common.False        );
      visitor.Expected      (position, Common.Digit        );
      visitor.ExpectedChars (position, Common.ValuePreludes);
      return false;
    }

    bool Raise_RootValue ()
    {
      visitor.ExpectedChars (position, Common.RootValuePreludes);
      return false;
    }

    bool Raise_Char ()
    {
      visitor.Expected (position, Common.Char);
      return false;
    }

    bool Raise_Digit ()
    {
      visitor.Expected (position, Common.Digit);
      return false;
    }

    bool Raise_HexDigit ()
    {
      visitor.Expected (position, Common.HexDigit);
      return false;
    }

    bool Raise_Escapes ()
    {
      visitor.ExpectedChars (position, Common.Escapes);
      return false;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool Consume_WhiteSpace ()
    {
      var l = input.Length;

      while (position < l && IsWhiteSpace (Char ()))
      {
        Advance ();
      }

      return true;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool Test_Char (char c)
    {
      return NotEos () && Char () == c;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool TryConsume_Char (char c)
    {
      if (Eos ())
      {
        visitor.ExpectedChar (position, c);
        return Raise_Eos ();
      }
      else if (Char () == c)
      {
        Advance ();
        return true;
      }
      else
      {
        visitor.ExpectedChar (position, c);
        return false;
      }
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool TryParse_AnyOf2 (char first, char second, out char r)
    {
      if (Eos ())
      {
        r =  default (char);
        visitor.ExpectedChar (position, first);
        visitor.ExpectedChar (position, second);
        return Raise_Eos ();
      }
      else
      {
        r = Char ();
        if (r == first || r == second)
        {
          Advance ();
          return true;
        }
        else
        {
          r =  default (char);
          visitor.ExpectedChar (position, first);
          visitor.ExpectedChar (position, second);
          return false;
        }
      }
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool TryConsume_Token (string tk)
    {
      Debug.Assert (tk != null);
      var tkl = tk.Length;
      if (tkl + position <= input.Length)
      {
        var start     = position;
        var index     = 0;

        while (index < tkl && tk[index] == Char ())
        {
          ++index;
          Advance ();
        }

        if (index == tkl)
        {
          return true;
        }
        else
        {
          // To support error reporting, move back on failure
          position = start;
          return false;
        }
      }
      else
      {
        return false;
      }
    }

    bool TryParse_Eos ()
    {
      if (Eos ())
      {
        return true;
      }
      else
      {
        visitor.Expected (position, Common.EOS);
        return false;
      }
    }

    bool TryParse_Null ()
    {
      if (TryConsume_Token (Common.Null))
      {
        return visitor.NullValue ();
      }
      else
      {
        return Raise_Value ();
      }
    }

    bool TryParse_True ()
    {
      if (TryConsume_Token (Common.True))
      {
        return visitor.BoolValue (true);
      }
      else
      {
        return Raise_Value ();
      }
    }

    bool TryParse_False ()
    {
      if (TryConsume_Token (Common.False))
      {
        return visitor.BoolValue (false);
      }
      else
      {
        return Raise_Value ();
      }
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool TryParse_Delimiter (bool first)
    {
      return first || (TryConsume_Char (',') && Consume_WhiteSpace ());
    }

    bool TryParse_ArrayValues ()
    {
      var first = true;

      while (!Test_Char (']'))
      {
        if (!(
              TryParse_Delimiter (first)
          &&  TryParse_Value ())
          )
        {
          return false;
        }

        first = false;
      }

      return true;
    }

    bool TryParse_Array ()
    {
      return
            TryConsume_Char ('[')
        &&  Consume_WhiteSpace ()
        &&  visitor.ArrayBegin ()
        &&  TryParse_ArrayValues ()
        &&  TryConsume_Char (']')
        &&  visitor.ArrayEnd ()
        ;
    }

    bool TryParse_ObjectValues ()
    {
      var first = true;

      while (!Test_Char ('}'))
      {
        if (!(
              TryParse_Delimiter (first)
          &&  TryParse_MemberKey ()
          &&  Consume_WhiteSpace ()
          &&  TryConsume_Char (':')
          &&  Consume_WhiteSpace ()
          &&  TryParse_Value ())
          )
        {
          return false;
        }

        first = false;
      }

      return true;
    }

    bool TryParse_Object ()
    {
      return
            TryConsume_Char ('{')
        &&  Consume_WhiteSpace ()
        &&  visitor.ObjectBegin ()
        &&  TryParse_ObjectValues ()
        &&  TryConsume_Char ('}')
        &&  visitor.ObjectEnd ()
        ;
    }

    bool TryParse_UnicodeChar ()
    {
      var r = (char)0;
      var i = 0;
      while (!Eos () && i < 4)
      {
        var c = Char ();

        if (IsDigit (c))
        {
          r = (char)((r << 4) + (c - '0'));
        }
        else if ('A' <= c && c <= 'F')
        {
          r = (char)((r << 4) + (c - 'A' + 10));
        }
        else if ('a' <= c && c <= 'f')
        {
          r = (char)((r << 4) + (c - 'a' + 10));
        }
        else
        {
          return Raise_HexDigit ();
        }

        ++i;
        Advance ();
      }

      if (i < 4)
      {
        return Raise_HexDigit () || Raise_Eos ();
      }

      stringBuilder.Append (r);

      return true;
    }

    bool TryParse_Chars ()
    {
      var current = position;

      while (!Eos ())
      {
        var c = Char ();
        switch (c)
        {
          case '"':
            stringBuilder.Append (input, current, position - current);
            return true;
          case '\n':
          case '\r':
            visitor.Unexpected (position, Common.NewLine);
            return false;
          case '\\':
            stringBuilder.Append (input, current, position - current);
            Advance ();

            if (Eos ())
            {
              return Raise_Escapes () || Raise_Eos ();
            }
            else
            {
              var e = Char ();
              switch (e)
              {
              case '"':
              case '\\':
              case '/':
                current = position;
                Advance ();
                break;
              case 'b':
                stringBuilder.Append ('\b');
                Advance ();
                current = position;
                break;
              case 'f':
                stringBuilder.Append ('\f');
                Advance ();
                current = position;
                break;
              case 'n':
                stringBuilder.Append ('\n');
                Advance ();
                current = position;
                break;
              case 'r':
                stringBuilder.Append ('\r');
                Advance ();
                current = position;
                break;
              case 't':
                stringBuilder.Append ('\t');
                Advance ();
                current = position;
                break;
              case 'u':
                Advance ();
                if (!TryParse_UnicodeChar ())
                {
                  return false;
                }
                current = position;
                break;
              default:
                return Raise_Escapes ();
              }
            }
            break;
          default:
            Advance ();
            break;
        }
      }

      return Raise_Char () || Raise_Eos ();
    }

    bool TryParse_ToStringBuilder ()
    {
      stringBuilder.Clear ();
      return
            TryConsume_Char ('"')
        &&  TryParse_Chars  ()
        &&  TryConsume_Char ('"')
        ;
    }

    bool TryParse_String ()
    {
      return
            TryParse_ToStringBuilder ()
        &&  visitor.StringValue (stringBuilder)
        ;
    }

    bool TryParse_MemberKey ()
    {
      return
            TryParse_ToStringBuilder ()
        &&  visitor.MemberKey (stringBuilder)
        ;
    }

    bool TryParse_UInt (out double r)
    {
      var z       = (double)'0';
      var first   = true;
      var result  = 0.0;

      while (!Eos ())
      {
        var c = Char ();
        if (IsDigit (c))
        {
          Advance ();
          result = 10.0 * result + (c - z);
        }
        else
        {
          r = result;
          return Raise_Digit () || !first;
        }
        first = false;
      }

      r = result;
      return Raise_Digit () || Raise_Eos () || !first;
    }

    bool TryParse_UInt0 (out double r)
    {
      // tryParse_UInt0 only consumes 0 if input is 0123, this in order to be conformant with spec
      if (TryConsume_Char ('0'))
      {
        r = 0.0;
        return true;
      }
      else
      {
        return TryParse_UInt (out r);
      }
    }

    bool TryParse_Fraction (out double r)
    {
      if (TryConsume_Char ('.'))
      {
        var spos = position;
        double f;
        if (TryParse_UInt (out f))
        {
          r = f * Common.Pow10 (spos - position);
          return true;
        }
        else
        {
          r = 0.0;
          return false;
        }
      }
      else
      {
        r = 0.0;
        return true;  // Fraction is optional
      }
    }

    bool TryParse_Exponent (out double r)
    {
      char exp;
      if (TryParse_AnyOf2 ('e', 'E', out exp))
      {
        char sign;
        if (!TryParse_AnyOf2 ('+', '-', out sign))
        {
          // Sign is optional
          sign = '+';
        }
        // TODO: Parse exponent as integer
        var ue = 0.0;
        if (TryParse_UInt (out ue))
        {
          var s = sign == '-' ? -1.0 : 1.0;
          var e = (int)(s * ue);
          r = Common.Pow10 (e);
          return true;
        }
        else
        {
          r = 1.0;
          return false;
        }
      }
      else
      {
        r = 1.0;
        return true;  // Exponent is optional
      }
    }

    bool TryParse_Number ()
    {
      var sign = TryConsume_Char ('-') ? -1.0 : 1.0;
      double i;
      double f;
      double e;

      return
            TryParse_UInt0    (out i)
        &&  TryParse_Fraction (out f)
        &&  TryParse_Exponent (out e)
        &&  visitor.NumberValue (sign*(i + f)*e)
        ;
    }

    bool TryParse_Value ()
    {
      if (Eos ())
      {
        return Raise_Value () || Raise_Eos ();
      }
      else
      {
        var c = Char ();
        switch (c)
        {
          case 'n': return TryParse_Null ()   && Consume_WhiteSpace ();
          case 't': return TryParse_True ()   && Consume_WhiteSpace ();
          case 'f': return TryParse_False ()  && Consume_WhiteSpace ();
          case '[': return TryParse_Array ()  && Consume_WhiteSpace ();
          case '{': return TryParse_Object () && Consume_WhiteSpace ();
          case '"': return TryParse_String () && Consume_WhiteSpace ();
          case '-': return TryParse_Number () && Consume_WhiteSpace ();
          default:
            if (IsDigit (c))
            {
              return TryParse_Number () && Consume_WhiteSpace ();
            }
            else
            {
              return Raise_Value ();
            }
        }
      }
    }

    bool TryParse_RootValue ()
    {
      if (Eos ())
      {
        return Raise_Value () || Raise_Eos ();
      }
      else
      {
        var c = Char ();
        switch (c)
        {
          case '[': return TryParse_Array ()   && Consume_WhiteSpace ();
          case '{': return TryParse_Object ()  && Consume_WhiteSpace ();
          default:
            return Raise_RootValue ();
        }
      }
    }

    public bool TryParse ()
    {
      return Consume_WhiteSpace () && TryParse_RootValue () && TryParse_Eos ();
    }
  }

  sealed partial class JsonWriter : IJsonVisitor
  {
    class Context
    {
      public bool     IsFirst = true;
      public string   Key     = null;
    }

    readonly StringBuilder  json      = new StringBuilder (Common.DefaultSize);
    readonly Stack<Context> contexts  = new Stack<Context> (Common.DefaultSize);

    public JsonWriter (bool i)
    {
      // TODO: Support indenting
      Push ();
    }

    public string Value
    {
      get
      {
        return json.ToString ();
      }
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    void Push ()
    {
      contexts.Push (new Context ());
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    void Pop ()
    {
      Debug.Assert (contexts.Count > 0);
      contexts.Pop ();
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    void PushMemberKey (string key)
    {
      Debug.Assert (contexts.Count > 0);
      var context = contexts.Peek ();
      context.Key = key;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    void Char (char ch)
    {
      //TODO: For what values do we escape with \u0000
      switch (ch)
      {
        case '\\':
          json.Append (@"\\");
          break;
        case '/':
          json.Append (@"\/");
          break;
        case '"':
          json.Append (@"\""");
          break;
        default:
          if (ch < Common.NonPrintableChars.Length)
          {
            json.Append (Common.NonPrintableChars[ch]);
          }
          else
          {
            json.Append (ch);
          }
          break;
      }
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    void JsonValue ()
    {
      Debug.Assert (contexts.Count > 0);
      var c = contexts.Peek ();
      if (c.IsFirst)
      {
        c.IsFirst = false;
      }
      else
      {
        json.Append (',');
      }

      if (c.Key != null)
      {
        json.Append ('"');
        foreach (var ch in c.Key)
        {
          Char (ch);
        }
        json.Append ('"');
        json.Append (':');

        c.Key = null;
      }
    }

    public bool ArrayBegin ()
    {
      JsonValue ();
      Push ();
      json.Append ('[');
      return true;
    }

    public bool ArrayEnd ()
    {
      json.Append (']');
      Pop ();
      return true;
    }

    public bool BoolValue (bool v)
    {
      JsonValue ();
      json.Append (v ? "true" : "false");
      return true;
    }

    public void Expected (int pos, string e)
    {
    }

    public void ExpectedChar (int pos, char e)
    {
    }

    public bool MemberKey (StringBuilder v)
    {
      PushMemberKey (v.ToString ());
      return true;
    }

    public bool MemberKey (string v)
    {
      PushMemberKey (v);
      return true;
    }

    public bool NullValue ()
    {
      JsonValue ();
      json.Append ("null");
      return true;
    }

    public bool NumberValue (double v)
    {
      JsonValue ();
      if (double.IsPositiveInfinity (v))
      {
        json.AppendFormat (@"""Infinity""");  // Json doesn't support Infinity number literal
      }
      else if (double.IsNegativeInfinity (v))
      {
        json.AppendFormat (@"""-Infinity""");  // Json doesn't support -Infinity number literal
      }
      else if (double.IsNaN (v))
      {
        json.AppendFormat (@"""Nan""");  // Json doesn't support Nan number literal
      }
      else
      {
        json.AppendFormat (CultureInfo.InvariantCulture, "{0:g}", v);
      }
      return true;
    }

    public bool ObjectBegin ()
    {
      JsonValue ();
      Push ();
      json.Append ('{');
      return true;
    }

    public bool ObjectEnd ()
    {
      json.Append ('}');
      Pop ();
      return true;
    }

    public bool StringValue (StringBuilder v)
    {
      JsonValue ();
      json.Append ('"');
      var count = v.Length;
      for (var iter = 0; iter < count; ++iter)
      {
        Char (v[iter]);
      }
      json.Append ('"');
      return true;
    }

    public bool StringValue (string v)
    {
      JsonValue ();
      json.Append ('"');
      foreach (var ch in v)
      {
        Char (ch);
      }
      json.Append ('"');
      return true;
    }

    public void Unexpected (int pos, string u)
    {
    }
  }

  sealed partial class ErrorWriter : IJsonVisitor
  {
    readonly List<string> expected      = new List<string> (Common.DefaultSize);
    readonly List<char>   expectedChar  = new List<char> (Common.DefaultSize);
    readonly List<string> unexpected    = new List<string> (Common.DefaultSize);

    readonly int          position;

    public ErrorWriter (int pos)
    {
      Debug.Assert (pos > -1);
      position = pos;
    }

    static void Append (string prefix, StringBuilder sb, string[] values)
    {
      if (values.Length > 0)
      {
        sb.AppendLine ();
        sb.Append ("Expected:");
        for (var iter = 0; iter < values.Length; ++iter)
        {
          if (iter == 0)
          {
          }
          else if (iter == values.Length - 1)
          {
            sb.Append (" or ");
          }
          else
          {
            sb.Append (", ");
          }

          sb.Append (values[iter]);
        }
      }
    }

    public string Value
    {
      get
      {
        var sb = new StringBuilder (Common.DefaultSize);

        sb.Append (Common.ErrorPrelude);

        sb.AppendLine ();
        sb.AppendFormat ("@Pos: {0}", position);

        var exp = expectedChar
          .Distinct ()
          .Select (c => "'" + c + "'")
          .Concat (expected)
          .Distinct ()
          .OrderBy (e => e)
          .ToArray ()
          ;

        var unexp = unexpected
          .Distinct ()
          .OrderBy (e => e)
          .ToArray ()
          ;

        Append ("Expected: ", sb, exp);
        Append ("Unexpected: ", sb, unexp);

        return sb.ToString ();
      }
    }

    public bool NullValue ()
    {
      return true;
    }

    public bool BoolValue (bool v)
    {
      return true;
    }

    public bool NumberValue (double v)
    {
      return true;
    }

    public bool StringValue (StringBuilder v)
    {
      return true;
    }

    public bool StringValue (string v)
    {
      return true;
    }

    public bool ArrayBegin ()
    {
      return true;
    }

    public bool ArrayEnd ()
    {
      return true;
    }

    public bool ObjectBegin ()
    {
      return true;
    }

    public bool ObjectEnd ()
    {
      return true;
    }

    public bool MemberKey (StringBuilder v)
    {
      return true;
    }

    public bool MemberKey (string v)
    {
      return true;
    }

    public void ExpectedChar (int pos, char e)
    {
      if (pos == position)
      {
        expectedChar.Add (e);
      }
    }

    public void Expected     (int pos, string e)
    {
      if (pos == position)
      {
        expected.Add (e);
      }
    }

    public void Unexpected   (int pos, string u)
    {
      if (pos == position)
      {
        unexpected.Add (u);
      }
    }
  }

  static partial class Json
  {
    public static string ToString (IJsonHierarchy hierarchy)
    {
      if (hierarchy == null)
      {
        return "[]";
      }

      var visitor = new JsonWriter (false);
      hierarchy.Apply (visitor);
      return visitor.Value;
    }
  }
}
