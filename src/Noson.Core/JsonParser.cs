﻿// ----------------------------------------------------------------------------------------------
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

  static partial class Common
  {
    public const int    DefaultSize               = 64            ;

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
              default:    return @"\u{0:04X}".FormatWith (i);
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

    static readonly string [] NonPrintableChars   = CreateNonPrintableChars ();
    static readonly double [] Pow10Table          = CreatePow10Table ();

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
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Text;

  using Noson.Details;

  partial interface IJsonParseVisitor
  {
    bool NullValue    ();
    bool BoolValue    (bool             v);
    bool NumberValue  (double           v);
    bool StringValue  (StringBuilder    v);
    bool ArrayBegin   ();
    bool ArrayEnd     ();
    bool ObjectBegin  ();
    bool ObjectEnd    ();
    bool MemberKey    (StringBuilder    v);
    void ExpectedChar (int pos, char    e);
    void Expected     (int pos, string  e);
    void Unexpected   (int pos, string  u);
  }

  partial class JsonParser
  {
    readonly string             input         ;
    readonly IJsonParseVisitor  visitor       ;

    readonly StringBuilder      stringBuilder ;

    int                         position      ;

    public JsonParser (string i, IJsonParseVisitor v)
    {
      input         = i ?? ""                               ;
      visitor       = v                                     ;
      stringBuilder = new StringBuilder (Common.DefaultSize);

      if (v == null)
      {
        throw new ArgumentException ("v : IParseVisitor must be not be null");
      }

    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool NotEos ()
    {
      // Faster to prefect length?
      return position < input.Length;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool Eos ()
    {
      // Faster to prefect length?
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
      return c == ' ' || c == '\t' || c == '\n' || c == '\r';
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
      visitor.Expected (position, Common.Null         );
      visitor.Expected (position, Common.True         );
      visitor.Expected (position, Common.False        );
      visitor.Expected (position, Common.Digit        );
      visitor.Expected (position, Common.ValuePreludes);
      return false;
    }

    bool Raise_RootValue ()
    {
      visitor.Expected (position, Common.RootValuePreludes);
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
      visitor.Expected (position, Common.Escapes);
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
              TryParse_Delimiter(first)
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
              TryParse_Delimiter(first)
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
      return TryParse_RootValue () && TryParse_Eos ();
    }
  }

  static partial class Json
  {
    public static dynamic ToDynamic (string json)
    {
      return null;
    }
  }

}


