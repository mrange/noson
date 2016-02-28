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
namespace Noson
{
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Dynamic;
  using System.Globalization;
  using System.Runtime.CompilerServices;
  using System.Text;

  using Details;

  abstract partial class JsonValue : DynamicObject, IJsonHierarchy
  {
    public const double NoNumber    = double.NaN;
    public const string MissingKey  = "";

    public override bool TryConvert (ConvertBinder binder, out object result)
    {
      if (binder.Type == typeof (bool))
      {
        result = AsBoolean ();
        return true;
      }
      else if (binder.Type == typeof (double))
      {
        result = AsNumber ();
        return true;
      }
      else if (binder.Type == typeof (string))
      {
        result = AsString ();
        return true;
      }
      else if (binder.Type == typeof (object[]))
      {
        result = AsObjectArray ();
        return true;
      }
      else
      {
        return base.TryConvert (binder, out result);
      }
    }

    public override bool TryGetMember (GetMemberBinder binder, out object result)
    {
      result = GetMember (binder.Name);
      return true;
    }

    public override bool TryGetIndex (GetIndexBinder binder, object[] indexes, out object result)
    {
      if (indexes.Length == 1)
      {
        var index = indexes[0];
        if (index is string)
        {
          result = GetMember ((string)index);
          return true;
        }
        else if (index is int)
        {
          // TODO: Support all IntLike types
          result = GetValue ((int)index);
          return true;
        }
        else
        {
          return base.TryGetIndex(binder, indexes, out result);
        }
      }
      else
      {
        return base.TryGetIndex(binder, indexes, out result);
      }
    }

    public abstract void      Apply (IJsonVisitor visitor);

    public abstract bool      IsValid ();

    public abstract bool      AsBoolean ();
    public abstract double    AsNumber ();
    public abstract string    AsString ();
    public abstract object[]  AsObjectArray ();

    public abstract JsonValue      GetMember (string name);
    public abstract string    GetKey (int i);
    public abstract JsonValue      GetValue (int i);
    public abstract int       GetCount ();

    public override string    ToString ()
    {
      return AsString ();
    }
  }

  abstract partial class JsonScalarValue : JsonValue
  {
    public override bool IsValid ()
    {
      return true;
    }

    public override object[] AsObjectArray ()
    {
      return new object [] { this };
    }

    public override JsonValue GetMember (string name)
    {
      return JsonMissingValue.Value;
    }

    public override string GetKey (int i)
    {
      return MissingKey;
    }

    public override JsonValue GetValue (int i)
    {
      return i == 0 ? this : JsonMissingValue.Value;
    }

    public override int GetCount ()
    {
      return 1;
    }
  }

  abstract partial class JsonVectorValue : JsonValue
  {
    public override bool IsValid ()
    {
      return true;
    }

    public override double AsNumber ()
    {
      return NoNumber;
    }
  }

  sealed partial class JsonMissingValue : JsonValue
  {
    public readonly static JsonValue Value = new JsonMissingValue ();

    public override void Apply(IJsonVisitor visitor)
    {
      Debug.Assert (visitor != null);
    }

    public override bool IsValid ()
    {
      return false;
    }

    public override bool AsBoolean ()
    {
      return false;
    }

    public override double AsNumber ()
    {
      return NoNumber;
    }

    public override object[] AsObjectArray ()
    {
      return new object [0];
    }

    public override int GetCount ()
    {
      return 0;
    }

    public override string AsString ()
    {
      return "";
    }

    public override string GetKey (int i)
    {
      return MissingKey;
    }

    public override JsonValue GetValue (int i)
    {
      return Value;
    }

    public override JsonValue GetMember (string name)
    {
      return Value;
    }
  }

  sealed partial class JsonNullValue : JsonScalarValue
  {
    public readonly static JsonValue Value = new JsonNullValue ();

    public override void Apply(IJsonVisitor visitor)
    {
      Debug.Assert (visitor != null);
      visitor.NullValue ();
    }

    public override bool AsBoolean ()
    {
      return false;
    }

    public override double AsNumber ()
    {
      return 0.0;
    }

    public override string AsString ()
    {
      return "";
    }
  }

  sealed partial class JsonBooleanValue : JsonScalarValue
  {
    public readonly static JsonValue True  = new JsonBooleanValue (true);
    public readonly static JsonValue False = new JsonBooleanValue (false);

    bool value;

    public JsonBooleanValue (bool v)
    {
      value = v;
    }

    public override void Apply(IJsonVisitor visitor)
    {
      Debug.Assert (visitor != null);
      visitor.BoolValue (value);
    }

    public override bool AsBoolean ()
    {
      return value;
    }

    public override double AsNumber ()
    {
      return value ? 1.0 : 0.0;
    }

    public override string AsString ()
    {
      return value ? "true" : "false";
    }
  }

  sealed partial class JsonNumberValue : JsonScalarValue
  {
    double value;

    public JsonNumberValue (double v)
    {
      value = v;
    }

    public override void Apply(IJsonVisitor visitor)
    {
      Debug.Assert (visitor != null);
      visitor.NumberValue (value);
    }

    public override bool AsBoolean ()
    {
      return value != 0.0;
    }

    public override double AsNumber ()
    {
      return value;
    }

    public override string AsString ()
    {
      return "{0:g}".FormatWith (value);
    }
  }

  sealed partial class JsonStringValue : JsonScalarValue
  {
    string value;

    public JsonStringValue (string v)
    {
      value = v ?? "";
    }

    public override void Apply(IJsonVisitor visitor)
    {
      Debug.Assert (visitor != null);
      visitor.StringValue (value);
    }

    public override bool AsBoolean ()
    {
      return !string.IsNullOrEmpty (value);
    }

    public override double AsNumber ()
    {
      double n;
      if (double.TryParse (value, NumberStyles.Float, CultureInfo.InvariantCulture, out n))
      {
        return n;
      }
      else
      {
        return NoNumber;
      }
    }

    public override string AsString ()
    {
      return value;
    }
  }

  sealed partial class JsonArrayValue : JsonVectorValue
  {
    List<JsonValue> value;

    public JsonArrayValue (List<JsonValue> v)
    {
      value = v ?? new List<JsonValue> ();
    }

    public override void Apply (IJsonVisitor visitor)
    {
      Debug.Assert (visitor != null);
      visitor.ArrayBegin ();
      var count = value.Count;
      for (var iter = 0; iter < count; ++iter)
      {
        value[iter].Apply (visitor);
      }
      visitor.ArrayEnd ();
    }

    public override bool AsBoolean ()
    {
      return value.Count > 0;
    }

    public override object[] AsObjectArray ()
    {
      object[] v = new object[value.Count];
      for (var iter = 0; iter < v.Length; ++iter)
      {
        v[iter] = value[iter];
      }
      return v;
    }

    public override string AsString ()
    {
      return "{0}".FormatWith (value.Count);
    }

    public override JsonValue GetMember (string name)
    {
      return JsonMissingValue.Value;
    }

    public override string GetKey (int i)
    {
      return MissingKey;
    }

    public override JsonValue GetValue (int i)
    {
      if (i > -1 && i < value.Count)
      {
        return value[i];
      }
      else
      {
        return JsonMissingValue.Value;
      }
    }

    public override int GetCount ()
    {
      return value.Count;
    }
  }

  sealed partial class JsonObjectValue : JsonVectorValue
  {
    List<Tuple<string, JsonValue>> value;

    public JsonObjectValue (List<Tuple<string, JsonValue>> v)
    {
      value = v ?? new List<Tuple<string, JsonValue>> ();
    }

    public override void Apply (IJsonVisitor visitor)
    {
      Debug.Assert (visitor != null);
      visitor.ObjectBegin ();
      var count = value.Count;
      for (var iter = 0; iter < count; ++iter)
      {
        var v = value[iter];
        visitor.MemberKey (v.Item1);
        v.Item2.Apply (visitor);
      }
      visitor.ObjectEnd ();
    }

    public override bool AsBoolean ()
    {
      return value.Count > 0;
    }

    public override object[] AsObjectArray ()
    {
      object[] v = new object[value.Count];
      for (var iter = 0; iter < v.Length; ++iter)
      {
        v[iter] = value[iter].Item2;
      }
      return v;
    }

    public override string AsString ()
    {
      return "{0}".FormatWith (value.Count);
    }

    public override JsonValue GetMember (string name)
    {
      foreach (var kv in value)
      {
        if (string.Equals (name, kv.Item1, StringComparison.Ordinal))
        {
          return kv.Item2;
        }
      }

      return JsonMissingValue.Value;
    }

    public override string GetKey (int i)
    {
      if (i > -1 && i < value.Count)
      {
        return value[i].Item1;
      }
      else
      {
        return MissingKey;
      }
    }

    public override JsonValue GetValue (int i)
    {
      if (i > -1 && i < value.Count)
      {
        return value[i].Item2;
      }
      else
      {
        return JsonMissingValue.Value;
      }
    }

    public override int GetCount ()
    {
      return value.Count;
    }
  }

  sealed partial class JsonBuilderVisitor : IJsonVisitor
  {
    abstract partial class BaseContext
    {
      public abstract JsonValue CreateJson ();

      public abstract void Append (JsonValue json);

      public abstract void SetMemberKey (string v);
    }

    sealed partial class RootContext : BaseContext
    {
      JsonValue value = JsonMissingValue.Value;

      public override void Append(JsonValue json)
      {
        Debug.Assert (!value.IsValid ());
        Debug.Assert (json != null);
        value = json;
      }

      public override JsonValue CreateJson()
      {
        Debug.Assert (value.IsValid ());
        return value;
      }

      public override void SetMemberKey(string v)
      {
        Debug.Assert (false);
      }
    }

    sealed partial class ArrayContext : BaseContext
    {
      readonly List<JsonValue> value = new List<JsonValue> (Common.DefaultSize);

      public override void Append(JsonValue json)
      {
        Debug.Assert (json != null);
        value.Add (json);
      }

      public override JsonValue CreateJson()
      {
        return new JsonArrayValue (value);
      }

      public override void SetMemberKey(string v)
      {
        Debug.Assert (false);
      }
    }

    sealed partial class ObjectContext : BaseContext
    {
      readonly List<Tuple<string, JsonValue>> value = new List<Tuple<string, JsonValue>> (Common.DefaultSize);
      string memberKey = "";

      public override void Append(JsonValue json)
      {
        Debug.Assert (json != null);
        value.Add (Tuple.Create (memberKey, json));
        memberKey = "";
      }

      public override JsonValue CreateJson()
      {
        return new JsonObjectValue (value);
      }

      public override void SetMemberKey(string v)
      {
        Debug.Assert (v != null);
        memberKey = v;
      }
    }

    readonly Stack<BaseContext> contexts = new Stack<BaseContext> (Common.DefaultSize);

    public JsonBuilderVisitor ()
    {
      Push (new RootContext ());
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool Push (BaseContext context)
    {
      contexts.Push (context);
      return true;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool Pop ()
    {
      Debug.Assert (contexts.Count > 1);
      var current = contexts.Pop ();
      var json    = current.CreateJson ();
      var parent  = contexts.Peek ();
      parent.Append (json);
      return true;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool Append (JsonValue json)
    {
      Debug.Assert (contexts.Count > 0);
      var current = contexts.Peek ();
      current.Append (json);
      return true;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool PushArray ()
    {
      Push (new ArrayContext ());
      return true;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool PushObject ()
    {
      Push (new ObjectContext ());
      return true;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    bool SetMemberKey (string v)
    {
      Debug.Assert (contexts.Count > 0);
      var current = contexts.Peek ();
      current.SetMemberKey (v);
      return true;
    }

    public JsonValue Value
    {
      get
      {
        Debug.Assert (contexts.Count == 1);
        var context = contexts.Peek ();
        return context.CreateJson ();
      }
    }

    public bool NullValue ()
    {
      return Append (JsonNullValue.Value);
    }

    public bool BoolValue (bool v)
    {
      return Append (v ? JsonBooleanValue.True : JsonBooleanValue.False);
    }

    public bool NumberValue (double v)
    {
      return Append (new JsonNumberValue (v));
    }

    public bool StringValue (StringBuilder v)
    {
      return StringValue (v.ToString ());
    }

    public bool StringValue (string v)
    {
      return Append (new JsonStringValue (v));
    }

    public bool ArrayBegin ()
    {
      return PushArray ();
    }

    public bool ArrayEnd ()
    {
      return Pop ();
    }

    public bool ObjectBegin ()
    {
      return PushObject ();
    }

    public bool ObjectEnd ()
    {
      return Pop ();
    }

    public bool MemberKey (StringBuilder v)
    {
      return MemberKey (v.ToString ());
    }

    public bool MemberKey (string v)
    {
      return SetMemberKey (v);
    }

    public void ExpectedChar (int pos, char e)
    {
    }

    public void Expected     (int pos, string e)
    {
    }

    public void Unexpected   (int pos, string u)
    {
    }
  }

  static partial class Json
  {
    public static JsonValue EmptyJson
    {
      get
      {
        return JsonMissingValue.Value;
      }
    }

    public static bool TryParse (
        string        input
      , bool          fullErrorInfo
      , out int       pos
      , out JsonValue json
      , out string    message
      )
    {
      var visitor = new JsonBuilderVisitor ();
      var parser  = new JsonParser (input, visitor);
      if (parser.TryParse ())
      {
        json    = visitor.Value;
        pos     = parser.Position;
        message = "";
        return true;
      }
      else
      {
        json    = JsonMissingValue.Value;
        pos     = parser.Position;
        message = Common.ErrorPrelude;

        if (fullErrorInfo)
        {
          var evisitor  = new ErrorWriter (parser.Position);
          var eparser   = new JsonParser (input, evisitor);
          var eresult   = eparser.TryParse ();
          Debug.Assert (!eresult);
          message = evisitor.Value;
        }

        return false;
      }
    }
  }
}
