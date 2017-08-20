{
  This file is part of JSKit framework.
  Copyright (c) 2017 Eugene Kryukov

  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
}

unit JSK.Base;

{$I JSK.Config.inc}

interface

uses
  Classes, SysUtils, TypInfo, Generics.Collections, Rtti, JSK.API;

type

  JSException = class(Exception);

  TJSContext = class;
  TEventCaller = class;
  TEventCallerClass = class of TEventCaller;

  TJSValue = record
  strict private
    [Weak] FContext: TJSContext;
    FHandle: JSValueRef;
  private
    class constructor Create;
    class destructor Destroy;
    class function CreateWithTValue(const Context: TJSContext; const Value: TValue): TJSValue; static;
    function AsTValue: TValue; overload;
    function AsTValue(P: TRttiProperty): TValue; overload;
    function AsTValue(TypeKind: TTypeKind): TValue; overload;
  public
    class function CreateWithHandle(const Context: TJSContext; const AHandle: JSValueRef): TJSValue; static;
    class function CreateWithObject(const Context: TJSContext; const Obj: TObject): TJSValue; overload; static;
    class function CreateWithNumber(const Context: TJSContext; const Value: Double): TJSValue; static;
    class function CreateWithString(const Context: TJSContext; const Value: string): TJSValue; static;
    class function CreateWithProc(const Context: TJSContext; const Value: TProc): TJSValue; static;
    function IsUndefined: Boolean;
    function IsNull: Boolean;
    function IsObject: Boolean;
    function IsString: Boolean;
    function IsNumber: Boolean;
    function IsBoolean: Boolean;

    function AsObject: TObject;
    function AsString: string;
    function AsNumber: Double;
    function AsBoolean: Boolean;

    function HasProperty(const Name: string): Boolean;
    function GetProperty(const Name: string): TJSValue;
    procedure SetProperty(const Name: string; const Value: TJSValue);

    function Call(const Args: array of TJSValue): TJSValue;

    property Context: TJSContext read FContext;
    property Handle: JSValueRef read FHandle;
  end;

  TEventCaller = class
  private
    FValue: TJSValue;
  protected
    procedure ChecksContext;
  public
    property Value: TJSValue read FValue;
  end;

  TNotifyEventCaller = class(TEventCaller)
  public
    procedure NotifyEvent(Sender: TObject);
  end;

  TJSHandleException = procedure (const Msg: string) of object;

  TJSContext = class
  private type
    TExportProps = array of string;
    TEnumInterfaceProc = reference to procedure (const IntfType: TRttiInterfaceType);
    TEventRec = record
      Cls: TEventCallerClass;
      Method: Pointer;
    end;
  private class var
    FFunctions: TDictionary<JSObjectRef, TProc>;
    FMethods: TObjectList<TObject>;
    FContexts: TDictionary<JSGlobalContextRef, TJSContext>;
    FClasses: TDictionary<PTypeInfo, JSClassRef>;
    FEvents: TDictionary<PTypeInfo, TEventRec>;
  private
    FHandle: JSGlobalContextRef;
    FGlobal: JSObjectRef;
    FOnHandleException: TJSHandleException;
    class constructor Create;
    class destructor Destroy;
    class function GetPropertyCallback(ctx: JSContextRef; obj: JSObjectRef; propertyName: JSStringRef;
      exception: JSValueRef): JSValueRef; cdecl; static;
    class function SetPropertyCallback(ctx: JSContextRef; obj: JSObjectRef; propertyName: JSStringRef;
      value: JSValueRef; exception: JSValueRef): Integer; cdecl; static;
    class function CallAsFunctionCallback(ctx: JSContextRef; func: JSObjectRef; thisObject: JSObjectRef;
      argumentCount: LongWord; arguments: PJSValueRefArray; exception: PJSValueRef): JSValueRef; cdecl; static;
    class function CallAsConstructorCallback(ctx: JSContextRef; construct: JSObjectRef; argumentCount: LongWord;
      arguments: PJSValueRefArray; exception: PJSValueRef): JSObjectRef; cdecl; static;
    class function ProcCallback(ctx: JSContextRef; func, thisObject: JSObjectRef; argumentCount: LongWord;
      arguments: PJSValueRefArray; exception: PJSValueRef): JSValueRef; cdecl; static;
  protected
    procedure HandleException(const Msg: string);
  public
    constructor Create;
    class function ContextWithHandle(const AHandle: JSContextRef): TJSContext;
    destructor Destroy; override;
    procedure RegisterClass(const Cls: TClass);
    procedure RegisterClasses(const Cls: array of TClass);
    procedure RegisterEnum(const Enum: PTypeInfo);
    procedure RegisterEnums(const Enum: array of PTypeInfo);
    class procedure RegisterEvent<T>(const EventCallerClass: TEventCallerClass; const MethodAddr: Pointer); static;
    function Evaluate(const Code: string): TJSValue;
    function GetObject(const Name: string): TJSValue;
    procedure SetObject(const Name: string; const Obj: TJSValue); overload;
    procedure SetObject(const Name: string; const Obj: TObject); overload;
    property Handle: JSGlobalContextRef read FHandle;
    property OnHandleException: TJSHandleException read FOnHandleException write FOnHandleException;
  end;

implementation

var
  GException: JSValueRef;

procedure CleanException;
begin
  GException := 0;
end;

procedure ChecksException(Ctx: JSContextRef);
var
  JStr: JSStringRef;
  EName, EMessage: string;
begin
  if (GException <> 0) and JSValueIsObject(Ctx, GException) then
  begin
    JStr := StrToJSString('name');
    EName := JSValueToStr(Ctx, JSObjectGetProperty(Ctx, GException, JStr, nil));
    JSStringRelease(JStr);
    JStr := StrToJSString('message');
    EMessage := JSValueToStr(Ctx, JSObjectGetProperty(Ctx, GException, JStr, nil));
    JSStringRelease(JStr);
    raise JSException.Create(EName + ': ' + EMessage);
  end;
end;

{ TEventCaller }

procedure TEventCaller.ChecksContext;
begin
  if FValue.Context.FHandle = 0 then
    raise JSException.Create('JSContext is destroyed');
end;

{ TEventCaller }

procedure TNotifyEventCaller.NotifyEvent(Sender: TObject);
begin
  ChecksContext;
  FValue.Call([TJSValue.CreateWithObject(FValue.Context, Sender)]);
end;

{ TJSValue }

class function TJSValue.CreateWithTValue(const Context: TJSContext; const Value: TValue): TJSValue;
begin
  if Value.IsObject then
    Result := TJSValue.CreateWithObject(Context, Value.AsObject)
  else if Value.IsOrdinal then
    Result := TJSValue.CreateWithNumber(Context, Value.AsOrdinal)
  else if Value.IsType<Double> then
    Result := TJSValue.CreateWithNumber(Context, Value.AsType<Double>)
  else if Value.IsType<string> then
    Result := TJSValue.CreateWithString(Context, Value.AsString)
  else
    raise JSException.Create('Unsupported TValue ' + IntToStr(Integer(Value.Kind)));
end;

function TJSValue.AsTValue(TypeKind: TTypeKind): TValue;
begin
  case TypeKind of
    tkString, tkLString, tkWString, tkUString:
      begin
        if IsString then
          Result := AsString
        else if IsNumber then
          Result := FloatToStr(AsNumber)
        else if IsObject then
          Result := '(Object)'
        else if IsNull or IsUndefined then
          Result := '(Null)'
        else
          raise JSException.Create('Invalid string value');
      end;
    tkInteger:
      Result := Trunc(AsNumber);
  else
    Result := AsTValue;
  end;
end;

function TJSValue.AsTValue(P: TRttiProperty): TValue;
var
  EventRec: TJSContext.TEventRec;
  EventCaller: TEventCaller;
  M: TMethod;
begin
  if P.PropertyType.TypeKind = tkEnumeration then
  begin
    TValue.Make(Trunc(AsNumber), P.PropertyType.Handle, Result);
  end
  else if P.PropertyType.TypeKind = tkMethod then
  begin
    if FContext.FEvents.TryGetValue(P.PropertyType.Handle, EventRec) then
    begin
      EventCaller := EventRec.Cls.Create;
      EventCaller.FValue := Self;
      FContext.FMethods.Add(EventCaller);
      M.Code := EventRec.Method;
      M.Data := EventCaller;
      TValue.Make(@M, P.PropertyType.Handle, Result);
    end;
  end
  else
    Result := AsTValue(P.PropertyType.TypeKind);
end;

function TJSValue.AsTValue: TValue;
begin
  CleanException;
  case JSValueGetType(FContext.Handle, FHandle) of
    JSType.JSObject:
      Result := TObject(JSObjectGetPrivate(FHandle));
    JSType.JSBoolean:
      Result := JSValueToBoolean(FContext.Handle, FHandle);
    JSType.JSNumber:
      Result := JSValueToNumber(FContext.Handle, FHandle, @GException);
    JSType.JSString:
      Result := JSValueToStr(FContext.Handle, FHandle);
    JSType.JSNull, JSType.JSUndefined:
      Result := TObject(nil);
  else
    raise JSException.Create('Unsupported argument ' + IntToStr(Integer(JSValueGetType(FContext.Handle, FHandle))));
  end;
  ChecksException(FContext.Handle);
end;

class function TJSValue.CreateWithHandle(const Context: TJSContext; const AHandle: JSValueRef): TJSValue;
begin
  Result.FContext := Context;
  Result.FHandle := AHandle;
end;

class function TJSValue.CreateWithNumber(const Context: TJSContext; const Value: Double): TJSValue;
begin
  Result.FContext := Context;
  Result.FHandle := JSValueMakeNumber(Context.Handle, Value);
end;

class function TJSValue.CreateWithObject(const Context: TJSContext; const Obj: TObject): TJSValue;
var
  Cls: JSClassRef;
begin
  if not Context.FClasses.TryGetValue(Obj.ClassType.ClassInfo, Cls) then
  begin
    Context.RegisterClass(Obj.ClassType);
    Context.FClasses.TryGetValue(Obj.ClassType.ClassInfo, Cls);
  end;

  Result.FContext := Context;
  Result.FHandle := JSObjectMake(Context.FHandle, Cls, Obj);
end;

class function TJSValue.CreateWithProc(const Context: TJSContext; const Value: TProc): TJSValue;
begin
  Result.FContext := Context;
  Result.FHandle := JSObjectMakeFunctionWithCallback(Context.Handle, 0, Context.ProcCallback);
  Context.FFunctions.Add(Result.FHandle, Value);
end;

class function TJSValue.CreateWithString(const Context: TJSContext; const Value: string): TJSValue;
var
  JS: JSStringRef;
begin
  Result.FContext := Context;
  JS := StrToJSString(Value);
  Result.FHandle := JSValueMakeString(Context.Handle, JS);
  JSStringRelease(JS);
end;

class constructor TJSValue.Create;
begin
end;

class destructor TJSValue.Destroy;
begin
end;

function TJSValue.GetProperty(const Name: string): TJSValue;
var
  JS: JSStringRef;
  R: JSValueRef;
begin
  JS := StrToJSString(Name);

  CleanException;
  R := JSObjectGetProperty(FContext.Handle, FHandle, JS, @GException);
  Result := TJSValue.CreateWithHandle(FContext, R);
  ChecksException(FContext.Handle);

  JSStringRelease(JS);
end;

function TJSValue.HasProperty(const Name: string): Boolean;
var
  JS: JSStringRef;
begin
  JS := StrToJSString(Name);
  Result := JSObjectHasProperty(FContext.Handle, FHandle, JS) > 0;
  JSStringRelease(JS);
end;

procedure TJSValue.SetProperty(const Name: string; const Value: TJSValue);
var
  JS: JSStringRef;
begin
  JS := StrToJSString(Name);

  CleanException;
  JSObjectSetProperty(FContext.Handle, FHandle, JS, Value.Handle, 0, @GException);
  ChecksException(FContext.Handle);

  JSStringRelease(JS);
end;

function TJSValue.AsBoolean: Boolean;
begin
  if IsBoolean then
    Result := JSValueToBoolean(FContext.Handle, FHandle)
  else
    raise JSException.Create('Value isn''t boolean');
end;

function TJSValue.AsNumber: Double;
var
  E: JSValueRef;
begin
  if IsNumber then
  begin
    E := 0;
    Result := JSValueToNumber(FContext.Handle, FHandle, @E);
  end
  else
    raise JSException.Create('Value isn''t number');
end;

function TJSValue.AsObject: TObject;
var
  Data: Pointer;
begin
  if IsObject then
  begin
    Data := JSObjectGetPrivate(FHandle);
    if Data <> nil then
      Result := TObject(Data)
    else
      raise JSException.Create('Value doesn''t have instance');
  end
  else
    raise JSException.Create('Value isn''t object');
end;

function TJSValue.AsString: string;
begin
  if IsString then
    Result := JSValueToStr(FContext.Handle, FHandle)
  else
    raise JSException.Create('Value isn''t string');
end;

function TJSValue.Call(const Args: array of TJSValue): TJSValue;
var
  I: Integer;
  JSArgs: array of JSValueRef;
begin
  CleanException;

  SetLength(JSArgs, Length(Args));
  for I := 0 to High(Args) do
    JSArgs[I] := Args[I].Handle;
  Result := TJSValue.CreateWithHandle(FContext, JSObjectCallAsFunction(FContext.Handle, FHandle, FHandle, Length(Args), @JSArgs[0], GException));

  ChecksException(FContext.Handle);
end;

function TJSValue.IsBoolean: Boolean;
begin
  Result := JSValueIsBoolean(FContext.Handle, FHandle);
end;

function TJSValue.IsNull: Boolean;
begin
  Result := JSValueIsNull(FContext.Handle, FHandle);
end;

function TJSValue.IsNumber: Boolean;
begin
  Result := JSValueIsNumber(FContext.Handle, FHandle);
end;

function TJSValue.IsObject: Boolean;
begin
  Result := JSValueIsObject(FContext.Handle, FHandle);
end;

function TJSValue.IsString: Boolean;
begin
  Result := JSValueIsString(FContext.Handle, FHandle);
end;

function TJSValue.IsUndefined: Boolean;
begin
  Result := JSValueIsUndefined(FContext.Handle, FHandle);
end;

{ TJSContext }

constructor TJSContext.Create;
begin
  inherited Create;
  if not JavaScripCoreAvailable then
    raise JSException.Create('JavaScripCore library not found.');
  FHandle := JSGlobalContextCreate(0);
  FGlobal := JSContextGetGlobalObject(FHandle);
  FContexts.Add(FHandle, Self);

  FClasses := TDictionary<PTypeInfo, JSClassRef>.Create;
  FFunctions := TDictionary<JSObjectRef, TProc>.Create;
  FEvents := TDictionary<PTypeInfo, TEventRec>.Create;
  FMethods := TObjectList<TObject>.Create;

  RegisterEvent<TNotifyEvent>(TNotifyEventCaller, @TNotifyEventCaller.NotifyEvent);
end;

class function TJSContext.ContextWithHandle(const AHandle: JSContextRef): TJSContext;
var
  Pair: TPair<JSGlobalContextRef, TJSContext>;
begin
  for Pair in FContexts do
    if JSContextGetGlobalObject(AHandle) = JSContextGetGlobalObject(Pair.Key) then
      Exit(Pair.Value);
  raise JSException.Create('Invalid execution context');
end;

class constructor TJSContext.Create;
begin
  FContexts := TDictionary<JSGlobalContextRef, TJSContext>.Create;
end;

destructor TJSContext.Destroy;
begin
  FMethods.DisposeOf;
  FEvents.DisposeOf;
  FFunctions.DisposeOf;
  FClasses.DisposeOf;

  FContexts.Remove(FHandle);
  JSGlobalContextRelease(FHandle);
  FHandle := 0;
  inherited;
end;

class destructor TJSContext.Destroy;
begin
  FContexts.DisposeOf;
end;

function TJSContext.Evaluate(const Code: string): TJSValue;
var
  Val: JSValueRef;
  JStr: JSStringRef;
  I: Integer;
begin
  try
    CleanException;
    JStr := StrToJSString(Code);
    Val := JSEvaluateScript(FHandle, JStr, 0, 0, 0, @GException);
    JSStringRelease(JStr);
    ChecksException(FHandle);

    Result := TJSValue.CreateWithHandle(Self, Val);
  except
    on E: JSException do
      HandleException(E.Message);
    else
      raise;
  end;
end;

function TJSContext.GetObject(const Name: string): TJSValue;
var
  R, Exception: JSValueRef;
  JS: JSStringRef;
begin
  Exception := 0;
  JS := StrToJSString(Name);
  R := JSObjectGetProperty(FHandle, FGlobal, JS, @Exception);
  JSStringRelease(JS);
  if Exception <> 0 then
    raise JSException.Create('GetProperty error');
  Result := TJSValue.CreateWithHandle(Self, R);
end;

procedure TJSContext.HandleException(const Msg: string);
begin
  if Assigned(FOnHandleException) then
    FOnHandleException(Msg)
  else
    raise Exception.Create(Msg);
end;

procedure TJSContext.SetObject(const Name: string; const Obj: TObject);
begin
  SetObject(Name, TJSValue.CreateWithObject(Self, Obj));
end;

procedure TJSContext.SetObject(const Name: string; const Obj: TJSValue);
var
  Exception: JSValueRef;
begin
  Exception := 0;
  JSObjectSetProperty(FHandle, FGlobal, StrToJSString(Name), Obj.Handle, kJSPropertyAttributeNone, @Exception);
  if Exception <> 0 then
    raise JSException.Create('SetProperty error');
end;

procedure TJSContext.RegisterClass(const Cls: TClass);
var
  T: TRttiInstanceType;
  M: TRttiMethod;
  JDef: JSClassDefinition;
  JFuncs: array of JSStaticFunction;
  JCls, JParentCls: JSClassRef;
  JStr: JSStringRef;
  RttiContext: TRttiContext;
  JContructor: JSObjectRef;
  I: Integer;
begin
  if FClasses.ContainsKey(Cls.ClassInfo) then
    Exit;

  JParentCls := 0;
  if Cls.ClassParent <> nil then
  begin
    RegisterClass(Cls.ClassParent);
    if not FClasses.TryGetValue(Cls.ClassParent.ClassInfo, JParentCls) then
      raise JSException.Create('Can''t export ParentClass');
  end;

  RttiContext := TRttiContext.Create;

  T := TRttiInstanceType(RttiContext.GetType(Cls.ClassInfo));
  if T <> nil then
  begin
    FillChar(JDef, SizeOf(JDef), 0);
    JDef.className := AllocAnsi(T.Name);

    SetLength(JFuncs, 0);
    for M in T.GetDeclaredMethods do
      if (M.Visibility in [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished]) then
      begin
        SetLength(JFuncs, Length(JFuncs) + 1);
        JFuncs[High(JFuncs)].name := AllocAnsi(M.Name);
        JFuncs[High(JFuncs)].callAsFunction := CallAsFunctionCallback;
        JFuncs[High(JFuncs)].attributes := kJSPropertyAttributeReadOnly;
      end;
    SetLength(JFuncs, Length(JFuncs) + 1);
    FillChar(JFuncs[High(JFuncs)], SizeOf(JFuncs[High(JFuncs)]), 0);

    try
      JDef.parentClass := JParentCls;
      JDef.staticFunctions := @JFuncs[0];
      JDef.getProperty := GetPropertyCallback;
      JDef.setProperty := SetPropertyCallback;
      JDef.callAsFunction := CallAsFunctionCallback;

      JCls := JSClassCreate(JDef);

      JStr := StrToJSString('name');
      JContructor := JSObjectMakeConstructor(Handle, JCls, CallAsConstructorCallback);
      JSObjectSetProperty(Handle, JContructor, JStr,
        TJSValue.CreateWithString(Self, Cls.QualifiedClassName).Handle, 0, nil);
      JSStringRelease(JStr);

      SetObject(T.Name, TJSValue.CreateWithHandle(Self, JContructor));
    finally
      for I := 0 to High(JFuncs) - 1 do
        DeallocAnsi(JFuncs[I].name);
      DeallocAnsi(JDef.className);
    end;

    if JCls = 0 then
      raise JSException.Create('Can''t create JS class');
    FClasses.Add(Cls.ClassInfo, JCls);
  end;

  RttiContext.Free;
end;

procedure TJSContext.RegisterClasses(const Cls: array of TClass);
var
  C: TClass;
begin
  for C in Cls do
    RegisterClass(C);
end;

procedure TJSContext.RegisterEnum(const Enum: PTypeInfo);
var
  RttiContext: TRttiContext;
  T: TRttiEnumerationType;
  I: Integer;
  N: string;
begin
  RttiContext := TRttiContext.Create;
  try
    T := TRttiEnumerationType(RttiContext.GetType(Enum));
    if T <> nil then
    begin
      for I := T.MinValue to T.MaxValue do
      begin
        N := GetEnumName(Enum, I);
        SetObject(N, TJSValue.CreateWithNumber(Self, GetEnumValue(Enum, N)));
      end;
    end;
  finally
    RttiContext.Free;
  end;
end;

procedure TJSContext.RegisterEnums(const Enum: array of PTypeInfo);
var
  E: PTypeInfo;
begin
  for E in Enum do
    RegisterEnum(E);
end;

class procedure TJSContext.RegisterEvent<T>(const EventCallerClass: TEventCallerClass; const MethodAddr: Pointer);
var
  EventRec: TEventRec;
begin
  EventRec.Cls := EventCallerClass;
  EventRec.Method := MethodAddr;
  FEvents.Add(TypeInfo(T), EventRec);
end;

class function TJSContext.ProcCallback(ctx: JSContextRef; func: JSObjectRef; thisObject: JSObjectRef; argumentCount: LongWord; arguments: PJSValueRefArray;
  exception: PJSValueRef): JSValueRef; cdecl;
var
  P: TProc;
begin
  if TJSContext.FFunctions.TryGetValue(func, P) then
    P;
  Result := 0;
end;

function FindCountProperty(T: TRttiInstanceType; const PropName: string; Instance: TObject; var Count: Integer): Boolean;
var
  CountName: string;
  P: TRttiProperty;
begin
  CountName := Copy(PropName, 1, Length(PropName) - 1) + 'Count';
  P := T.GetProperty(CountName);
  if P <> nil then
  begin
    Result := True;
    Count := P.GetValue(Instance).AsInteger;
  end
  else
    Result := False;
end;

class function TJSContext.GetPropertyCallback(ctx: JSContextRef; obj: JSObjectRef; propertyName: JSStringRef; exception: JSValueRef): JSValueRef; cdecl;
var
  PropName: string;
  RttiContext: TRttiContext;
  Instance: TObject;
  T: TRttiInstanceType;
  P: TRttiProperty;
  ArrayP: TRttiIndexedProperty;
  F: TRttiField;
  JSArray: array of JSValueRef;
  Count, I: Integer;
begin
  Result := 0;

  PropName := JSStringToStr(propertyName);

  Instance := TObject(JSObjectGetPrivate(obj));

  RttiContext := TRttiContext.Create;
  try
    T := TRttiInstanceType(RttiContext.GetType(Instance.ClassInfo));
    if T <> nil then
    begin
      P := T.GetProperty(PropName);
      if P <> nil then
      begin
        if (P.Visibility in [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished]) and P.IsReadable then
          Exit(TJSValue.CreateWithTValue(TJSContext.ContextWithHandle(ctx), P.GetValue(Instance)).Handle)
        else
          raise JSException.Create('Property ' + PropName + ' isn''t readable');
      end;
      ArrayP := T.GetIndexedProperty(PropName);
      if (ArrayP <> nil) then
      begin
        if (ArrayP.Visibility in [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished]) and ArrayP.IsReadable then
        begin
          if FindCountProperty(T, PropName, Instance, Count) then
          begin
            SetLength(JSArray, Count);
            for I := 0 to High(JSArray) do
              JSArray[I] := TJSValue.CreateWithTValue(TJSContext.ContextWithHandle(ctx), ArrayP.GetValue(Instance, [I])).Handle;
            Exit(JSObjectMakeArray(ctx, Count, @JSArray[0], 0));
          end
          else
            raise JSException.Create('Can''t find Count property for array ' + PropName);
        end
        else
          raise JSException.Create('Array property ' + PropName + ' isn''t readable');
      end;
      F := T.GetField(PropName);
      if F <> nil then
      begin
        if F.Visibility in [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished] then
          Exit(TJSValue.CreateWithTValue(TJSContext.ContextWithHandle(ctx), F.GetValue(Instance)).Handle)
        else
          raise JSException.Create('Field ' + PropName + ' isn''t readable');
      end;
    end;
  finally
    RttiContext.Free;
  end;
end;

class function TJSContext.SetPropertyCallback(ctx: JSContextRef; obj: JSObjectRef; propertyName: JSStringRef; value,
  exception: JSValueRef): Integer;
var
  PropName: string;
  RttiContext: TRttiContext;
  Instance: TObject;
  T: TRttiInstanceType;
  P: TRttiProperty;
  F: TRttiField;
  C: TJSContext;
begin
  Result := 0;

  PropName := JSStringToStr(propertyName);

  Instance := TObject(JSObjectGetPrivate(obj));

  RttiContext := TRttiContext.Create;
  T := TRttiInstanceType(RttiContext.GetType(Instance.ClassInfo));
  if T <> nil then
  begin
    P := T.GetProperty(PropName);
    if P <> nil then
    begin
      C := TJSContext.ContextWithHandle(ctx);
      if (P.Visibility in [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished]) and P.IsWritable then
        P.SetValue(Instance, TJSValue.CreateWithHandle(C, value).AsTValue(P))
      else
        raise JSException.Create('Property ' + PropName + ' isn''t writable');
      Result := 1;
    end;
    F := T.GetField(PropName);
    if F <> nil then
    begin
      C := TJSContext.ContextWithHandle(ctx);
      if F.Visibility in [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished] then
        F.SetValue(Instance, TJSValue.CreateWithHandle(C, value).AsTValue(P))
      else
        raise JSException.Create('Field ' + PropName + ' isn''t readable');
      Result := 1;
    end;
  end;
  RttiContext.Free;
end;

class function TJSContext.CallAsConstructorCallback(ctx: JSContextRef; construct: JSObjectRef; argumentCount: LongWord;
  arguments: PJSValueRefArray; exception: PJSValueRef): JSObjectRef;
var
  ClsName: string;
  JStr: JSStringRef;
  RttiContext: TRttiContext;
  T: TRttiInstanceType;
  M: TRttiMethod;
  I: Integer;
  Args: array of TValue;
  NewObj: TObject;
begin
  Result := 0;

  CleanException;
  try
    JStr := StrToJSString('name');
    ClsName := JSValueToStr(ctx, JSObjectGetProperty(ctx, construct, JStr, @GException));
    JSStringRelease(JStr);

    if ClsName <> '' then
    begin
      RttiContext := TRttiContext.Create;

      T := TRttiInstanceType(RttiContext.FindType(ClsName));
      if T <> nil then
      begin
        NewObj := T.MetaclassType.NewInstance;
        M := T.GetMethod('Create');
        if M <> nil then
        begin
          if Length(M.GetParameters) <> argumentCount then
            raise JSException.Create('Invalid number of parameter for constrcutor');

          SetLength(Args, argumentCount);
          for I := 0 to argumentCount - 1 do
            Args[I] := TJSValue.CreateWithHandle(TJSContext.ContextWithHandle(ctx),
              arguments[I]).AsTValue(M.GetParameters[I].ParamType.TypeKind);

          M.Invoke(NewObj, Args);
          Result := TJSValue.CreateWithObject(TJSContext.ContextWithHandle(ctx), NewObj).Handle
        end
        else
          raise JSException.Create('Can''t find constructor Create');
      end;

      RttiContext.Free;
    end
    else
      raise JSException.Create('Can''t find class name for constructor');
  finally
    ChecksException(ctx);
  end;
end;

class function TJSContext.CallAsFunctionCallback(ctx: JSContextRef; func: JSObjectRef; thisObject: JSObjectRef; argumentCount: LongWord; arguments: PJSValueRefArray;
  exception: PJSValueRef): JSValueRef; cdecl;
var
  I: Integer;
  InstType: TRttiInstanceType;
  FuncName: string;
  RttiContext: TRttiContext;
  Instance: TObject;
  R: TValue;
  M: TRttiMethod;
  Args: array of TValue;
  JStr: JSStringRef;
  C: TJSContext;
begin
  C := TJSContext.ContextWithHandle(ctx);

  CleanException;
  try
    JStr := StrToJSString('name');
    FuncName := JSValueToStr(ctx, JSObjectGetProperty(ctx, func, JStr, @GException));
    JSStringRelease(JStr);
  finally
    ChecksException(ctx);
  end;

  Result := 0;

  Instance := TObject(JSObjectGetPrivate(thisObject));

  RttiContext := TRttiContext.Create;

  InstType := TRttiInstanceType(RttiContext.GetType(Instance.ClassInfo));
  if InstType <> nil then
  begin
    M := InstType.GetMethod(FuncName);
    if (M <> nil) and (M.Visibility in [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished]) then
    begin
      if Length(M.GetParameters) <> argumentCount then
        raise JSException.Create('Wrong parameters count for method ' + M.Name);

      SetLength(Args, argumentCount);
      for I := 0 to argumentCount - 1 do
        Args[I] := TJSValue.CreateWithHandle(C, arguments[I]).AsTValue(M.GetParameters[I].ParamType.TypeKind);
      if M.ReturnType <> nil then
      begin
        R := M.Invoke(Instance, Args);
        Result := TJSValue.CreateWithTValue(C, R).Handle;
      end
      else
      begin
        M.Invoke(Instance, Args);
        Result := 0;
      end;
    end;
  end;

  RttiContext.Free;
end;

end.

