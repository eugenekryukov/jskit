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

unit JSK.API;

{$I JSK.Config.inc}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF WINDOWS} Winapi.Windows, {$ENDIF} Math;

const

  {$IFDEF MACOS}
  JavaScriptCoreLib = '/System/Library/Frameworks/JavaScriptCore.framework/JavaScriptCore';
  {$ELSE}
  {$IFDEF LINUX}
  JavaScriptCoreLib = 'libjavascriptcoregtk-1.0.so';
  {$ELSE}
  JavaScriptCoreLib = 'JavaScriptCore.dll';
  {$ENDIF}
  {$ENDIF}

type

  JSAnsiString = PUtf8Char;

  // A constant identifying the type of a JSValue.

  JSType = (
    JSUndefined,
    JSNull,
    JSBoolean,
    JSNumber,
    JSString,
    JSObject
  );

const

  kJSPropertyAttributeNone = 0;
  kJSPropertyAttributeReadOnly = 1 shl 1;
  kJSPropertyAttributeDontEnum = 1 shl 2;
  kJSPropertyAttributeDontDelete = 1 shl 3;

type

  JSClassAttribute = (
    kJSClassAttributeNone = 0,
    kJSClassAttributeNoAutomaticPrototype = 1 shl 1
  );

  JSContextGroupRef = NativeInt;
  JSContextRef = NativeInt;
  JSGlobalContextRef = NativeInt;
  JSStringRef = NativeInt;
  JSClassRef = NativeInt;
  JSPropertyNameArrayRef = NativeInt;
  JSPropertyNameAccumulatorRef = NativeInt;
  JSValueRef = NativeInt;
  PJSValueRef = ^JSValueRef;
  JSObjectRef = NativeInt;

  JSValueRefArray = array [0..$FFFF] of JSValueRef;
  PJSValueRefArray = ^JSValueRefArray;

  JSPropertyAttributes = Cardinal;
  JSClassAttributes = Cardinal;

  JSObjectInitializeCallback =
    procedure (ctx: JSContextRef; obj: JSObjectRef); cdecl;

  JSObjectFinalizeCallback =
    procedure (obj: JSObjectRef); cdecl;

  JSObjectHasPropertyCallback =
    function (ctx: JSContextRef; obj: JSObjectRef; propertyName: JSStringRef): Integer; cdecl;

  JSObjectGetPropertyCallback =
    function (ctx: JSContextRef; obj: JSObjectRef; propertyName: JSStringRef; exception: JSValueRef): JSValueRef; cdecl;

  JSObjectSetPropertyCallback =
    function(ctx: JSContextRef; obj: JSObjectRef; propertyName: JSStringRef; value: JSValueRef;
      exception: JSValueRef): Integer; cdecl;

  JSObjectDeletePropertyCallback =
    function(ctx: JSContextRef; obj: JSObjectRef; propertyName: JSStringRef; exception: PJSValueRef): Integer; cdecl;

  JSObjectGetPropertyNamesCallback =
    procedure(ctx: JSContextRef; obj: JSObjectRef; propertyNames: JSPropertyNameAccumulatorRef); cdecl;

  JSObjectCallAsFunctionCallback =
    function(ctx: JSContextRef; func: JSObjectRef; thisObject: JSObjectRef; argumentCount: LongWord; arguments: PJSValueRefArray;
      exception: PJSValueRef): JSValueRef; cdecl;

  JSObjectCallAsConstructorCallback =
    function(ctx: JSContextRef; construct: JSObjectRef; argumentCount: LongWord;
      arguments: PJSValueRefArray; exception: PJSValueRef): JSObjectRef; cdecl;

  JSObjectHasInstanceCallback =
    function (ctx: JSContextRef; construct: JSObjectRef; possibleInstance: JSValueRef;
      exception: PJSValueRef): Integer; cdecl;

  JSObjectConvertToTypeCallback =
    function(ctx: JSContextRef; obj: JSObjectRef; typ: JSType;
      exception: PJSValueRef): JSValueRef; cdecl;

  JSStaticValue = record
    name: JSAnsiString;
    getProperty: JSObjectGetPropertyCallback;
    setProperty: JSObjectSetPropertyCallback;
    attributes: JSPropertyAttributes;
  end;
  PJSStaticValue = ^JSStaticValue;

  JSStaticFunction = record
    name: JSAnsiString;
    callAsFunction: JSObjectCallAsFunctionCallback;
    attributes: JSPropertyAttributes;
  end;
  PJSStaticFunction = ^JSStaticFunction;

  JSClassDefinition = record
    version: Integer;
    attributes: JSClassAttributes;
    className: JSAnsiString;
    parentClass: JSClassRef;
    staticValues: PJSStaticValue;
    staticFunctions: PJSStaticFunction;
    initialize: JSObjectInitializeCallback;
    finalize: JSObjectFinalizeCallback;
    hasProperty: JSObjectHasPropertyCallback;
    getProperty: JSObjectGetPropertyCallback;
    setProperty: JSObjectSetPropertyCallback;
    deleteProperty: JSObjectDeletePropertyCallback;
    getPropertyNames: JSObjectGetPropertyNamesCallback;
    callAsFunction: JSObjectCallAsFunctionCallback;
    callAsConstructor: JSObjectCallAsConstructorCallback;
    hasInstance: JSObjectHasInstanceCallback;
    convertToType: JSObjectConvertToTypeCallback;
  end;

var

// JSBase

  JSEvaluateScript: function (ctx: JSContextRef; script: JSStringRef; thisObject: JSObjectRef; sourceURL: JSStringRef; startingLineNumber: Integer; exception: PJSValueRef): JSValueRef; cdecl;
  JSCheckScriptSyntax: function (ctx: JSContextRef; script: JSStringRef; sourceURL: JSStringRef; startingLineNumber: Integer; exception: JSValueRef): Integer; cdecl;
  JSGarbageCollect: procedure (ctx: JSContextRef); cdecl;

// JSContext

  JSContextGroupCreate: function : JSContextGroupRef; cdecl;
  JSContextGroupRetain: function (group: JSContextGroupRef): JSContextGroupRef; cdecl;
  JSContextGroupRelease: procedure (group: JSContextGroupRef); cdecl;
  JSGlobalContextCreate: function (globalObjectClass: JSClassRef): JSGlobalContextRef; cdecl;
  JSGlobalContextCreateInGroup: function (group: JSContextGroupRef; globalObjectClass: JSClassRef): JSGlobalContextRef; cdecl;
  JSGlobalContextRetain: function (ctx: JSGlobalContextRef): JSGlobalContextRef; cdecl;
  JSGlobalContextRelease: procedure (ctx: JSGlobalContextRef); cdecl;
  JSContextGetGlobalObject: function (ctx: JSContextRef): JSObjectRef; cdecl;
  JSContextGetGroup: function (ctx: JSContextRef): JSContextGroupRef; cdecl;

// JSString

  JSStringCreateWithCharacters: function (chars: PChar; numChars: LongWord): JSStringRef; cdecl;
  JSStringRetain: function (&string: JSStringRef): JSStringRef; cdecl;
  JSStringRelease: procedure (&string: JSStringRef); cdecl;
  JSStringGetLength: function (&string: JSStringRef): LongWord; cdecl;
  JSStringGetCharactersPtr: function (&string: JSStringRef): PChar; cdecl;
  JSStringGetMaximumUTF8CStringSize: function (&string: JSStringRef): LongWord; cdecl;
  JSStringIsEqual: function (a: JSStringRef; b: JSStringRef): Integer; cdecl;

// JSClass

  JSClassCreate: function (var definition: JSClassDefinition): JSClassRef; cdecl;
  JSClassRetain: function (jsClass: JSClassRef): JSClassRef; cdecl;
  JSClassRelease: procedure (jsClass: JSClassRef); cdecl;
  JSObjectMake: function (ctx: JSContextRef; jsClass: JSClassRef; data: Pointer): JSObjectRef; cdecl;

// JSObject

  JSObjectMakeFunctionWithCallback: function (ctx: JSContextRef; name: JSStringRef; callAsFunction: JSObjectCallAsFunctionCallback): JSObjectRef; cdecl;
  JSObjectMakeConstructor: function (ctx: JSContextRef; jsClass: JSClassRef; callAsConstructor: JSObjectCallAsConstructorCallback): JSObjectRef; cdecl;
  JSObjectMakeArray: function (ctx: JSContextRef; argumentCount: LongWord; arguments: PJSValueRef; exception: JSValueRef) : JSObjectRef; cdecl;
  JSObjectMakeDate: function (ctx: JSContextRef; argumentCount: LongWord; arguments: JSValueRef; exception: JSValueRef): JSObjectRef; cdecl;
  JSObjectMakeError: function (ctx: JSContextRef; argumentCount: LongWord; arguments: JSValueRef; exception: JSValueRef): JSObjectRef; cdecl;
  JSObjectMakeRegExp: function (ctx: JSContextRef; argumentCount: LongWord; arguments: JSValueRef; exception: JSValueRef): JSObjectRef; cdecl;
  JSObjectMakeFunction: function (ctx: JSContextRef; name: JSStringRef; parameterCount: Cardinal; parameterNames: JSStringRef; body: JSStringRef; sourceURL: JSStringRef; startingLineNumber: Integer; exception: JSValueRef): JSObjectRef; cdecl;
  JSObjectGetPrototype: function (ctx: JSContextRef; &object: JSObjectRef): JSValueRef; cdecl;
  JSObjectSetPrototype: procedure (ctx: JSContextRef; &object: JSObjectRef; value: JSValueRef); cdecl;
  JSObjectHasProperty: function (ctx: JSContextRef; &object: JSObjectRef; propertyName: JSStringRef): Integer; cdecl;
  JSObjectGetProperty: function (ctx: JSContextRef; &object: JSObjectRef; propertyName: JSStringRef; exception: PJSValueRef) : JSValueRef; cdecl;
  JSObjectSetProperty: procedure (ctx: JSContextRef; &object: JSObjectRef; propertyName: JSStringRef; value: JSValueRef; attributes: JSPropertyAttributes; exception: PJSValueRef); cdecl;
  JSObjectDeleteProperty: function (ctx: JSContextRef; &object: JSObjectRef; propertyName: JSStringRef; exception: JSValueRef): Integer; cdecl;
  JSObjectGetPropertyAtIndex: function (ctx: JSContextRef; &object: JSObjectRef; propertyIndex: Cardinal; exception: JSValueRef): JSValueRef; cdecl;
  JSObjectSetPropertyAtIndex: procedure (ctx: JSContextRef; &object: JSObjectRef; propertyIndex: Cardinal; value: JSValueRef; exception: JSValueRef); cdecl;
  JSObjectGetPrivate: function (&object: JSObjectRef): Pointer; cdecl;
  JSObjectSetPrivate: function (&object: JSObjectRef; data: Pointer): Boolean; cdecl;
  JSObjectIsFunction: function (ctx: JSContextRef; &object: JSObjectRef): Integer; cdecl;
  JSObjectCallAsFunction: function (ctx: JSContextRef; &object: JSObjectRef; thisObject: JSObjectRef; argumentCount: LongWord; arguments: PJSValueRefArray; exception: JSValueRef): JSValueRef; cdecl;
  JSObjectIsConstructor: function (ctx: JSContextRef; &object: JSObjectRef): Integer; cdecl;
  JSObjectCallAsConstructor: function (ctx: JSContextRef; &object: JSObjectRef; argumentCount: LongWord; arguments: JSValueRef; exception: JSValueRef): JSObjectRef; cdecl;

  JSObjectCopyPropertyNames: function (ctx: JSContextRef; &object: JSObjectRef): JSPropertyNameArrayRef; cdecl;
  JSPropertyNameArrayRetain: function (&array: JSPropertyNameArrayRef): JSPropertyNameArrayRef; cdecl;
  JSPropertyNameArrayRelease: procedure (&array: JSPropertyNameArrayRef); cdecl;
  JSPropertyNameArrayGetCount: function (&array: JSPropertyNameArrayRef): LongWord; cdecl;
  JSPropertyNameArrayGetNameAtIndex: function (&array: JSPropertyNameArrayRef; index: LongWord): JSStringRef; cdecl;
  JSPropertyNameAccumulatorAddName: procedure (accumulator: JSPropertyNameAccumulatorRef; propertyName: JSStringRef); cdecl;

// JSValue

  JSValueGetType: function (ctx: JSContextRef; param1: JSValueRef): JSType; cdecl;
  JSValueIsUndefined: function (ctx: JSContextRef; value: JSValueRef): LongBool; cdecl;
  JSValueIsNull: function (ctx: JSContextRef; value: JSValueRef): LongBool; cdecl;
  JSValueIsBoolean: function (ctx: JSContextRef; value: JSValueRef): LongBool; cdecl;
  JSValueIsNumber: function (ctx: JSContextRef; value: JSValueRef): LongBool; cdecl;
  JSValueIsString: function (ctx: JSContextRef; value: JSValueRef): LongBool; cdecl;
  JSValueIsObject: function (ctx: JSContextRef; value: JSValueRef): LongBool; cdecl;
  JSValueIsObjectOfClass: function (ctx: JSContextRef; value: JSValueRef; jsClass: JSClassRef): LongBool; cdecl;
  JSValueIsEqual: function (ctx: JSContextRef; a: JSValueRef; b: JSValueRef; exception: JSValueRef): LongBool; cdecl;
  JSValueIsStrictEqual: function (ctx: JSContextRef; a: JSValueRef; b: JSValueRef): LongBool; cdecl;
  JSValueIsInstanceOfConstructor: function (ctx: JSContextRef; value: JSValueRef; &constructor: JSObjectRef; exception: JSValueRef): LongBool; cdecl;

  JSValueMakeUndefined: function (ctx: JSContextRef): JSValueRef; cdecl;
  JSValueMakeNull: function (ctx: JSContextRef): JSValueRef; cdecl;
  JSValueMakeBoolean: function (ctx: JSContextRef; boolean: LongBool): JSValueRef; cdecl;
  JSValueMakeNumber: function (ctx: JSContextRef; number: Double): JSValueRef; cdecl;
  JSValueMakeString: function (ctx: JSContextRef; &string: JSStringRef): JSValueRef; cdecl;
  JSValueMakeFromJSONString: function (ctx: JSContextRef; &string: JSStringRef): JSValueRef; cdecl;
  JSValueCreateJSONString: function (ctx: JSContextRef; value: JSValueRef; indent: Cardinal; exception: JSValueRef) : JSStringRef; cdecl;
  JSValueToBoolean: function (ctx: JSContextRef; value: JSValueRef): LongBool; cdecl;
  JSValueToNumber: function (ctx: JSContextRef; value: JSValueRef; exception: PJSValueRef): Double; cdecl;
  JSValueToStringCopy: function (ctx: JSContextRef; value: JSValueRef; exception: JSValueRef): JSStringRef; cdecl;
  JSValueToObject: function (ctx: JSContextRef; value: JSValueRef; exception: JSValueRef): JSObjectRef; cdecl;

function StrToJSString(const Str: string): JSStringRef;
function JSStringToStr(const Str: JSStringRef): string;
function JSValueToStr(const Ctx: JSContextRef; const Val: JSValueRef): string;

function AllocAnsi(const S: string): JSAnsiString;
procedure DeallocAnsi(const S: JSAnsiString);

var
  JavaScripCoreAvailable: Boolean = False;

implementation

{$IFDEF WIN32}
{$R JSK.Win32.res}
{$ENDIF}
{$IFDEF WIN64}
{$R JSK.Win64.res}
{$ENDIF}

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Generics.Collections;

function AllocAnsi(const S: string): JSAnsiString;
var
  A: Utf8String;
begin
  A := S;
  GetMem(Result, Length(A) + 1);
  Move(PUtf8Char(A)^, Result^, Length(A) + 1);
end;

procedure DeallocAnsi(const S: JSAnsiString);
begin
  FreeMem(S);
end;

function StrToJSString(const Str: string): JSStringRef;
begin
  Result := JSStringCreateWithCharacters(PChar(Str), Length(Str));
end;

function JSStringToStr(const Str: JSStringRef): string;
var
  Len: Integer;
begin
  Len := JSStringGetLength(Str);
  SetLength(Result, Len);
  Move(JSStringGetCharactersPtr(Str)^, PChar(Result)^, Len * 2);
end;

function JSValueToStr(const Ctx: JSContextRef; const Val: JSValueRef): string;
var
  V: JSStringRef;
begin
  if JSValueIsString(ctx, Val) then
  begin
    V := JSValueToStringCopy(ctx, Val, 0);
    Result := JSStringToStr(V);
    JSStringRelease(V);
  end
  else
    Result := '';
end;

var
  JSLib: THandle;
  TempFiles: TStrings;

function CreateTempFile(var TempFile: string; const ResName: string): Boolean;
var
  Src, Dst: TStream;
  FileName: string;
begin
  Result := False;
  {$ifdef mswindows}
  if TempFiles = nil then
    TempFiles := TStringList.Create;

  FileName := ResName + '.dll';
  TempFile := TPath.Combine(TPath.GetTempPath, FileName);
  if not TFile.Exists(TempFile) then
  begin
    Src := TResourceStream.Create(HInstance, PChar(ResName), RT_RCDATA);
    try
      Dst := TFileStream.Create(TempFile, fmCreate);
      try
        Dst.CopyFrom(Src, Src.Size);
      finally
        Dst.Free;
      end;
      TempFiles.Add(TempFile);
      Result := True;
    finally
      Src.Free;
    end;
  end;
  {$endif}
end;

procedure LoadJavaScriptCore;
var
  TempFile: string;
begin
  JSLib := LoadLibrary(JavaScriptCoreLib);
  JavaScripCoreAvailable := JSLib <> 0;

  if not JavaScripCoreAvailable then
  begin
    CreateTempFile(TempFile, 'icudt46');
    CreateTempFile(TempFile, 'icuin46');
    CreateTempFile(TempFile, 'icuuc46');
    CreateTempFile(TempFile, 'WTF');
    CreateTempFile(TempFile, 'JavaScriptCore');
    TDirectory.SetCurrentDirectory(TPath.GetDirectoryName(TempFile));
    JSLib := LoadLibrary(PChar(TempFile));
    JavaScripCoreAvailable := JSLib <> 0;
  end;
  
  if JavaScripCoreAvailable then
  begin
    JSEvaluateScript := GetProcAddress(JSLib, 'JSEvaluateScript');
    JSCheckScriptSyntax := GetProcAddress(JSLib, 'JSCheckScriptSyntax');
    JSGarbageCollect := GetProcAddress(JSLib, 'JSGarbageCollect');
    JSContextGroupCreate := GetProcAddress(JSLib, 'JSContextGroupCreate');
    JSContextGroupRetain := GetProcAddress(JSLib, 'JSContextGroupRetain');
    JSContextGroupRelease := GetProcAddress(JSLib, 'JSContextGroupRelease');
    JSGlobalContextCreate := GetProcAddress(JSLib, 'JSGlobalContextCreate');
    JSGlobalContextCreateInGroup := GetProcAddress(JSLib, 'JSGlobalContextCreateInGroup');
    JSGlobalContextRetain := GetProcAddress(JSLib, 'JSGlobalContextRetain');
    JSGlobalContextRelease := GetProcAddress(JSLib, 'JSGlobalContextRelease');
    JSContextGetGlobalObject := GetProcAddress(JSLib, 'JSContextGetGlobalObject');
    JSContextGetGroup := GetProcAddress(JSLib, 'JSContextGetGroup');
    JSStringCreateWithCharacters := GetProcAddress(JSLib, 'JSStringCreateWithCharacters');
    JSStringRetain := GetProcAddress(JSLib, 'JSStringRetain');
    JSStringRelease := GetProcAddress(JSLib, 'JSStringRelease');
    JSStringGetLength := GetProcAddress(JSLib, 'JSStringGetLength');
    JSStringGetCharactersPtr := GetProcAddress(JSLib, 'JSStringGetCharactersPtr');
    JSStringGetMaximumUTF8CStringSize := GetProcAddress(JSLib, 'JSStringGetMaximumUTF8CStringSize');
    JSStringIsEqual := GetProcAddress(JSLib, 'JSStringIsEqual');
    JSClassCreate := GetProcAddress(JSLib, 'JSClassCreate');
    JSClassRetain := GetProcAddress(JSLib, 'JSClassRetain');
    JSClassRelease := GetProcAddress(JSLib, 'JSClassRelease');
    JSObjectMake := GetProcAddress(JSLib, 'JSObjectMake');
    JSObjectMakeFunctionWithCallback := GetProcAddress(JSLib, 'JSObjectMakeFunctionWithCallback');
    JSObjectMakeConstructor := GetProcAddress(JSLib, 'JSObjectMakeConstructor');
    JSObjectMakeArray := GetProcAddress(JSLib, 'JSObjectMakeArray');
    JSObjectMakeDate := GetProcAddress(JSLib, 'JSObjectMakeDate');
    JSObjectMakeError := GetProcAddress(JSLib, 'JSObjectMakeError');
    JSObjectMakeRegExp := GetProcAddress(JSLib, 'JSObjectMakeRegExp');
    JSObjectMakeFunction := GetProcAddress(JSLib, 'JSObjectMakeFunction');
    JSObjectGetPrototype := GetProcAddress(JSLib, 'JSObjectGetPrototype');
    JSObjectSetPrototype := GetProcAddress(JSLib, 'JSObjectSetPrototype');
    JSObjectHasProperty := GetProcAddress(JSLib, 'JSObjectHasProperty');
    JSObjectGetProperty := GetProcAddress(JSLib, 'JSObjectGetProperty');
    JSObjectSetProperty := GetProcAddress(JSLib, 'JSObjectSetProperty');
    JSObjectDeleteProperty := GetProcAddress(JSLib, 'JSObjectDeleteProperty');
    JSObjectGetPropertyAtIndex := GetProcAddress(JSLib, 'JSObjectGetPropertyAtIndex');
    JSObjectSetPropertyAtIndex := GetProcAddress(JSLib, 'JSObjectSetPropertyAtIndex');
    JSObjectGetPrivate := GetProcAddress(JSLib, 'JSObjectGetPrivate');
    JSObjectSetPrivate := GetProcAddress(JSLib, 'JSObjectSetPrivate');
    JSObjectIsFunction := GetProcAddress(JSLib, 'JSObjectIsFunction');
    JSObjectCallAsFunction := GetProcAddress(JSLib, 'JSObjectCallAsFunction');
    JSObjectIsConstructor := GetProcAddress(JSLib, 'JSObjectIsConstructor');
    JSObjectCallAsConstructor := GetProcAddress(JSLib, 'JSObjectCallAsConstructor');
    JSObjectCopyPropertyNames := GetProcAddress(JSLib, 'JSObjectCopyPropertyNames');
    JSPropertyNameArrayRetain := GetProcAddress(JSLib, 'JSPropertyNameArrayRetain');
    JSPropertyNameArrayRelease := GetProcAddress(JSLib, 'JSPropertyNameArrayRelease');
    JSPropertyNameArrayGetCount := GetProcAddress(JSLib, 'JSPropertyNameArrayGetCount');
    JSPropertyNameArrayGetNameAtIndex := GetProcAddress(JSLib, 'JSPropertyNameArrayGetNameAtIndex');
    JSPropertyNameAccumulatorAddName := GetProcAddress(JSLib, 'JSPropertyNameAccumulatorAddName');
    JSValueGetType := GetProcAddress(JSLib, 'JSValueGetType');
    JSValueIsUndefined := GetProcAddress(JSLib, 'JSValueIsUndefined');
    JSValueIsNull := GetProcAddress(JSLib, 'JSValueIsNull');
    JSValueIsBoolean := GetProcAddress(JSLib, 'JSValueIsBoolean');
    JSValueIsNumber := GetProcAddress(JSLib, 'JSValueIsNumber');
    JSValueIsString := GetProcAddress(JSLib, 'JSValueIsString');
    JSValueIsObject := GetProcAddress(JSLib, 'JSValueIsObject');
    JSValueIsObjectOfClass := GetProcAddress(JSLib, 'JSValueIsObjectOfClass');
    JSValueIsEqual := GetProcAddress(JSLib, 'JSValueIsEqual');
    JSValueIsStrictEqual := GetProcAddress(JSLib, 'JSValueIsStrictEqual');
    JSValueIsInstanceOfConstructor := GetProcAddress(JSLib, 'JSValueIsInstanceOfConstructor');
    JSValueMakeUndefined := GetProcAddress(JSLib, 'JSValueMakeUndefined');
    JSValueMakeNull := GetProcAddress(JSLib, 'JSValueMakeNull');
    JSValueMakeBoolean := GetProcAddress(JSLib, 'JSValueMakeBoolean');
    JSValueMakeNumber := GetProcAddress(JSLib, 'JSValueMakeNumber');
    JSValueMakeString := GetProcAddress(JSLib, 'JSValueMakeString');
    JSValueMakeFromJSONString := GetProcAddress(JSLib, 'JSValueMakeFromJSONString');
    JSValueCreateJSONString := GetProcAddress(JSLib, 'JSValueCreateJSONString');
    JSValueToBoolean := GetProcAddress(JSLib, 'JSValueToBoolean');
    JSValueToNumber := GetProcAddress(JSLib, 'JSValueToNumber');
    JSValueToStringCopy := GetProcAddress(JSLib, 'JSValueToStringCopy');
    JSValueToObject := GetProcAddress(JSLib, 'JSValueToObject');
  end;
end;

procedure UnloadJavaScriptCore;
var
  I: Integer;
begin
  {$ifdef mswindows}
  if (JSLib <> 0) and (TempFiles.Count > 0) then
    FreeLibrary(JSLib);
  TempFiles.DisposeOf;
  {$endif}
end;

initialization
  {$IFDEF WIN64}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  {$ENDIF}
  LoadJavaScriptCore;
finalization
  UnloadJavaScriptCore;
end.
