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

unit JSK.Components;

{$I JSK.Config.inc}

interface

uses
  Classes, SysUtils, TypInfo, Generics.Collections, Rtti, JSK.API, JSK.Base;

type

  TJSScriptObject = (
    Application,
    Form,
    Children
  );
  TJSScriptObjects = set of TJSScriptObject;

  TJSScriptClass = (
    Vcl
  );
  TJSScriptClasses = set of TJSScriptClass;

  TJSScript = class(TComponent)
  private
    FContext: TJSContext;
    FOnHandleException: TJSHandleException;
    FObjects: TJSScriptObjects;
    FClasses: TJSScriptClasses;
    procedure SetObjects(const Value: TJSScriptObjects);
    procedure SetClasses(const Value: TJSScriptClasses);
    procedure RegisterVcl;
    procedure RegisterChildren;
  protected
    procedure Loaded; override;
    procedure HandleException(const Msg: string);
    procedure RegisterClasses;
    procedure RegisterObjects;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Context: TJSContext read FContext;
  published
    property Classes: TJSScriptClasses read FClasses write SetClasses;
    property Objects: TJSScriptObjects read FObjects write SetObjects;
    property OnHandleException: TJSHandleException read FOnHandleException write FOnHandleException;
  end;

procedure Register;

implementation

uses
  Vcl.Dialogs, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics;

procedure Register;
begin
  RegisterComponents('JSPack', [TJSScript]);
end;

{ TJSScript }

constructor TJSScript.Create;
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
  begin
    FContext := TJSContext.Create;
    FContext.OnHandleException := HandleException;
  end;
  FClasses := [TJSScriptClass.Vcl];
  FObjects := [TJSScriptObject.Application];
end;

destructor TJSScript.Destroy;
begin
  FreeAndNil(FContext);
  inherited;
end;

procedure TJSScript.HandleException(const Msg: string);
begin
  if Assigned(FOnHandleException) then
    FOnHandleException(Msg)
  else
    ShowMessage(Msg);
end;

procedure TJSScript.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    RegisterClasses;
    RegisterObjects;
  end;
end;

procedure TJSScript.RegisterVcl;
begin
  FContext.RegisterClasses([TLabel, TButton, TEdit, TMemo]);
  FContext.RegisterEnums([TypeInfo(TFormBorderStyle), TypeInfo(TBrushStyle), TypeInfo(TPenStyle)]);
end;

procedure TJSScript.RegisterClasses;
begin
  if TJSScriptClass.Vcl in FClasses then
    RegisterVcl;
end;

procedure TJSScript.RegisterChildren;
var
  I: Integer;
  Child: TComponent;
begin
  if Owner <> nil then
    for I := 0 to Owner.ComponentCount - 1 do
    begin
      Child := Owner.Components[I];
      if Child.Name <> '' then
        Context.SetObject(Child.Name, Child);
    end;
end;

procedure TJSScript.RegisterObjects;
begin
  if TJSScriptObject.Application in FObjects then
    Context.SetObject('Application', Application);
  if TJSScriptObject.Form in FObjects then
    Context.SetObject('Form', Owner);
  if TJSScriptObject.Children in FObjects then
    RegisterChildren;
end;

procedure TJSScript.SetClasses(const Value: TJSScriptClasses);
begin
  FClasses := Value;
end;

procedure TJSScript.SetObjects(const Value: TJSScriptObjects);
begin
  FObjects := Value;
end;

end.

