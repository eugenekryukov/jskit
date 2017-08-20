program ConsoleJS;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, JSK.Base;

var
  Context: TJSContext;
  Result: TJSValue;
begin
  try
    if ParamCount > 0 then
    begin
      Context := TJSContext.Create;
      try
        Context.SetObject('CallDelphiProc', TJSValue.CreateWithProc(Context,
          procedure
          begin
            Writeln('Called from JS');
          end));

        Writeln('Evaluation result:');

        Result := Context.Evaluate(ParamStr(1));

        if Result.IsString then
          Writeln(Result.AsString);
      finally
        Context.Free;
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
