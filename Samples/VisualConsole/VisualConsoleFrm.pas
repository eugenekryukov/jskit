unit VisualConsoleFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JSK.Components, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm3 = class(TForm)
    ConsoleMemo: TMemo;
    Splitter1: TSplitter;
    CodeMemo: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    JSScript1: TJSScript;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  // Export Delphi object as new global object
  JSScript1.Context.SetObject('Console', ConsoleMemo.Lines);
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  JSScript1.Context.Evaluate(CodeMemo.Lines.Text);
end;

end.
