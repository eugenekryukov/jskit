unit ControlsFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls, JSK.Components;

type
  TForm4 = class(TForm)
    PlayPanelOwner: TPanel;
    ScriptPanel: TPanel;
    Splitter1: TSplitter;
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    PlayButton: TButton;
    PlayLabel: TLabel;
    Memo1: TMemo;
    JSScript1: TJSScript;
    TabSheet2: TTabSheet;
    Memo2: TMemo;
    PlayBox: TPaintBox;
    PlayPanel: TPanel;
    TabSheet3: TTabSheet;
    Memo3: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
begin
  JSScript1.Context.Evaluate(TMemo(PageControl1.ActivePage.Controls[0]).Lines.Text)
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TForm4.PageControl1Change(Sender: TObject);
var
  I: Integer;
begin
  while PlayPanel.ControlCount > 0 do
    PlayPanel.Controls[0].DisposeOf;
end;

end.
