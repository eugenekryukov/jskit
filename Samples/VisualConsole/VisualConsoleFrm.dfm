object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'JSPack VisualConsole'
  ClientHeight = 457
  ClientWidth = 658
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 281
    Width = 658
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 284
  end
  object ConsoleMemo: TMemo
    Left = 0
    Top = 284
    Width = 658
    Height = 173
    Align = alBottom
    BorderStyle = bsNone
    Color = clBlack
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clSilver
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object CodeMemo: TMemo
    Left = 0
    Top = 41
    Width = 658
    Height = 240
    Align = alClient
    BorderStyle = bsNone
    Lines.Strings = (
      '// Export Delphi object to JS just by one call'
      '// Console JS object is a reference for ConsoleMemo.Lines object'
      '// All public and published properties and method are available'
      ''
      'Console.Clear();'
      ''
      'Console.Add('#39'Hello JSPack'#39');'
      'Console.Add(100);'
      ''
      'for (I = 0; I < 10; I++) {'
      '  Console.Add(Math.random());'
      '}'
      ''
      'Console.Add("Lines :" + Console.Count);')
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitTop = 39
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 658
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 292
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Run'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object JSScript1: TJSScript
    Classes = [Vcl]
    Objects = [Application, Form, Children]
    Left = 320
    Top = 176
  end
end
