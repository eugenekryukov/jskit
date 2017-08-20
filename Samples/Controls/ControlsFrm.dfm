object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'JSPack Controls'
  ClientHeight = 565
  ClientWidth = 739
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
    Top = 305
    Width = 739
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 260
  end
  object PlayPanelOwner: TPanel
    Left = 0
    Top = 308
    Width = 739
    Height = 257
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PlayLabel: TLabel
      Left = 20
      Top = 62
      Width = 45
      Height = 13
      Caption = 'PlayLabel'
    end
    object PlayBox: TPaintBox
      Left = 20
      Top = 102
      Width = 105
      Height = 105
    end
    object PlayButton: TButton
      Left = 20
      Top = 22
      Width = 93
      Height = 25
      Caption = 'PlayButton'
      TabOrder = 0
    end
    object PlayPanel: TPanel
      AlignWithMargins = True
      Left = 200
      Top = 3
      Width = 536
      Height = 251
      Margins.Left = 200
      Align = alClient
      Caption = 'PlayPanel'
      TabOrder = 1
    end
  end
  object ScriptPanel: TPanel
    Left = 0
    Top = 0
    Width = 739
    Height = 305
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 314
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Run'
      TabOrder = 0
      OnClick = Button1Click
    end
    object PageControl1: TPageControl
      AlignWithMargins = True
      Left = 3
      Top = 33
      Width = 733
      Height = 269
      Margins.Top = 33
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 1
      OnChange = PageControl1Change
      object TabSheet1: TTabSheet
        Caption = 'Events'
        object Memo1: TMemo
          Left = 0
          Top = 0
          Width = 725
          Height = 241
          Align = alClient
          BorderStyle = bsNone
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Lucida Console'
          Font.Style = []
          Lines.Strings = (
            '//'
            '// Set TNotifyEvent in JavaScript'
            '//'
            ''
            'var Counter = 0;'
            ''
            'PlayButton.Caption = '#39'Click Me'#39';'
            ''
            'PlayButton.OnClick = function (Sender) {'
            '  PlayButton.Caption = '#39'PlayButton'#39';'
            
              '  PlayLabel.Caption = Sender.Caption + '#39' clicked '#39' + Counter.toS' +
              'tring() + '#39'!'#39';'
            '  Counter++;'
            '}'
            ''
            'PlayBox.OnPaint = function (Sender) {'
            '  PlayBox.Canvas.Pen.Style = psDash;'
            ''
            '  PlayBox.Canvas.Ellipse(0, 0, Math.random() * PlayBox.Width,'
            '   Math.random() * PlayBox.Width);'
            '}'
            ''
            'PlayBox.Invalidate();')
          ParentFont = False
          TabOrder = 0
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Create objects'
        ImageIndex = 1
        object Memo2: TMemo
          Left = 0
          Top = 0
          Width = 725
          Height = 241
          Align = alClient
          BorderStyle = bsNone
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Lucida Console'
          Font.Style = []
          Lines.Strings = (
            '//'
            '// Create new instances in JavaScript'
            '//'
            ''
            'var NewButton = new TButton(Form);'
            ''
            'NewButton.Parent = PlayPanel;'
            'NewButton.Left = Math.random() * 300;'
            'NewButton.Top = Math.random() * 200;'
            'NewButton.Width = 200;'
            ''
            'NewButton.Caption = new Date().toLocaleString();')
          ParentFont = False
          TabOrder = 0
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Arrays'
        ImageIndex = 2
        object Memo3: TMemo
          Left = 0
          Top = 0
          Width = 725
          Height = 241
          Align = alClient
          BorderStyle = bsNone
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Lucida Console'
          Font.Style = []
          Lines.Strings = (
            '//'
            '// Access indexed proeprty as JavaScript array'
            '//'
            ''
            'var Memo = new TMemo(Form);'
            'Memo.Parent = PlayPanel;'
            ''
            
              'Memo.Lines.Add('#39'Form.ControlCount '#39' + Form.Controls.length.toStr' +
              'ing());'
            ''
            'for (var C in Form.Controls) {'
            
              '  Memo.Lines.Add('#39'Control '#39' + C.toString() + '#39' name is '#39' + Form.' +
              'Controls[C].Name);'
            '}')
          ParentFont = False
          TabOrder = 0
        end
      end
    end
  end
  object JSScript1: TJSScript
    Classes = [Vcl]
    Objects = [Application, Form, Children]
    Left = 520
    Top = 442
  end
end
