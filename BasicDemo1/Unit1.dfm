object Form1: TForm1
  Left = 526
  Top = 192
  Caption = 'Basic Diff Demo'
  ClientHeight = 385
  ClientWidth = 367
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 23
    Top = 23
    Width = 193
    Height = 15
    Caption = 'Type some text below and compare'
  end
  object PaintBox1: TPaintBox
    Left = 24
    Top = 160
    Width = 320
    Height = 161
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Visible = False
    OnPaint = PaintBox1Paint
  end
  object Edit1: TEdit
    Left = 23
    Top = 50
    Width = 320
    Height = 23
    TabOrder = 0
    Text = 'Lorem ipsum aterium'
  end
  object Edit2: TEdit
    Left = 23
    Top = 83
    Width = 320
    Height = 23
    TabOrder = 1
    Text = 'Lohames in uto'
  end
  object Button1: TButton
    Left = 23
    Top = 121
    Width = 320
    Height = 25
    Caption = 'Co&mpare'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 24
    Top = 335
    Width = 320
    Height = 25
    Caption = '&Close'
    ModalResult = 1
    TabOrder = 3
    OnClick = Button2Click
  end
end
