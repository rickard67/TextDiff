object Form1: TForm1
  Left = 190
  Top = 219
  Caption = 'Basic Diff Demo2'
  ClientHeight = 365
  ClientWidth = 804
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 804
    Height = 23
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblFile1: TLabel
      Left = 0
      Top = 5
      Width = 36
      Height = 15
      Caption = ' File1: '
    end
    object lblFile2: TLabel
      Left = 381
      Top = 5
      Width = 36
      Height = 15
      Caption = ' File2: '
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 346
    Width = 804
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 100
      end>
  end
  object ResultGrid: TStringGrid
    Left = 0
    Top = 23
    Width = 804
    Height = 323
    Align = alClient
    ColCount = 4
    DefaultRowHeight = 17
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    GridLineWidth = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goRowSelect]
    ParentFont = False
    TabOrder = 2
    OnDrawCell = ResultGridDrawCell
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 137
    object File1: TMenuItem
      Caption = '&File'
      object Open11: TMenuItem
        Caption = 'Open &1 ...'
        ShortCut = 16433
        OnClick = Open11Click
      end
      object Open21: TMenuItem
        Caption = 'Open &2 ...'
        ShortCut = 16434
        OnClick = Open21Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuCompare: TMenuItem
        Caption = '&Compare'
        Enabled = False
        ShortCut = 120
        OnClick = mnuCompareClick
      end
      object mnuCancel: TMenuItem
        Caption = 'C&ancel'
        Enabled = False
        ShortCut = 27
        OnClick = mnuCancelClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object mnuView: TMenuItem
      Caption = '&View'
      Enabled = False
      object PreviousChanges1: TMenuItem
        Caption = '&Previous Changes'
        ShortCut = 16464
        OnClick = PreviousChanges1Click
      end
      object NextChanges1: TMenuItem
        Caption = '&Next Changes'
        ShortCut = 16462
        OnClick = NextChanges1Click
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object mnuIgnoreCase: TMenuItem
        Caption = 'Ignore &Case'
        OnClick = mnuIgnoreCaseClick
      end
      object mnuIgnoreWhiteSpace: TMenuItem
        Caption = 'Ignore &White Space'
        OnClick = mnuIgnoreWhiteSpaceClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuNP: TMenuItem
        Caption = 'O(NP) Sequence Comparison Algorithm'
        Checked = True
        GroupIndex = 10
        RadioItem = True
        OnClick = mnuNPClick
      end
      object mnuND: TMenuItem
        Caption = 'O(ND) Difference Algorithm'
        GroupIndex = 10
        RadioItem = True
        OnClick = mnuNDClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 183
    Top = 119
  end
end
