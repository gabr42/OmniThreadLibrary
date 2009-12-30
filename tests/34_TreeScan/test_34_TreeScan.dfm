object frmTreeScanDemo: TfrmTreeScanDemo
  Left = 0
  Top = 0
  Caption = 'Tree scan demo'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 111
    Top = 0
    Width = 524
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnBuildTree: TButton
    Left = 8
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Build tree'
    TabOrder = 1
    OnClick = btnBuildTreeClick
  end
  object btnSeqScan: TButton
    Left = 8
    Top = 72
    Width = 97
    Height = 25
    Caption = 'Sequential scan'
    Enabled = False
    TabOrder = 2
    OnClick = btnSeqScanClick
  end
  object btnParaScan: TButton
    Left = 8
    Top = 104
    Width = 97
    Height = 25
    Caption = 'Parallel scan'
    Enabled = False
    TabOrder = 3
    OnClick = btnParaScanClick
  end
  object btnBuildLarge: TButton
    Left = 8
    Top = 40
    Width = 97
    Height = 25
    Caption = 'Build large tree'
    TabOrder = 4
    OnClick = btnBuildLargeClick
  end
end
