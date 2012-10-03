object frmPipelineDemo: TfrmPipelineDemo
  Left = 0
  Top = 0
  Caption = 'Cascade/pipeline demo'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 119
    Top = 0
    Width = 516
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object btnExtended: TButton
    Left = 8
    Top = 39
    Width = 105
    Height = 25
    Caption = 'Extended Pipeline'
    TabOrder = 1
    OnClick = btnExtendedClick
  end
  object btnSimple: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Simple Pipeline'
    TabOrder = 0
    OnClick = btnSimpleClick
  end
  object btnExtended2: TButton
    Left = 8
    Top = 70
    Width = 105
    Height = 25
    Caption = 'Extended Pipeline 2'
    TabOrder = 3
    OnClick = btnExtended2Click
  end
  object btnStressTest: TButton
    Left = 8
    Top = 304
    Width = 105
    Height = 25
    Caption = 'Stress test'
    TabOrder = 4
    OnClick = btnStressTestClick
  end
  object btnCancelPipe: TButton
    Left = 8
    Top = 101
    Width = 105
    Height = 25
    Caption = 'Pipeline Cancelation'
    TabOrder = 5
    OnClick = btnCancelPipeClick
  end
  object btnCancelPipe2: TButton
    Left = 8
    Top = 132
    Width = 105
    Height = 25
    Caption = 'Pipeline Cancelation'
    TabOrder = 6
    OnClick = btnCancelPipeClick
  end
end
