object frmParallelForDemo: TfrmParallelForDemo
  Left = 0
  Top = 0
  Caption = 'Parallel.For demo'
  ClientHeight = 394
  ClientWidth = 656
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    656
    394)
  PixelsPerInch = 96
  TextHeight = 13
  object btnStart: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object lbLog: TListBox
    Left = 0
    Top = 39
    Width = 656
    Height = 355
    Align = alBottom
    ItemHeight = 13
    TabOrder = 1
  end
  object btnCancelWith: TButton
    Left = 573
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Cancel with'
    TabOrder = 2
    OnClick = btnCancelWithClick
  end
  object btnTest: TButton
    Left = 492
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test'
    TabOrder = 3
    OnClick = btnTestClick
  end
end
