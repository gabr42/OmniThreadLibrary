object frmForEachSlice: TfrmForEachSlice
  Left = 0
  Top = 0
  Caption = 'ForEach.Slice demo'
  ClientHeight = 394
  ClientWidth = 656
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnStart: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object lbLog: TListBox
    Left = 0
    Top = 56
    Width = 656
    Height = 338
    Align = alBottom
    ItemHeight = 13
    TabOrder = 1
  end
end
