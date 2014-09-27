object frmForVsForEach: TfrmForVsForEach
  Left = 0
  Top = 0
  Caption = 
    'for vs. Parallel.For vs. Parallel.ForEach vs. TParallel.For Spee' +
    'd Test'
  ClientHeight = 382
  ClientWidth = 653
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnTest: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object lbLog: TListBox
    Left = 0
    Top = 56
    Width = 653
    Height = 326
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    ExplicitTop = 64
  end
end
