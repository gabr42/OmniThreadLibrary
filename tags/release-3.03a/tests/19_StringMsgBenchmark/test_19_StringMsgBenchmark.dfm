object frmTestStringMsgBenchmark: TfrmTestStringMsgBenchmark
  Left = 0
  Top = 0
  Caption = 'String and method message dispatch'
  ClientHeight = 286
  ClientWidth = 596
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnStartBenchmark: TButton
    Left = 8
    Top = 8
    Width = 106
    Height = 25
    Caption = 'Start benchmark'
    TabOrder = 0
    OnClick = btnStartBenchmarkClick
  end
  object lbLog: TListBox
    Left = 120
    Top = 0
    Width = 476
    Height = 286
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
end
