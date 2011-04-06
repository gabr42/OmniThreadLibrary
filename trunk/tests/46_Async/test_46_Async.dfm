object frmDemoParallelAsync: TfrmDemoParallelAsync
  Left = 0
  Top = 0
  Caption = 'Parallel.Async tester'
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
  object btnAsync: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Async'
    TabOrder = 0
    OnClick = btnAsyncClick
  end
  object lbLog: TListBox
    Left = 89
    Top = 0
    Width = 546
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
end
