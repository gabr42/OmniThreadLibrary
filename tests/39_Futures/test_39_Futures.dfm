object frmFuturesDemo: TfrmFuturesDemo
  Left = 0
  Top = 0
  Caption = ' '
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
  object btnTestFuture1: TButton
    Left = 8
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Future Primes'
    TabOrder = 0
    OnClick = btnTestFuture1Click
  end
  object lbLog: TListBox
    Left = 103
    Top = 0
    Width = 532
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnTestFuture2: TButton
    Left = 8
    Top = 39
    Width = 89
    Height = 25
    Caption = 'Future Primes'
    TabOrder = 2
    OnClick = btnTestFuture2Click
  end
  object btnTestFuture3: TButton
    Left = 8
    Top = 70
    Width = 89
    Height = 25
    Caption = 'Future Primes'
    TabOrder = 3
    OnClick = btnTestFuture3Click
  end
end
