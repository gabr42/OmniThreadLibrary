object frmOderedForDemo: TfrmOderedForDemo
  Left = 0
  Top = 0
  Caption = 'Ordered/Chained Parallel.For'
  ClientHeight = 337
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnUnorderedPrimes: TButton
    Left = 8
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Unordered primes'
    TabOrder = 0
    OnClick = btnUnorderedPrimesClick
  end
  object lbLog: TListBox
    Left = 111
    Top = 0
    Width = 522
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnOrderedPrimes: TButton
    Left = 8
    Top = 71
    Width = 97
    Height = 25
    Caption = 'Ordered primes'
    TabOrder = 2
    OnClick = btnOrderedPrimesClick
  end
  object Button1: TButton
    Left = 8
    Top = 304
    Width = 97
    Height = 25
    Caption = 'Chained (move!)'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 102
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 40
    Width = 97
    Height = 25
    Caption = 'Unordered primes 2'
    TabOrder = 5
    OnClick = Button3Click
  end
end
