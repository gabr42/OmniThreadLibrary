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
  object btnUnorderedPrimes1: TButton
    Left = 8
    Top = 8
    Width = 122
    Height = 25
    Caption = 'Unordered primes 1'
    TabOrder = 0
    OnClick = btnUnorderedPrimes1Click
  end
  object lbLog: TListBox
    Left = 136
    Top = 0
    Width = 497
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnOrderedPrimes: TButton
    Left = 8
    Top = 70
    Width = 122
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
    Caption = 'cooperative'
    TabOrder = 3
    OnClick = Button1Click
  end
  object btnUnorderedPrimes2: TButton
    Left = 8
    Top = 39
    Width = 122
    Height = 25
    Caption = 'Unordered primes 2'
    TabOrder = 4
    OnClick = btnUnorderedPrimes2Click
  end
end
