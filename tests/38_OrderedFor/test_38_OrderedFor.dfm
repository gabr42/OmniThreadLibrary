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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnUnorderedPrimes1: TButton
    Left = 8
    Top = 8
    Width = 138
    Height = 25
    Caption = 'Unordered primes 1'
    TabOrder = 0
    OnClick = btnUnorderedPrimes1Click
  end
  object lbLog: TListBox
    Left = 152
    Top = 0
    Width = 481
    Height = 318
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnOrderedPrimes: TButton
    Left = 8
    Top = 70
    Width = 138
    Height = 25
    Caption = 'Ordered primes'
    TabOrder = 2
    OnClick = btnOrderedPrimesClick
  end
  object btnUnorderedPrimes2: TButton
    Left = 8
    Top = 39
    Width = 138
    Height = 25
    Caption = 'Unordered primes 2'
    TabOrder = 3
    OnClick = btnUnorderedPrimes2Click
  end
  object btnUnorderedCancel: TButton
    Left = 8
    Top = 101
    Width = 138
    Height = 25
    Caption = 'Ordered primes + cancel'
    TabOrder = 4
    OnClick = btnOrderedPrimesClick
  end
  object cbRepeatTest: TCheckBox
    Left = 8
    Top = 144
    Width = 138
    Height = 17
    Caption = 'Repeat'
    TabOrder = 5
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 318
    Width = 633
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 200
    OnTimer = Timer1Timer
    Left = 8
    Top = 264
  end
end
