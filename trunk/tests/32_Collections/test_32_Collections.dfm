object frmTestOtlCollections: TfrmTestOtlCollections
  Left = 0
  Top = 0
  Caption = 'OtlCollections tester'
  ClientHeight = 323
  ClientWidth = 586
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 135
    Top = 0
    Width = 451
    Height = 323
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
  end
  object btnTest: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Single-threaded test'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object btn2to2: TButton
    Tag = 2
    Left = 8
    Top = 72
    Width = 121
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 2
    OnClick = StartTest
  end
  object btn3to3: TButton
    Tag = 3
    Left = 8
    Top = 104
    Width = 121
    Height = 25
    Caption = '3 -> 3'
    TabOrder = 3
    OnClick = StartTest
  end
  object btn4to4: TButton
    Tag = 4
    Left = 8
    Top = 136
    Width = 121
    Height = 25
    Caption = '4 -> 4'
    TabOrder = 4
    OnClick = StartTest
  end
  object btn1to1: TButton
    Tag = 1
    Left = 8
    Top = 40
    Width = 121
    Height = 25
    Caption = '1 -> 1'
    TabOrder = 1
    OnClick = StartTest
  end
  object btnTestIntf: TButton
    Left = 8
    Top = 288
    Width = 121
    Height = 25
    Caption = 'Send interfaces'
    TabOrder = 6
    OnClick = btnTestIntfClick
  end
  object cbRepeat: TCheckBox
    Left = 8
    Top = 231
    Width = 97
    Height = 17
    Caption = 'Repeat'
    TabOrder = 7
  end
  object btn6to6: TButton
    Tag = 6
    Left = 8
    Top = 168
    Width = 121
    Height = 25
    Caption = '6 -> 6'
    TabOrder = 8
    OnClick = StartTest
  end
  object btn8to8: TButton
    Tag = 8
    Left = 8
    Top = 200
    Width = 121
    Height = 25
    Caption = '8 -> 8'
    TabOrder = 9
    OnClick = StartTest
  end
  object OtlMonitor: TOmniEventMonitor
    OnTaskTerminated = OtlMonitorTaskTerminated
    Left = 224
    Top = 8
  end
end
