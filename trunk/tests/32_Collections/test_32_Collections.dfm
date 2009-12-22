object frmTestOtlCollections: TfrmTestOtlCollections
  Left = 0
  Top = 0
  Caption = 'OtlCollections tester'
  ClientHeight = 273
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
    Height = 273
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
    Left = 8
    Top = 71
    Width = 121
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 2
    OnClick = btn2to2Click
  end
  object btn3to3: TButton
    Left = 8
    Top = 102
    Width = 121
    Height = 25
    Caption = '3 -> 3'
    TabOrder = 3
    OnClick = btn3to3Click
  end
  object btn4to4: TButton
    Left = 8
    Top = 133
    Width = 121
    Height = 25
    Caption = '4 -> 4'
    TabOrder = 4
    OnClick = btn4to4Click
  end
  object btn1to1: TButton
    Left = 8
    Top = 40
    Width = 121
    Height = 25
    Caption = '1 -> 1'
    TabOrder = 1
    OnClick = btn1to1Click
  end
  object btnTestIntf: TButton
    Left = 8
    Top = 240
    Width = 121
    Height = 25
    Caption = 'Send interfaces'
    TabOrder = 6
    OnClick = btnTestIntfClick
  end
  object cbRepeat: TCheckBox
    Left = 8
    Top = 164
    Width = 97
    Height = 17
    Caption = 'Repeat'
    TabOrder = 7
  end
  object OtlMonitor: TOmniEventMonitor
    OnTaskTerminated = OtlMonitorTaskTerminated
    Left = 224
    Top = 8
  end
end
