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
    TabOrder = 0
  end
  object btnTest: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Single-threaded test'
    TabOrder = 1
    OnClick = btnTestClick
  end
  object btn2to2: TButton
    Left = 8
    Top = 39
    Width = 121
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 2
    OnClick = btn2to2Click
  end
  object OtlMonitor: TOmniEventMonitor
    OnTaskTerminated = OtlMonitorTaskTerminated
    Left = 224
    Top = 8
  end
end
