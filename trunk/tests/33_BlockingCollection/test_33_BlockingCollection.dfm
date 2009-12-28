object frmTestOmniBlockingCollection: TfrmTestOmniBlockingCollection
  Left = 0
  Top = 0
  Caption = 'TOmniBlockingCollection tester'
  ClientHeight = 410
  ClientWidth = 630
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    630
    410)
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 152
    Top = 0
    Width = 478
    Height = 410
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
  end
  object btnTest: TButton
    Left = 8
    Top = 8
    Width = 138
    Height = 25
    Caption = 'Single-threaded test'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object btn2to2: TButton
    Tag = 2
    Left = 8
    Top = 72
    Width = 138
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 2
    OnClick = StartTest
  end
  object btn3to3: TButton
    Tag = 3
    Left = 8
    Top = 104
    Width = 138
    Height = 25
    Caption = '3 -> 3'
    TabOrder = 3
    OnClick = StartTest
  end
  object btn4to4: TButton
    Tag = 4
    Left = 8
    Top = 136
    Width = 138
    Height = 25
    Caption = '4 -> 4'
    TabOrder = 4
    OnClick = StartTest
  end
  object btn1to1: TButton
    Tag = 1
    Left = 8
    Top = 41
    Width = 138
    Height = 25
    Caption = '1 -> 1'
    TabOrder = 1
    OnClick = StartTest
  end
  object btnTestIntf: TButton
    Left = 8
    Top = 377
    Width = 138
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Send interfaces'
    TabOrder = 6
    OnClick = btnTestIntfClick
  end
  object cbRepeat: TCheckBox
    Left = 8
    Top = 270
    Width = 97
    Height = 17
    Caption = 'Repeat'
    TabOrder = 7
  end
  object btn8to8: TButton
    Tag = 8
    Left = 8
    Top = 168
    Width = 138
    Height = 25
    Caption = '8 -> 8'
    TabOrder = 8
    OnClick = StartTest
  end
  object rgCollectionType: TRadioGroup
    Left = 8
    Top = 293
    Width = 138
    Height = 60
    Caption = 'Collection class'
    ItemIndex = 0
    Items.Strings = (
      '.Take'
      '.TryTake')
    TabOrder = 9
  end
  object btn1to7: TButton
    Left = 8
    Top = 200
    Width = 138
    Height = 25
    Caption = '1 -> 7'
    TabOrder = 10
    OnClick = btn1to7Click
  end
  object btn7to1: TButton
    Left = 8
    Top = 232
    Width = 138
    Height = 25
    Caption = '7 -> 1'
    TabOrder = 11
    OnClick = btn7to1Click
  end
  object OtlMonitor: TOmniEventMonitor
    OnTaskTerminated = OtlMonitorTaskTerminated
    Left = 224
    Top = 8
  end
end
