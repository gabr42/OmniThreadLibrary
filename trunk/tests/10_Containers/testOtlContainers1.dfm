object frmTestOtlContainers: TfrmTestOtlContainers
  Left = 0
  Top = 0
  ActiveControl = btnStackStressTest
  Caption = 'OtlContainers tester'
  ClientHeight = 447
  ClientWidth = 611
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 143
    Top = 0
    Width = 468
    Height = 447
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 7
  end
  object btnStackStressTest: TButton
    Left = 8
    Top = 56
    Width = 129
    Height = 25
    Caption = 'Stack stress test'
    TabOrder = 0
    OnClick = btnStackStressTestClick
  end
  object btnQueueStressTest: TButton
    Left = 8
    Top = 271
    Width = 129
    Height = 25
    Caption = 'Queue stress test'
    TabOrder = 5
    OnClick = btnQueueStressTestClick
  end
  object btnStackCorrectnessTest: TButton
    Left = 8
    Top = 180
    Width = 129
    Height = 25
    Caption = 'Stack correctness test'
    TabOrder = 4
    OnClick = btnStackCorrectnessTestClick
  end
  object btnQueueCorrectnessTest: TButton
    Left = 8
    Top = 302
    Width = 129
    Height = 25
    Caption = 'Queue correctness test'
    TabOrder = 6
    OnClick = btnQueueCorrectnessTestClick
  end
  object btnStack2to1: TButton
    Left = 72
    Top = 87
    Width = 65
    Height = 25
    Caption = '2 -> 1'
    TabOrder = 1
    OnClick = btnStack2to1Click
  end
  object btnStack1to2: TButton
    Left = 72
    Top = 118
    Width = 65
    Height = 25
    Caption = '1 -> 2'
    TabOrder = 2
    OnClick = btnStack1to2Click
  end
  object btnStack2to2: TButton
    Left = 72
    Top = 149
    Width = 65
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 3
    OnClick = btnStack2to2Click
  end
  object btnBaseStackStressTest: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Base stack stress test'
    TabOrder = 8
    OnClick = btnBaseStackStressTestClick
  end
  object btnBaseQueueStressTest: TButton
    Left = 8
    Top = 224
    Width = 129
    Height = 25
    Caption = 'Base queue stress test'
    TabOrder = 9
    OnClick = btnBaseQueueStressTestClick
  end
  object OmniTaskEventDispatch1: TOmniTaskEventDispatch
    OnTaskMessage = OmniTaskEventDispatch1TaskMessage
    Left = 8
    Top = 144
  end
end
