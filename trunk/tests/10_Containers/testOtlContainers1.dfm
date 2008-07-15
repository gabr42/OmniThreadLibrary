object frmTestOtlContainers: TfrmTestOtlContainers
  Left = 0
  Top = 0
  Caption = 'OtlContainers tester'
  ClientHeight = 306
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
    Height = 306
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 7
  end
  object btnStackStressTest: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Stack stress test'
    TabOrder = 0
    OnClick = btnStackStressTestClick
  end
  object btnBufferStressTest: TButton
    Left = 8
    Top = 183
    Width = 129
    Height = 25
    Caption = 'Ring buffer stress test'
    TabOrder = 5
    OnClick = btnBufferStressTestClick
  end
  object btnStackCorrectnessTest: TButton
    Left = 8
    Top = 132
    Width = 129
    Height = 25
    Caption = 'Stack correctness test'
    TabOrder = 4
    OnClick = btnStackCorrectnessTestClick
  end
  object btnBufferCorrectnessTest: TButton
    Left = 8
    Top = 214
    Width = 129
    Height = 25
    Caption = 'Buffer correctness test'
    TabOrder = 6
    OnClick = btnBufferCorrectnessTestClick
  end
  object btnStack2to1: TButton
    Left = 72
    Top = 39
    Width = 65
    Height = 25
    Caption = '2 -> 1'
    TabOrder = 1
    OnClick = btnStack2to1Click
  end
  object btnStack1to2: TButton
    Left = 72
    Top = 70
    Width = 65
    Height = 25
    Caption = '1 -> 2'
    TabOrder = 2
    OnClick = btnStack1to2Click
  end
  object btnStack2to2: TButton
    Left = 72
    Top = 101
    Width = 65
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 3
    OnClick = btnStack2to2Click
  end
  object OmniTaskEventDispatch1: TOmniTaskEventDispatch
    OnTaskMessage = OmniTaskEventDispatch1TaskMessage
    Left = 8
    Top = 248
  end
end
