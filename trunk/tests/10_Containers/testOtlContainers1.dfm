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
    TabOrder = 0
  end
  object btnStackStressTest: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Stack stress test'
    TabOrder = 1
    OnClick = btnStackStressTestClick
  end
  object btnBufferStressTest: TButton
    Left = 8
    Top = 87
    Width = 129
    Height = 25
    Caption = 'Ring buffer stress test'
    TabOrder = 2
    OnClick = btnBufferStressTestClick
  end
  object btnStackCorrectnessTest: TButton
    Left = 8
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Stack correctness test'
    TabOrder = 3
    OnClick = btnStackCorrectnessTestClick
  end
  object btnBufferCorrectnessTest: TButton
    Left = 8
    Top = 118
    Width = 129
    Height = 25
    Caption = 'Buffer correctness test'
    TabOrder = 4
    OnClick = btnBufferCorrectnessTestClick
  end
  object OmniTaskEventDispatch1: TOmniTaskEventDispatch
    OnTaskMessage = OmniTaskEventDispatch1TaskMessage
    Left = 8
    Top = 248
  end
end
