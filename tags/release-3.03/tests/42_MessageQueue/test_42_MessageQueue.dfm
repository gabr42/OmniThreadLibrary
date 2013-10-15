object frmTestMessageQueue: TfrmTestMessageQueue
  Left = 0
  Top = 0
  Caption = 'TOmniMessageQueue stress tests'
  ClientHeight = 620
  ClientWidth = 740
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    740
    620)
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 208
    Top = 0
    Width = 532
    Height = 620
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 11
  end
  object btnQueueStressTest: TButton
    Tag = 11
    Left = 8
    Top = 8
    Width = 194
    Height = 25
    Caption = 'MessageQueue stress test (1 -> 1)'
    TabOrder = 2
    OnClick = btnQueueStressTestClick
  end
  object btnQueueCorrectnessTest: TButton
    Left = 8
    Top = 90
    Width = 129
    Height = 25
    Caption = 'MQueue correctness test'
    TabOrder = 5
    OnClick = btnQueueCorrectnessTestClick
  end
  object btnSaveLog: TButton
    Left = 8
    Top = 556
    Width = 194
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save log'
    TabOrder = 9
    OnClick = btnSaveLogClick
  end
  object btnQueue2to1: TButton
    Tag = 21
    Left = 8
    Top = 36
    Width = 63
    Height = 25
    Caption = '2 -> 1'
    TabOrder = 0
    OnClick = btnQueueStressTestClick
  end
  object btnQueue4to1: TButton
    Tag = 41
    Left = 8
    Top = 63
    Width = 63
    Height = 25
    Caption = '4 -> 1'
    TabOrder = 1
    OnClick = btnQueueStressTestClick
  end
  object inpTestDuration_sec: TLabeledEdit
    Left = 8
    Top = 529
    Width = 194
    Height = 21
    Anchors = [akLeft, akBottom]
    EditLabel.Width = 95
    EditLabel.Height = 13
    EditLabel.Caption = 'Test duration (sec):'
    TabOrder = 7
    Text = '60'
  end
  object btnClearLog: TButton
    Left = 8
    Top = 587
    Width = 194
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear log'
    TabOrder = 10
    OnClick = btnClearLogClick
  end
  object btnQueue1to2: TButton
    Tag = 12
    Left = 74
    Top = 36
    Width = 63
    Height = 25
    Caption = '1 -> 2'
    TabOrder = 6
    OnClick = btnQueueStressTestClick
  end
  object btnQueue2to2: TButton
    Tag = 22
    Left = 74
    Top = 63
    Width = 63
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 8
    OnClick = btnQueueStressTestClick
  end
  object btnQueue1to4: TButton
    Tag = 14
    Left = 139
    Top = 36
    Width = 63
    Height = 25
    Caption = '1 - > 4'
    TabOrder = 3
    OnClick = btnQueueStressTestClick
  end
  object btnQueue4to4: TButton
    Tag = 44
    Left = 139
    Top = 63
    Width = 63
    Height = 25
    Caption = '4 -> 4'
    TabOrder = 4
    OnClick = btnQueueStressTestClick
  end
  object btnQueueAll: TButton
    Left = 139
    Top = 90
    Width = 63
    Height = 25
    Caption = 'Run all'
    TabOrder = 12
    OnClick = btnQueueAllClick
  end
  object OmniEventMonitor1: TOmniEventMonitor
    OnTaskMessage = OmniEventMonitor1TaskMessage
    Left = 248
    Top = 576
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'log'
    Left = 216
    Top = 576
  end
end
