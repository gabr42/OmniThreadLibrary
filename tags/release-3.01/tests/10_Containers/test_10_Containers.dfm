object frmTestOtlContainers: TfrmTestOtlContainers
  Left = 0
  Top = 0
  ActiveControl = btnBaseStackStressTest
  Caption = 'OtlContainers tester'
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
    TabOrder = 36
  end
  object btnStackStressTest: TButton
    Tag = 11
    Left = 8
    Top = 128
    Width = 194
    Height = 25
    Caption = 'Stack stress test (1 -> 1)'
    TabOrder = 8
    OnClick = btnStackStressTestClick
  end
  object btnQueueStressTest: TButton
    Tag = 11
    Left = 8
    Top = 365
    Width = 194
    Height = 25
    Caption = 'Queue stress test (1 -> 1)'
    TabOrder = 22
    OnClick = btnQueueStressTestClick
  end
  object btnStackCorrectnessTest: TButton
    Left = 8
    Top = 209
    Width = 129
    Height = 25
    Caption = 'Stack correctness test'
    TabOrder = 14
    OnClick = btnStackCorrectnessTestClick
  end
  object btnQueueCorrectnessTest: TButton
    Left = 8
    Top = 447
    Width = 129
    Height = 25
    Caption = 'Queue correctness test'
    TabOrder = 26
    OnClick = btnQueueCorrectnessTestClick
  end
  object btnStack2to1: TButton
    Tag = 21
    Left = 8
    Top = 155
    Width = 63
    Height = 25
    Caption = '2 -> 1'
    TabOrder = 5
    OnClick = btnStackStressTestClick
  end
  object btnStack1to2: TButton
    Tag = 12
    Left = 74
    Top = 155
    Width = 63
    Height = 25
    Caption = '1 -> 2'
    TabOrder = 10
    OnClick = btnStackStressTestClick
  end
  object btnStack2to2: TButton
    Tag = 22
    Left = 74
    Top = 182
    Width = 63
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 9
    OnClick = btnStackStressTestClick
  end
  object btnBaseStackStressTest: TButton
    Tag = 11
    Left = 8
    Top = 8
    Width = 194
    Height = 25
    Caption = 'Base stack stress test (1 -> 1)'
    TabOrder = 0
    OnClick = btnBaseStackStressTestClick
  end
  object btnBaseQueueStressTest: TButton
    Tag = 11
    Left = 8
    Top = 247
    Width = 194
    Height = 25
    Caption = 'Base queue stress test (1 -> 1)'
    TabOrder = 15
    OnClick = btnBaseQueueStressTestClick
  end
  object btnSaveLog: TButton
    Left = 8
    Top = 556
    Width = 194
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save log'
    TabOrder = 32
    OnClick = btnSaveLogClick
  end
  object btnStack4to1: TButton
    Tag = 41
    Left = 8
    Top = 182
    Width = 63
    Height = 25
    Caption = '4 -> 1'
    TabOrder = 7
    OnClick = btnStackStressTestClick
  end
  object btnBaseStack2to1: TButton
    Tag = 21
    Left = 8
    Top = 36
    Width = 63
    Height = 25
    Caption = '2 -> 1'
    TabOrder = 1
    OnClick = btnBaseStackStressTestClick
  end
  object btnBaseStack1to2: TButton
    Tag = 12
    Left = 74
    Top = 36
    Width = 63
    Height = 25
    Caption = '1 -> 2'
    TabOrder = 3
    OnClick = btnBaseStackStressTestClick
  end
  object btnBaseStack2to2: TButton
    Tag = 22
    Left = 74
    Top = 63
    Width = 63
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 4
    OnClick = btnBaseStackStressTestClick
  end
  object btnBaseStack4to1: TButton
    Tag = 41
    Left = 8
    Top = 63
    Width = 63
    Height = 25
    Caption = '4 -> 1'
    TabOrder = 2
    OnClick = btnBaseStackStressTestClick
  end
  object btnBaseStackCorrectnessTest: TButton
    Left = 8
    Top = 89
    Width = 129
    Height = 25
    Caption = 'Base s. correctness test'
    TabOrder = 6
    OnClick = btnStackCorrectnessTestClick
  end
  object btnBaseQueueCorrectnessTest: TButton
    Left = 8
    Top = 328
    Width = 129
    Height = 25
    Caption = 'Base q. correctness test'
    TabOrder = 21
    OnClick = btnQueueCorrectnessTestClick
  end
  object btnBaseQueue2to1: TButton
    Tag = 21
    Left = 8
    Top = 274
    Width = 63
    Height = 25
    Caption = '2 -> 1'
    TabOrder = 13
    OnClick = btnBaseQueueStressTestClick
  end
  object btnBaseQueue4to1: TButton
    Tag = 41
    Left = 8
    Top = 301
    Width = 63
    Height = 25
    Caption = '4 -> 1'
    TabOrder = 17
    OnClick = btnBaseQueueStressTestClick
  end
  object btnQueue2to1: TButton
    Tag = 21
    Left = 8
    Top = 393
    Width = 63
    Height = 25
    Caption = '2 -> 1'
    TabOrder = 18
    OnClick = btnQueueStressTestClick
  end
  object btnQueue4to1: TButton
    Tag = 41
    Left = 8
    Top = 420
    Width = 63
    Height = 25
    Caption = '4 -> 1'
    TabOrder = 20
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
    TabOrder = 30
    Text = '60'
  end
  object btnAllTests: TButton
    Left = 8
    Top = 482
    Width = 194
    Height = 25
    Caption = 'Run all tests'
    TabOrder = 28
    OnClick = btnAllTestsClick
  end
  object btnClearLog: TButton
    Left = 8
    Top = 587
    Width = 194
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear log'
    TabOrder = 34
    OnClick = btnClearLogClick
  end
  object btnBaseQueue1to2: TButton
    Tag = 12
    Left = 74
    Top = 274
    Width = 63
    Height = 25
    Caption = '1 -> 2'
    TabOrder = 25
    OnClick = btnBaseQueueStressTestClick
  end
  object btnBaseQueue2to2: TButton
    Tag = 22
    Left = 74
    Top = 301
    Width = 63
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 27
    OnClick = btnBaseQueueStressTestClick
  end
  object btnQueue1to2: TButton
    Tag = 12
    Left = 74
    Top = 393
    Width = 63
    Height = 25
    Caption = '1 -> 2'
    TabOrder = 29
    OnClick = btnQueueStressTestClick
  end
  object btnQueue2to2: TButton
    Tag = 22
    Left = 74
    Top = 420
    Width = 63
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 31
    OnClick = btnQueueStressTestClick
  end
  object btnBaseStack1to4: TButton
    Tag = 14
    Left = 140
    Top = 35
    Width = 63
    Height = 25
    Caption = '1 -> 4'
    TabOrder = 33
    OnClick = btnBaseStackStressTestClick
  end
  object btnBaseStack4to4: TButton
    Tag = 44
    Left = 139
    Top = 63
    Width = 63
    Height = 25
    Caption = '4 -> 4'
    TabOrder = 35
    OnClick = btnBaseStackStressTestClick
  end
  object btnStack1to4: TButton
    Tag = 14
    Left = 139
    Top = 155
    Width = 63
    Height = 25
    Caption = '1 -> 4'
    TabOrder = 11
    OnClick = btnStackStressTestClick
  end
  object btnStack4to4: TButton
    Tag = 44
    Left = 139
    Top = 182
    Width = 63
    Height = 25
    Caption = '4 -> 4'
    TabOrder = 12
    OnClick = btnStackStressTestClick
  end
  object btnBaseQueue1to4: TButton
    Tag = 14
    Left = 139
    Top = 274
    Width = 63
    Height = 25
    Caption = '1 -> 4'
    TabOrder = 16
    OnClick = btnBaseQueueStressTestClick
  end
  object btnBaseQueue4to4: TButton
    Tag = 44
    Left = 139
    Top = 301
    Width = 63
    Height = 25
    Caption = '4 -> 4'
    TabOrder = 19
    OnClick = btnBaseQueueStressTestClick
  end
  object btnQueue1to4: TButton
    Tag = 14
    Left = 139
    Top = 393
    Width = 63
    Height = 25
    Caption = '1 - > 4'
    TabOrder = 23
    OnClick = btnQueueStressTestClick
  end
  object btnQueue4to4: TButton
    Tag = 44
    Left = 139
    Top = 420
    Width = 63
    Height = 25
    Caption = '4 -> 4'
    TabOrder = 24
    OnClick = btnQueueStressTestClick
  end
  object btnBaseStackAll: TButton
    Left = 139
    Top = 89
    Width = 63
    Height = 25
    Caption = 'Run all'
    TabOrder = 37
    OnClick = btnBaseStackAllClick
  end
  object btnStackAll: TButton
    Left = 139
    Top = 209
    Width = 63
    Height = 25
    Caption = 'Run all'
    TabOrder = 38
    OnClick = btnStackAllClick
  end
  object btnBaseQueueAll: TButton
    Left = 139
    Top = 328
    Width = 63
    Height = 25
    Caption = 'Run all'
    TabOrder = 39
    OnClick = btnBaseQueueAllClick
  end
  object btnQueueAll: TButton
    Left = 139
    Top = 447
    Width = 63
    Height = 25
    Caption = 'Run all'
    TabOrder = 40
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
