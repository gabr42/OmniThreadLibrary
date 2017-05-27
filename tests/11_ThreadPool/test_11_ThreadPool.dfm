object frmTestOtlThreadPool: TfrmTestOtlThreadPool
  Left = 0
  Top = 0
  Caption = 'OtlThreadPool tester'
  ClientHeight = 552
  ClientWidth = 611
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClick = btnCancelAllClick
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    611
    552)
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 143
    Top = 0
    Width = 468
    Height = 552
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 15
  end
  object btnRunTask: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Run task'
    TabOrder = 0
    OnClick = btnScheduleClick
  end
  object btnScheduleTask: TButton
    Left = 8
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Schedule task'
    TabOrder = 1
    OnClick = btnScheduleClick
  end
  object btnSchedule6: TButton
    Left = 8
    Top = 70
    Width = 129
    Height = 25
    Caption = 'Schedule 6 tasks'
    TabOrder = 2
    OnClick = btnSchedule6Click
  end
  object btnScheduleAndCancel: TButton
    Left = 8
    Top = 195
    Width = 129
    Height = 25
    Caption = 'Schedule and cancel'
    TabOrder = 6
    OnClick = btnScheduleAndCancelClick
  end
  object btnCancelLong: TButton
    Left = 8
    Top = 257
    Width = 129
    Height = 25
    Caption = 'Cancel long task'
    TabOrder = 8
    OnClick = btnScheduleAndCancelClick
  end
  object btnCancelAll: TButton
    Left = 8
    Top = 288
    Width = 129
    Height = 25
    Caption = 'Cancel all'
    TabOrder = 9
    OnClick = btnCancelAllClick
  end
  object btnSchedule80: TButton
    Left = 8
    Top = 133
    Width = 129
    Height = 25
    Caption = 'Schedule 80 tasks'
    TabOrder = 4
    OnClick = btnSchedule6Click
  end
  object btnSchedule80All: TButton
    Left = 8
    Top = 164
    Width = 129
    Height = 25
    Caption = 'Schedule 80 in long q.'
    TabOrder = 5
    OnClick = btnSchedule6Click
  end
  object btnSaveLog: TButton
    Left = 8
    Top = 519
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save log'
    TabOrder = 14
    OnClick = btnSaveLogClick
  end
  object btnScheduleUnobserved: TButton
    Left = 8
    Top = 343
    Width = 129
    Height = 25
    Caption = 'Schedule unobserved'
    TabOrder = 10
    OnClick = btnScheduleUnobservedClick
  end
  object btnScheduleObserved: TButton
    Left = 8
    Top = 374
    Width = 129
    Height = 25
    Caption = 'Schedule observed'
    TabOrder = 11
    OnClick = btnScheduleObservedClick
  end
  object btnScheduleAndWait: TButton
    Left = 8
    Top = 423
    Width = 129
    Height = 25
    Caption = 'Schedule and wait'
    TabOrder = 12
    OnClick = btnScheduleAndWaitClick
  end
  object btnScheduleAndTerminate: TButton
    Left = 8
    Top = 226
    Width = 129
    Height = 25
    Caption = 'Schedule and terminate'
    TabOrder = 7
    OnClick = btnScheduleAndCancelClick
  end
  object btnTestZeroExecutorThreads: TButton
    Left = 8
    Top = 471
    Width = 129
    Height = 25
    Caption = '0 executor thread'
    TabOrder = 13
    OnClick = btnTestZeroExecutorThreadsClick
  end
  object btnSchedule6Long: TButton
    Left = 8
    Top = 101
    Width = 129
    Height = 25
    Caption = 'Schedule 6 long tasks'
    TabOrder = 3
    OnClick = btnSchedule6Click
  end
  object cbSignalToken: TCheckBox
    Left = 8
    Top = 315
    Width = 129
    Height = 17
    Caption = 'Signal canc. token'
    TabOrder = 16
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'log'
    Left = 160
    Top = 64
  end
  object OmniTED: TOmniEventMonitor
    OnPoolThreadCreated = OmniTEDPoolThreadCreated
    OnPoolThreadDestroying = OmniTEDPoolThreadDestroying
    OnPoolThreadKilled = OmniTEDPoolThreadKilled
    OnPoolWorkItemCompleted = OmniTEDPoolWorkItemCompleted
    OnTaskMessage = OmniTEDTaskMessage
    OnTaskTerminated = OmniTEDTaskTerminated
    Left = 216
    Top = 64
  end
end
