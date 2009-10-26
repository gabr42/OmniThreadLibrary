object frmTestOtlThreadPool: TfrmTestOtlThreadPool
  Left = 0
  Top = 0
  Caption = 'OtlThreadPool tester'
  ClientHeight = 306
  ClientWidth = 611
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    611
    306)
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
  object btnRunTask: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Run task'
    TabOrder = 1
    OnClick = btnScheduleClick
  end
  object btnScheduleTask: TButton
    Left = 8
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Schedule task'
    TabOrder = 2
    OnClick = btnScheduleClick
  end
  object btnSchedule6: TButton
    Left = 8
    Top = 70
    Width = 129
    Height = 25
    Caption = 'Schedule 6 tasks'
    TabOrder = 3
    OnClick = btnSchedule6Click
  end
  object btnScheduleAndCancel: TButton
    Left = 8
    Top = 163
    Width = 129
    Height = 25
    Caption = 'Schedule and cancel'
    TabOrder = 4
    OnClick = btnScheduleAndCancelClick
  end
  object btnCancelLong: TButton
    Left = 8
    Top = 194
    Width = 129
    Height = 25
    Caption = 'Cancel long task'
    TabOrder = 5
    OnClick = btnScheduleAndCancelClick
  end
  object btnCancelAll: TButton
    Left = 8
    Top = 225
    Width = 129
    Height = 25
    Caption = 'Cancel all'
    TabOrder = 6
    OnClick = btnCancelAllClick
  end
  object btnSchedule80: TButton
    Left = 8
    Top = 101
    Width = 129
    Height = 25
    Caption = 'Schedule 80 tasks'
    TabOrder = 7
    OnClick = btnSchedule6Click
  end
  object btnSchedule80All: TButton
    Left = 8
    Top = 132
    Width = 129
    Height = 25
    Caption = 'Schedule 80 in long q.'
    TabOrder = 8
    OnClick = btnSchedule6Click
  end
  object btnSaveLog: TButton
    Left = 8
    Top = 273
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save log'
    TabOrder = 9
    OnClick = btnSaveLogClick
  end
  object OmniTED: TOmniEventMonitor
    OnPoolThreadCreated = OmniTEDPoolThreadCreated
    OnPoolThreadDestroying = OmniTEDPoolThreadDestroying
    OnPoolThreadKilled = OmniTEDPoolThreadKilled
    OnPoolWorkItemCompleted = OmniTEDPoolWorkItemCompleted
    OnTaskTerminated = OmniTEDTaskTerminated
    Left = 8
    Top = 264
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'log'
    Left = 112
    Top = 264
  end
end
