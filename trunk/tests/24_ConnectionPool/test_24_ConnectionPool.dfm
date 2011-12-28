object frmConnectionPoolDemo: TfrmConnectionPoolDemo
  Left = 0
  Top = 0
  Caption = 'Connection pool demo'
  ClientHeight = 359
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 111
    Top = 0
    Width = 452
    Height = 359
    Align = alRight
    ItemHeight = 13
    TabOrder = 0
  end
  object btnSchedule: TButton
    Left = 8
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Schedule'
    TabOrder = 1
    OnClick = btnScheduleClick
  end
  object btnScheduleAndWait: TButton
    Left = 8
    Top = 39
    Width = 97
    Height = 25
    Caption = 'Schedule and wait'
    TabOrder = 2
    OnClick = btnScheduleAndWaitClick
  end
  object OTLMonitor: TOmniEventMonitor
    OnTaskTerminated = OTLMonitorTaskTerminated
    Left = 72
    Top = 64
  end
end
