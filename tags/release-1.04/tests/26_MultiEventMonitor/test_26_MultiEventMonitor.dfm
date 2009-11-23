object frmMultiMonitorDemo: TfrmMultiMonitorDemo
  Left = 0
  Top = 0
  Caption = 'Multi monitor test'
  ClientHeight = 507
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    440
    507)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 9
    Width = 104
    Height = 13
    Caption = 'Number of messages:'
  end
  object Label2: TLabel
    Left = 8
    Top = 34
    Width = 139
    Height = 13
    Caption = 'Number of threads/monitors:'
  end
  object lblQueueSize: TLabel
    Left = 8
    Top = 62
    Width = 57
    Height = 13
    Caption = 'Queue size:'
  end
  object btnStart: TButton
    Left = 349
    Top = 8
    Width = 84
    Height = 42
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object lbFiles: TListBox
    Left = 8
    Top = 87
    Width = 424
    Height = 395
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object seMessagesCount: TSpinEdit
    Left = 119
    Top = 6
    Width = 73
    Height = 22
    MaxValue = 1000000
    MinValue = 1
    TabOrder = 2
    Value = 10000
  end
  object seMonitors: TSpinEdit
    Left = 157
    Top = 31
    Width = 35
    Height = 22
    MaxValue = 16
    MinValue = 1
    TabOrder = 3
    Value = 4
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 488
    Width = 440
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object inpQueueSize: TSpinEdit
    Left = 119
    Top = 59
    Width = 73
    Height = 22
    MaxValue = 1000000
    MinValue = 1
    TabOrder = 5
    Value = 1000
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 16
    Top = 440
  end
end
