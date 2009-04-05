object frmMultiMonitorDemo: TfrmMultiMonitorDemo
  Left = 0
  Top = 0
  Caption = 'Multi monitor test'
  ClientHeight = 630
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    680
    630)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 13
    Width = 104
    Height = 13
    Anchors = []
    Caption = 'Number of messages:'
  end
  object Label2: TLabel
    Left = 495
    Top = 13
    Width = 41
    Height = 13
    Anchors = []
    Caption = 'Monitors'
  end
  object btnStart: TButton
    Left = 597
    Top = 9
    Width = 75
    Height = 21
    Anchors = []
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object lbFiles: TListBox
    Left = 8
    Top = 38
    Width = 664
    Height = 571
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object outFiles: TLabeledEdit
    Left = 293
    Top = 10
    Width = 75
    Height = 21
    Margins.Top = 5
    Anchors = []
    EditLabel.Width = 85
    EditLabel.Height = 13
    EditLabel.Margins.Left = 30
    EditLabel.Caption = 'Message number:'
    LabelPosition = lpLeft
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
  end
  object seMessagesCount: TSpinEdit
    Left = 115
    Top = 10
    Width = 73
    Height = 22
    Anchors = []
    MaxValue = 1000000
    MinValue = 1
    TabOrder = 3
    Value = 10000
  end
  object cbRefreshScreen: TCheckBox
    Left = 374
    Top = 11
    Width = 107
    Height = 17
    Anchors = []
    Caption = 'ProcessMessages'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object seMonitors: TSpinEdit
    Left = 542
    Top = 10
    Width = 35
    Height = 22
    Anchors = []
    MaxValue = 16
    MinValue = 1
    TabOrder = 5
    Value = 4
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 24
    Top = 576
  end
end
