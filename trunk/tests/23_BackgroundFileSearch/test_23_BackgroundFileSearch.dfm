object frmBackgroundFileSearchDemo: TfrmBackgroundFileSearchDemo
  Left = 0
  Top = 0
  Caption = 'Background file search demo'
  ClientHeight = 245
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    466
    245)
  PixelsPerInch = 96
  TextHeight = 13
  object inpFolderMask: TLabeledEdit
    Left = 8
    Top = 24
    Width = 369
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 92
    EditLabel.Height = 13
    EditLabel.Caption = 'Base folder + mask'
    TabOrder = 0
    Text = 'c:\*.dll'
  end
  object btnScan: TButton
    Left = 383
    Top = 22
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Scan'
    TabOrder = 1
    OnClick = btnScanClick
  end
  object lbFiles: TListBox
    Left = 8
    Top = 91
    Width = 450
    Height = 146
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object outScanning: TLabeledEdit
    Left = 8
    Top = 64
    Width = 369
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 62
    EditLabel.Height = 13
    EditLabel.Caption = 'Scanning ....'
    ParentColor = True
    ReadOnly = True
    TabOrder = 3
  end
  object outFiles: TLabeledEdit
    Left = 383
    Top = 64
    Width = 75
    Height = 21
    Anchors = [akTop, akRight]
    EditLabel.Width = 45
    EditLabel.Height = 13
    EditLabel.Caption = 'Found ...'
    ParentColor = True
    ReadOnly = True
    TabOrder = 4
  end
  object OTLMonitor: TOmniEventMonitor
    OnTaskMessage = OTLMonitorTaskMessage
    OnTaskTerminated = OTLMonitorTaskTerminated
    Left = 16
    Top = 200
  end
  object tmrDisplayStatus: TTimer
    Enabled = False
    Interval = 333
    OnTimer = tmrDisplayStatusTimer
    Left = 48
    Top = 200
  end
end
