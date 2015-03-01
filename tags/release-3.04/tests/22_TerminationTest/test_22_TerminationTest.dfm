object frmTerminationDemo: TfrmTerminationDemo
  Left = 0
  Top = 0
  Caption = 'Termination test'
  ClientHeight = 286
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 89
    Top = 0
    Width = 337
    Height = 286
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnStop: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = btnStopClick
  end
  object btnTerminate: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Terminate'
    TabOrder = 2
    OnClick = btnTerminateClick
  end
  object OTLMonitor: TOmniEventMonitor
    OnTaskMessage = OTLMonitorTaskMessage
    OnTaskTerminated = OTLMonitorTaskTerminated
    Left = 24
    Top = 232
  end
end
