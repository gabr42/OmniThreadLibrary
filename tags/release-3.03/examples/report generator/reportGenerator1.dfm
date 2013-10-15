object frmReportGenerator: TfrmReportGenerator
  Left = 0
  Top = 0
  Caption = 'Report Generator'
  ClientHeight = 337
  ClientWidth = 710
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object btnStart: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object lbLog: TListBox
    Left = 89
    Top = 0
    Width = 621
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnStop: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 2
    OnClick = btnStopClick
  end
  object SimulatorTimer: TTimer
    Enabled = False
    OnTimer = SimulatorTimerTimer
    Left = 24
    Top = 288
  end
end
