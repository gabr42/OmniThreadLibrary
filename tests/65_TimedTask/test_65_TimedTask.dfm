object frmTimedTask: TfrmTimedTask
  Left = 0
  Top = 0
  Caption = 'TimedTask tester'
  ClientHeight = 87
  ClientWidth = 491
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblinterval: TLabel
    Left = 16
    Top = 19
    Width = 69
    Height = 13
    Caption = 'Interval (sec):'
  end
  object lblServerTime: TLabel
    Left = 240
    Top = 19
    Width = 59
    Height = 13
    Caption = 'Server time:'
  end
  object lblYourIP: TLabel
    Left = 240
    Top = 53
    Width = 39
    Height = 13
    Caption = 'Your IP:'
  end
  object btnStart: TButton
    Left = 16
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 104
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = btnStopClick
  end
  object inpInterval: TSpinEdit
    Left = 104
    Top = 16
    Width = 75
    Height = 22
    Increment = 5
    MaxValue = 60
    MinValue = 5
    TabOrder = 2
    Value = 10
    OnChange = inpIntervalChange
  end
  object outServerTime: TEdit
    Left = 305
    Top = 16
    Width = 168
    Height = 21
    TabOrder = 3
  end
  object outYourIP: TEdit
    Left = 305
    Top = 50
    Width = 168
    Height = 21
    TabOrder = 4
  end
end
