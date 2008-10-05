object frmTestStringMsgDispatch: TfrmTestStringMsgDispatch
  Left = 0
  Top = 0
  Caption = 'Two way w/ string message dispatch'
  ClientHeight = 286
  ClientWidth = 596
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
  object lbLog: TListBox
    Left = 120
    Top = 0
    Width = 476
    Height = 286
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 6
  end
  object btnStartHello: TButton
    Left = 8
    Top = 8
    Width = 106
    Height = 25
    Caption = 'Start "Hello"'
    TabOrder = 0
    OnClick = btnStartHelloClick
  end
  object btnStopHello: TButton
    Left = 8
    Top = 132
    Width = 106
    Height = 25
    Caption = 'Stop "Hello"'
    Enabled = False
    TabOrder = 4
    OnClick = btnStopHelloClick
  end
  object btnChangeMessage: TButton
    Left = 8
    Top = 39
    Width = 106
    Height = 25
    Caption = 'Change message'
    Enabled = False
    TabOrder = 1
    OnClick = btnChangeMessageClick
  end
  object btnTestInvalidMsg: TButton
    Left = 8
    Top = 101
    Width = 106
    Height = 25
    Caption = 'Invalid message'
    Enabled = False
    TabOrder = 3
    OnClick = btnTestInvalidMsgClick
  end
  object btnSendObject: TButton
    Left = 8
    Top = 70
    Width = 106
    Height = 25
    Caption = 'Send object'
    Enabled = False
    TabOrder = 2
    OnClick = btnSendObjectClick
  end
  object cbStringMessages: TCheckBox
    Left = 8
    Top = 184
    Width = 106
    Height = 17
    Caption = 'String messages'
    TabOrder = 5
  end
  object OmniEventMonitor1: TOmniEventMonitor
    OnTaskMessage = OmniEventMonitor1TaskMessage
    OnTaskTerminated = OmniEventMonitor1TaskTerminated
    Left = 8
    Top = 248
  end
end
