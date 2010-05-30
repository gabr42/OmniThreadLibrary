object frmTestRegisterComm: TfrmTestRegisterComm
  Left = 0
  Top = 0
  Caption = 'RegisterComm tester'
  ClientHeight = 286
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 103
    Top = 0
    Width = 323
    Height = 286
    Align = alRight
    ItemHeight = 13
    TabOrder = 0
  end
  object btnSendTo1: TButton
    Left = 8
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Send to task 1'
    TabOrder = 1
    OnClick = btnSendTo1Click
  end
  object btnSendTo2: TButton
    Left = 8
    Top = 39
    Width = 89
    Height = 25
    Caption = 'Send to task 2'
    TabOrder = 2
    OnClick = btnSendTo2Click
  end
  object btnSendObject: TButton
    Left = 8
    Top = 96
    Width = 89
    Height = 25
    Caption = 'Send object'
    TabOrder = 3
    OnClick = btnSendObjectClick
  end
  object btnSendString: TButton
    Left = 8
    Top = 127
    Width = 89
    Height = 25
    Caption = 'Send string'
    TabOrder = 4
    OnClick = btnSendStringClick
  end
  object btnSendFloat: TButton
    Left = 8
    Top = 158
    Width = 89
    Height = 25
    Caption = 'Send float'
    TabOrder = 5
    OnClick = btnSendFloatClick
  end
  object btnSendBool: TButton
    Left = 8
    Top = 189
    Width = 89
    Height = 25
    Caption = 'Send boolean'
    TabOrder = 6
    OnClick = btnSendBoolClick
  end
  object btnSendIntf: TButton
    Left = 8
    Top = 220
    Width = 89
    Height = 25
    Caption = 'Send interface'
    TabOrder = 7
    OnClick = btnSendIntfClick
  end
  object btnSendWS: TButton
    Left = 8
    Top = 251
    Width = 89
    Height = 25
    Caption = 'Send WideString'
    TabOrder = 8
    OnClick = btnSendWSClick
  end
  object OmniTED: TOmniEventMonitor
    OnTaskMessage = OmniTEDTaskMessage
    Left = 8
    Top = 248
  end
end
