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
    Left = 104
    Top = 0
    Width = 492
    Height = 286
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnStartHello: TButton
    Left = 8
    Top = 8
    Width = 90
    Height = 25
    Action = actStartHello
    TabOrder = 1
  end
  object btnStopHello: TButton
    Left = 8
    Top = 70
    Width = 90
    Height = 25
    Action = actStopHello
    TabOrder = 2
  end
  object btnChangeMessage: TButton
    Left = 8
    Top = 39
    Width = 90
    Height = 25
    Action = actChangeMessage
    TabOrder = 3
  end
  object btnTestInvalidMsg: TButton
    Left = 8
    Top = 253
    Width = 90
    Height = 25
    Caption = 'Invalid message'
    TabOrder = 4
    OnClick = btnTestInvalidMsgClick
  end
  object ActionList: TActionList
    Left = 8
    Top = 224
    object actStartHello: TAction
      Caption = 'Start "Hello"'
      OnExecute = actStartHelloExecute
      OnUpdate = actStartHelloUpdate
    end
    object actStopHello: TAction
      Caption = 'Stop "Hello"'
      OnExecute = actStopHelloExecute
      OnUpdate = actStopHelloUpdate
    end
    object actChangeMessage: TAction
      Caption = 'Change message'
      OnExecute = actChangeMessageExecute
      OnUpdate = actChangeMessageUpdate
    end
  end
  object OmniEventMonitor1: TOmniEventMonitor
    OnTaskMessage = OmniEventMonitor1TaskMessage
    OnTaskTerminated = OmniEventMonitor1TaskTerminated
    Left = 40
    Top = 224
  end
end
