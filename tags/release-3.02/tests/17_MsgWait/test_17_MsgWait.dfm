object frmTestMsgWait: TfrmTestMsgWait
  Left = 0
  Top = 0
  Caption = 'MsgWait tester'
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
  object btnStart: TButton
    Left = 8
    Top = 8
    Width = 90
    Height = 25
    Action = actStart
    TabOrder = 0
  end
  object lbLog: TListBox
    Left = 104
    Top = 0
    Width = 322
    Height = 286
    Align = alRight
    ItemHeight = 13
    TabOrder = 1
  end
  object btnStop: TButton
    Left = 8
    Top = 39
    Width = 90
    Height = 25
    Action = actStop
    TabOrder = 2
  end
  object ActionList: TActionList
    Left = 8
    Top = 248
    object actStart: TAction
      Caption = 'Start'
      OnExecute = actStartExecute
      OnUpdate = actStartUpdate
    end
    object actStop: TAction
      Caption = 'Stop'
      OnExecute = actStopExecute
      OnUpdate = actStopUpdate
    end
  end
  object oeMonitor: TOmniEventMonitor
    OnTaskMessage = oeMonitorTaskMessage
    Left = 48
    Top = 248
  end
end
