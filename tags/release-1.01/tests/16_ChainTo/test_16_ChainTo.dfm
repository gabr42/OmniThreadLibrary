object frmTestChainTo: TfrmTestChainTo
  Left = 0
  Top = 0
  Caption = 'ChainTo tester'
  ClientHeight = 306
  ClientWidth = 611
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
    Left = 143
    Top = 0
    Width = 468
    Height = 306
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnStartTasks: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Start tasks'
    TabOrder = 1
    OnClick = btnStartTasksClick
  end
  object OmniTED: TOmniEventMonitor
    OnTaskMessage = OmniTEDTaskMessage
    OnTaskTerminated = OmniTEDTaskTerminated
    Left = 8
    Top = 264
  end
end
