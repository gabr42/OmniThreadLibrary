object frmInvokeAnonymousDemo: TfrmInvokeAnonymousDemo
  Left = 0
  Top = 0
  Caption = 'Invoke(anonymous) demo'
  ClientHeight = 337
  ClientWidth = 635
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
    Left = 119
    Top = 0
    Width = 516
    Height = 337
    Align = alRight
    ItemHeight = 13
    TabOrder = 0
  end
  object btnInvoke: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Invoke'
    TabOrder = 1
    OnClick = btnInvokeClick
  end
  object btnInvokeMonitored: TButton
    Left = 8
    Top = 39
    Width = 105
    Height = 25
    Caption = 'Invoke Monitored'
    TabOrder = 2
    OnClick = btnInvokeClick
  end
  object OmniEventMonitor1: TOmniEventMonitor
    OnTaskMessage = OmniEventMonitor1TaskMessage
    Left = 8
    Top = 72
  end
end
