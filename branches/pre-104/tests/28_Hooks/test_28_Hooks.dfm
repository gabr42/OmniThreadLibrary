object frmHooksDemo: TfrmHooksDemo
  Left = 0
  Top = 0
  Caption = 'Hooks demo'
  ClientHeight = 245
  ClientWidth = 472
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
    Left = 89
    Top = 0
    Width = 383
    Height = 245
    Align = alRight
    ItemHeight = 13
    TabOrder = 0
  end
  object btnRunTask: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Run task'
    TabOrder = 1
    OnClick = btnRunTaskClick
  end
  object OtlMonitor: TOmniEventMonitor
    Left = 8
    Top = 208
  end
end
