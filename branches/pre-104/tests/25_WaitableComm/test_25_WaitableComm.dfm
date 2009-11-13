object frmWaitableCommDemo: TfrmWaitableCommDemo
  Left = 0
  Top = 0
  Caption = 'Waitable Comm Demo'
  ClientHeight = 245
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    472
    245)
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    AlignWithMargins = True
    Left = 89
    Top = 0
    Width = 383
    Height = 245
    Margins.Left = 0
    Margins.Top = 89
    Margins.Right = 0
    Margins.Bottom = 0
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnReceive: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'ReceiveWait'
    TabOrder = 1
    OnClick = btnReceiveClick
  end
  object btnSendWait: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'SendWait'
    TabOrder = 2
    OnClick = btnSendWaitClick
  end
end
