object frmTestWaitableObjects: TfrmTestWaitableObjects
  Left = 0
  Top = 0
  Caption = 'Waitable Objects demo'
  ClientHeight = 343
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnSignal1: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Signal event 1'
    TabOrder = 0
    OnClick = btnSignal1Click
  end
  object btnSignal2: TButton
    Left = 8
    Top = 39
    Width = 105
    Height = 25
    Caption = 'Signal event 2'
    TabOrder = 1
    OnClick = btnSignal2Click
  end
  object lbLog: TListBox
    Left = 119
    Top = 0
    Width = 463
    Height = 343
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object btnUnregister1: TButton
    Left = 8
    Top = 96
    Width = 105
    Height = 25
    Caption = 'Unregister event 1'
    TabOrder = 3
    OnClick = btnUnregister1Click
  end
  object btnRegister1: TButton
    Left = 8
    Top = 127
    Width = 105
    Height = 25
    Caption = 'Register event 1'
    TabOrder = 4
    OnClick = btnRegister1Click
  end
end
