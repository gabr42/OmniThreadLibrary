object frmAsyncAwaitTest: TfrmAsyncAwaitTest
  Left = 0
  Top = 0
  Caption = 'Async/Await test'
  ClientHeight = 70
  ClientWidth = 301
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnSleep1: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Sleep'
    TabOrder = 0
    OnClick = btnSleep1Click
  end
  object btnSleep2: TButton
    Left = 112
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Sleep'
    TabOrder = 1
    OnClick = btnSleep1Click
  end
  object btnSleep3: TButton
    Left = 200
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Sleep'
    TabOrder = 2
    OnClick = btnSleep1Click
  end
end
