object frmTestHelloWorld: TfrmTestHelloWorld
  Left = 0
  Top = 0
  Caption = 'Hello World'
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
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 104
    Top = 0
    Width = 322
    Height = 286
    Align = alRight
    ItemHeight = 13
    TabOrder = 0
  end
  object btnHello: TButton
    Left = 8
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Hello, world!'
    TabOrder = 1
    OnClick = btnHelloClick
  end
end
