object Form16: TForm16
  Left = 0
  Top = 0
  Caption = 'Form16'
  ClientHeight = 243
  ClientWidth = 527
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
  object btnWork: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Work!'
    TabOrder = 0
    OnClick = btnWorkClick
  end
  object lbLog: TListBox
    Left = 89
    Top = 0
    Width = 438
    Height = 243
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
end
