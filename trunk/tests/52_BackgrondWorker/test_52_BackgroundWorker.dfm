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
  object btnWork3: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Work x3!'
    TabOrder = 2
    OnClick = btnWork3Click
  end
  object btnException: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Exception'
    TabOrder = 3
    OnClick = btnExceptionClick
  end
  object btnCancel: TButton
    Left = 8
    Top = 101
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = btnCancelClick
  end
  object btnCancelAll: TButton
    Left = 8
    Top = 132
    Width = 75
    Height = 25
    Caption = 'Cancel all'
    TabOrder = 5
    OnClick = btnCancelAllClick
  end
  object btnCancel2: TButton
    Left = 8
    Top = 163
    Width = 75
    Height = 25
    Caption = 'Cancel 2'
    TabOrder = 6
    OnClick = btnCancel2Click
  end
end
