object frmTThreadComm: TfrmTThreadComm
  Left = 0
  Top = 0
  Caption = 'TThread communication demo'
  ClientHeight = 584
  ClientWidth = 861
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
  object btn: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = '42'
    TabOrder = 0
    OnClick = btnClick
  end
  object lbLog: TListBox
    Left = 0
    Top = 56
    Width = 861
    Height = 528
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object Button1: TButton
    Left = 112
    Top = 16
    Width = 129
    Height = 25
    Caption = 'Two random queries'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 264
    Top = 16
    Width = 153
    Height = 25
    Caption = 'Two threaded queries'
    TabOrder = 3
    OnClick = Button2Click
  end
end
