object frmRunInvokeTester: TfrmRunInvokeTester
  Left = 0
  Top = 0
  Caption = 'Run(Invoke) tester'
  ClientHeight = 243
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnLongWay: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Long way'
    TabOrder = 0
    OnClick = BtnLongWayClick
  end
  object BtnShortWay: TButton
    Left = 112
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Short way'
    TabOrder = 1
    OnClick = BtnShortWayClick
  end
  object LBLog: TListBox
    Left = 0
    Top = 56
    Width = 527
    Height = 187
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
end
