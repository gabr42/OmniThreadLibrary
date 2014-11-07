object frmTestTWaitFor: TfrmTestTWaitFor
  Left = 0
  Top = 0
  Caption = 'TWaitFor tester'
  ClientHeight = 392
  ClientWidth = 813
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
  object btnWaitForAll: TButton
    Left = 16
    Top = 16
    Width = 129
    Height = 25
    Caption = 'Wait for all handles'
    TabOrder = 0
    OnClick = btnWaitForAllClick
  end
  object lbLog: TListBox
    Left = 168
    Top = 0
    Width = 645
    Height = 392
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnWaitForAny: TButton
    Left = 16
    Top = 47
    Width = 129
    Height = 25
    Caption = 'Wait for any handle'
    TabOrder = 2
    OnClick = btnWaitForAnyClick
  end
end
