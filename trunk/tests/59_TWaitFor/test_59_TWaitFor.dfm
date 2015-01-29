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
  PixelsPerInch = 96
  TextHeight = 13
  object lblNumHandles: TLabel
    Left = 16
    Top = 13
    Width = 65
    Height = 13
    Caption = 'Num handles:'
  end
  object btnWaitForAll: TButton
    Left = 16
    Top = 64
    Width = 129
    Height = 25
    Caption = 'Wait for all handles'
    TabOrder = 0
    OnClick = btnWaitForAllClick
  end
  object lbLog: TListBox
    Left = 160
    Top = 0
    Width = 653
    Height = 392
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnWaitForAny: TButton
    Left = 16
    Top = 95
    Width = 129
    Height = 25
    Caption = 'Wait for any handle'
    TabOrder = 2
    OnClick = btnWaitForAnyClick
  end
  object inpNumHandles: TSpinEdit
    Left = 16
    Top = 32
    Width = 129
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 130
  end
end
