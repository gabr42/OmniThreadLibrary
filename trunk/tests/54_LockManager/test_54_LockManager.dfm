object frmTestLockManager: TfrmTestLockManager
  Left = 0
  Top = 0
  Caption = 'Lock Manager test'
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
  object ListBox1: TListBox
    Left = 89
    Top = 0
    Width = 438
    Height = 243
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnUnsafe: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Unsafe'
    TabOrder = 1
    OnClick = btnUnsafeClick
  end
  object Safe: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Safe'
    TabOrder = 2
    OnClick = SafeClick
  end
end
