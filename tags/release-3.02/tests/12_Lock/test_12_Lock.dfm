object frmTestLock: TfrmTestLock
  Left = 0
  Top = 0
  Caption = 'Lock tester'
  ClientHeight = 306
  ClientWidth = 611
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 143
    Top = 0
    Width = 468
    Height = 306
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnNoLock: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'No lock'
    TabOrder = 1
    OnClick = btnNoLockClick
  end
  object btnLock: TButton
    Left = 8
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Lock'
    TabOrder = 2
    OnClick = btnNoLockClick
  end
  object btnTestLock: TButton
    Left = 8
    Top = 117
    Width = 129
    Height = 25
    Caption = 'Test lock'
    TabOrder = 3
    OnClick = btnTestLockClick
  end
  object btnTestNoLock: TButton
    Left = 8
    Top = 86
    Width = 129
    Height = 25
    Caption = 'Test no lock'
    TabOrder = 4
    OnClick = btnTestLockClick
  end
  object OmniTED: TOmniEventMonitor
    Left = 8
    Top = 264
  end
end
