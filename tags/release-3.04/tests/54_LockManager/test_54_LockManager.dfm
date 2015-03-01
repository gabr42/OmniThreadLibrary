object frmTestLockManager: TfrmTestLockManager
  Left = 0
  Top = 0
  Caption = 'Lock Manager test'
  ClientHeight = 325
  ClientWidth = 527
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
  object ListBox1: TListBox
    Left = 89
    Top = 0
    Width = 438
    Height = 325
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
  object btnSafe: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Safe'
    TabOrder = 2
    OnClick = btnSafeClick
  end
  object btnMonitor: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Monitor'
    TabOrder = 3
    OnClick = btnMonitorClick
  end
  object btnReentrancy: TButton
    Left = 8
    Top = 218
    Width = 75
    Height = 25
    Caption = 'Reentrancy'
    TabOrder = 4
    OnClick = btnReentrancyClick
  end
  object btnUnsafe1: TButton
    Tag = 1
    Left = 8
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Unsafe 1'
    TabOrder = 5
    OnClick = btnUnsafeClick
  end
  object btnSafe1: TButton
    Tag = 1
    Left = 8
    Top = 143
    Width = 75
    Height = 25
    Caption = 'Safe 1'
    TabOrder = 6
    OnClick = btnSafeClick
  end
  object btnMonitor1: TButton
    Tag = 1
    Left = 8
    Top = 174
    Width = 75
    Height = 25
    Caption = 'Monitor 1'
    TabOrder = 7
    OnClick = btnMonitorClick
  end
  object btnNoCollisions: TButton
    Left = 8
    Top = 264
    Width = 75
    Height = 25
    Caption = 'No collisions'
    TabOrder = 8
    OnClick = btnNoCollisionsClick
  end
  object btnNoCollisionsMonitor: TButton
    Left = 8
    Top = 295
    Width = 75
    Height = 25
    Caption = 'No coll. (Mon)'
    TabOrder = 9
    OnClick = btnNoCollisionsMonitorClick
  end
end
