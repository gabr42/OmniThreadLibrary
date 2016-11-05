object frmFolderScanner: TfrmFolderScanner
  Left = 0
  Top = 0
  Caption = 'Folder Scanner / Pipeline demo'
  ClientHeight = 336
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    643
    336)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFolder: TLabel
    Left = 16
    Top = 24
    Width = 34
    Height = 13
    Caption = 'Folder:'
  end
  object lblNumScanners: TLabel
    Left = 16
    Top = 56
    Width = 84
    Height = 13
    Caption = 'Parallel scanners:'
  end
  object inpFolder: TEdit
    Left = 56
    Top = 21
    Width = 489
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnSelect: TButton
    Left = 551
    Top = 19
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Select'
    TabOrder = 1
    OnClick = btnSelectClick
  end
  object btnStart: TButton
    Left = 241
    Top = 53
    Width = 161
    Height = 49
    Action = actStart
    Anchors = [akTop]
    TabOrder = 2
  end
  object inpNumScanners: TSpinEdit
    Left = 106
    Top = 53
    Width = 63
    Height = 22
    MaxValue = 64
    MinValue = 1
    TabOrder = 3
    Value = 4
  end
  object lbLog: TListBox
    Left = 0
    Top = 120
    Width = 643
    Height = 216
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 4
  end
  object ActionList: TActionList
    Left = 448
    Top = 56
    object actStart: TAction
      Caption = 'Start'
      OnExecute = actStartExecute
      OnUpdate = actStartUpdate
    end
    object actStop: TAction
      Caption = 'Stop'
      OnExecute = actStopExecute
    end
  end
end
