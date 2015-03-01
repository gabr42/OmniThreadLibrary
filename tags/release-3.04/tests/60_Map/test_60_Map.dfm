object frmTestParallelMap: TfrmTestParallelMap
  Left = 0
  Top = 0
  Caption = 'Parallel.Map tester'
  ClientHeight = 411
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    629
    411)
  PixelsPerInch = 96
  TextHeight = 13
  object btnMap: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Map'
    TabOrder = 0
    OnClick = btnMapClick
  end
  object lbLog: TListBox
    Left = 108
    Top = 8
    Width = 513
    Height = 395
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnMap2: TButton
    Left = 16
    Top = 47
    Width = 75
    Height = 25
    Caption = 'Map'
    TabOrder = 2
    OnClick = btnMap2Click
  end
  object btnSerial: TButton
    Left = 16
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Serial'
    TabOrder = 3
    OnClick = btnSerialClick
  end
  object btnParallel: TButton
    Left = 16
    Top = 151
    Width = 75
    Height = 25
    Caption = 'Parallel'
    TabOrder = 4
    OnClick = btnParallelClick
  end
end
