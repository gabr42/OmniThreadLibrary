object frmParallelAggregateDemo: TfrmParallelAggregateDemo
  Left = 0
  Top = 0
  Caption = 'Parallel..Aggregate demo'
  ClientHeight = 243
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 114
    Height = 13
    Caption = 'Sum numbers from 1 to '
  end
  object inpMaxSummand: TSpinEdit
    Left = 136
    Top = 13
    Width = 57
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 100
  end
  object btnSumParallel: TButton
    Left = 279
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Parallel'
    TabOrder = 1
    OnClick = btnSumParallelClick
  end
  object lbLog: TListBox
    Left = 0
    Top = 64
    Width = 450
    Height = 179
    Align = alBottom
    ItemHeight = 13
    TabOrder = 2
  end
  object btnSumSerial: TButton
    Left = 198
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Serial'
    TabOrder = 3
    OnClick = btnSumSerialClick
  end
end
