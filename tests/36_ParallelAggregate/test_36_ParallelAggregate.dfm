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
  object Label2: TLabel
    Left = 296
    Top = 16
    Width = 34
    Height = 13
    Caption = 'Result:'
  end
  object inpMaxSummand: TSpinEdit
    Left = 136
    Top = 13
    Width = 57
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
  object btnSumNumbers: TButton
    Left = 199
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 1
    OnClick = btnSumNumbersClick
  end
  object Button2: TButton
    Left = 216
    Top = 155
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
  end
  object outSum: TEdit
    Left = 336
    Top = 13
    Width = 97
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 3
  end
end
