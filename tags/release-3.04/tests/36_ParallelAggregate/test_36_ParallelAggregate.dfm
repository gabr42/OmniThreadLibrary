object frmParallelAggregateDemo: TfrmParallelAggregateDemo
  Left = 0
  Top = 0
  Caption = 'Parallel..Aggregate demo'
  ClientHeight = 243
  ClientWidth = 537
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
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 114
    Height = 13
    Caption = 'Sum numbers from 1 to '
  end
  object lblCountPrimes: TLabel
    Left = 16
    Top = 56
    Width = 110
    Height = 13
    Caption = 'Count primes from 1 to'
  end
  object Label3: TLabel
    Left = 473
    Top = 16
    Width = 48
    Height = 13
    Caption = 'Num CPU:'
  end
  object inpMaxSummand: TSpinEdit
    Left = 135
    Top = 13
    Width = 73
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 1000000
  end
  object btnSumParallel: TButton
    Left = 295
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Parallel'
    TabOrder = 4
    OnClick = btnSumParallelClick
  end
  object lbLog: TListBox
    Left = 0
    Top = 88
    Width = 537
    Height = 155
    Align = alBottom
    ItemHeight = 13
    TabOrder = 7
  end
  object btnSumSequential: TButton
    Left = 214
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Sequential'
    TabOrder = 3
    OnClick = btnSumSequentialClick
  end
  object inpNumCPU: TSpinEdit
    Left = 473
    Top = 35
    Width = 57
    Height = 22
    MaxValue = 0
    MinValue = 1
    TabOrder = 2
    Value = 1
  end
  object inpMaxPrime: TSpinEdit
    Left = 136
    Top = 53
    Width = 73
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 1000000
  end
  object btnCountParallel: TButton
    Left = 296
    Top = 51
    Width = 75
    Height = 25
    Caption = 'Parallel'
    TabOrder = 6
    OnClick = btnCountParallelClick
  end
  object btnCountSequential: TButton
    Left = 215
    Top = 51
    Width = 75
    Height = 25
    Caption = 'Sequential'
    TabOrder = 5
    OnClick = btnCountSequentialClick
  end
  object btnSumParallel2: TButton
    Left = 376
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Parallel 2'
    TabOrder = 8
    OnClick = btnSumParallel2Click
  end
  object btnCountParallel2: TButton
    Left = 377
    Top = 51
    Width = 75
    Height = 25
    Caption = 'Parallel 2'
    TabOrder = 9
    OnClick = btnCountParallel2Click
  end
end
