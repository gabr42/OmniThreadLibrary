object Form34: TForm34
  Left = 0
  Top = 0
  Caption = 'Exceptions in OtlParallel'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnAsync: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Async'
    Enabled = False
    TabOrder = 0
  end
  object btnFuture1: TButton
    Left = 16
    Top = 78
    Width = 91
    Height = 25
    Caption = 'Future 1'
    TabOrder = 1
    OnClick = btnFuture1Click
  end
  object btnJoin: TButton
    Left = 16
    Top = 47
    Width = 91
    Height = 25
    Caption = 'Join'
    TabOrder = 2
    OnClick = btnJoinClick
  end
  object btnForeach: TButton
    Left = 16
    Top = 171
    Width = 75
    Height = 25
    Caption = 'ForEach'
    Enabled = False
    TabOrder = 3
  end
  object btnPipeline: TButton
    Left = 16
    Top = 202
    Width = 75
    Height = 25
    Caption = 'Pipeline'
    Enabled = False
    TabOrder = 4
  end
  object btnForkJoin: TButton
    Left = 16
    Top = 233
    Width = 75
    Height = 25
    Caption = 'Fork/Join'
    Enabled = False
    TabOrder = 5
  end
  object lbLog: TListBox
    Left = 128
    Top = 0
    Width = 507
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 6
  end
  object btnFuture2: TButton
    Left = 16
    Top = 109
    Width = 91
    Height = 25
    Caption = 'Future 2'
    TabOrder = 7
    OnClick = btnFuture2Click
  end
  object btnFuture3: TButton
    Left = 16
    Top = 140
    Width = 91
    Height = 25
    Caption = 'Future 3'
    TabOrder = 8
    OnClick = btnFuture3Click
  end
end
