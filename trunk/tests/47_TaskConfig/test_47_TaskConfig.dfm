object frmDemoParallelTaskConfig: TfrmDemoParallelTaskConfig
  Left = 0
  Top = 0
  Caption = 'Parallel.TaskConfig tester'
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
    Left = 8
    Top = 10
    Width = 75
    Height = 25
    Caption = 'Async'
    TabOrder = 0
    OnClick = btnAsyncClick
  end
  object lbLog: TListBox
    Left = 89
    Top = 0
    Width = 546
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnJoin: TButton
    Left = 8
    Top = 41
    Width = 75
    Height = 25
    Caption = 'Join'
    TabOrder = 2
    OnClick = btnJoinClick
  end
  object btnFuture: TButton
    Left = 8
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Future'
    TabOrder = 3
    OnClick = btnFutureClick
  end
  object btnPipeline: TButton
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Pipeline'
    TabOrder = 4
    OnClick = btnPipelineClick
  end
  object btnForEach: TButton
    Left = 8
    Top = 136
    Width = 75
    Height = 25
    Caption = 'ForEach'
    TabOrder = 5
    OnClick = btnForEachClick
  end
  object btnForkJoin: TButton
    Left = 8
    Top = 167
    Width = 75
    Height = 25
    Caption = 'ForkJoin'
    TabOrder = 6
    OnClick = btnForkJoinClick
  end
end
