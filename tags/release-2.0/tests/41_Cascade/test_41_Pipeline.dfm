object frmPipelineDemo: TfrmPipelineDemo
  Left = 0
  Top = 0
  Caption = 'Cascade/pipeline demo'
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
  object lbLog: TListBox
    Left = 119
    Top = 0
    Width = 516
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnPipeline: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Parallel.Pipeline'
    TabOrder = 1
    OnClick = btnPipelineClick
  end
  object btnPipelineNoWait: TButton
    Left = 8
    Top = 39
    Width = 105
    Height = 25
    Caption = 'Pipeline.NoWait'
    TabOrder = 2
    OnClick = btnPipelineNoWaitClick
  end
end
