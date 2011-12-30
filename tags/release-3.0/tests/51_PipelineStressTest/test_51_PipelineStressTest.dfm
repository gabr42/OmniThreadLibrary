object frmPipelineStressTest: TfrmPipelineStressTest
  Left = 0
  Top = 0
  Caption = 'Pipeline stress test'
  ClientHeight = 285
  ClientWidth = 418
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
  object lblStatus: TLabel
    Left = 16
    Top = 72
    Width = 41
    Height = 13
    Caption = 'lblStatus'
  end
  object btStartStop: TButton
    Left = 144
    Top = 32
    Width = 97
    Height = 25
    Caption = 'Run test'
    TabOrder = 0
    OnClick = btStartStopClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 96
    Width = 402
    Height = 169
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
    WordWrap = False
  end
end
