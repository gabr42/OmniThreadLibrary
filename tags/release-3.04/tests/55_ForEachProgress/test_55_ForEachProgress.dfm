object frmForEachWithProgressBar: TfrmForEachWithProgressBar
  Left = 0
  Top = 0
  Caption = 'ForEach w/ progress bar'
  ClientHeight = 135
  ClientWidth = 496
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pbForEach: TProgressBar
    Left = 24
    Top = 24
    Width = 449
    Height = 17
    TabOrder = 0
  end
  object btnStart: TButton
    Left = 171
    Top = 64
    Width = 153
    Height = 49
    Caption = 'Start'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnStartClick
  end
end
