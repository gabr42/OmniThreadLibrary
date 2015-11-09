object frmUpdateProgressBar: TfrmUpdateProgressBar
  Left = 0
  Top = 0
  Caption = 'Parallel Progress Bar'
  ClientHeight = 132
  ClientWidth = 721
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pbParallel: TProgressBar
    Left = 0
    Top = 115
    Width = 721
    Height = 17
    Align = alBottom
    TabOrder = 0
  end
  object btnPPLMessage: TButton
    Left = 16
    Top = 16
    Width = 169
    Height = 25
    Caption = 'PPL w/ Message'
    TabOrder = 1
    OnClick = btnPPLMessageClick
  end
  object btnPPLWithQueue: TButton
    Left = 16
    Top = 47
    Width = 169
    Height = 25
    Caption = 'PPL w/ Queue'
    TabOrder = 2
    OnClick = btnPPLWithQueueClick
  end
  object btnOTLWithMessage: TButton
    Left = 280
    Top = 16
    Width = 169
    Height = 25
    Caption = 'OTL w/ Message'
    TabOrder = 3
    OnClick = btnOTLWithMessageClick
  end
  object btnPPLWithTimer: TButton
    Left = 16
    Top = 78
    Width = 169
    Height = 25
    Caption = 'PPL w/ Timer'
    TabOrder = 4
    OnClick = btnPPLWithTimerClick
  end
  object btnOTLWithTimer: TButton
    Left = 280
    Top = 78
    Width = 169
    Height = 25
    Caption = 'OTL w/ Timer'
    TabOrder = 5
    OnClick = btnOTLWithTimerClick
  end
  object btnPPLAsyncAwait: TButton
    Left = 535
    Top = 16
    Width = 169
    Height = 25
    Caption = 'PPL Async/Await'
    TabOrder = 6
    OnClick = btnPPLAsyncAwaitClick
  end
  object btnTThreadAsyncAwait: TButton
    Left = 535
    Top = 47
    Width = 169
    Height = 25
    Caption = 'TThread Async/Await'
    TabOrder = 7
    OnClick = btnTThreadAsyncAwaitClick
  end
  object tmrUpdateProgress: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrUpdateProgressTimer
    Left = 224
    Top = 72
  end
end
