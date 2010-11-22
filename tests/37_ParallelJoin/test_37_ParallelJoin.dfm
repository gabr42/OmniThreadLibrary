object frmTestParallelJoin: TfrmTestParallelJoin
  Left = 0
  Top = 0
  Caption = 'Parallel.Join demo'
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
  object btnJoinAll: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Join (all cores)'
    TabOrder = 0
    OnClick = btnJoinAllClick
  end
  object btnJointOne: TButton
    Left = 8
    Top = 39
    Width = 105
    Height = 25
    Caption = 'Join (one core)'
    TabOrder = 1
    OnClick = btnJointOneClick
  end
  object lbLog: TListBox
    Left = 119
    Top = 0
    Width = 516
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object btnJoinTProc: TButton
    Left = 8
    Top = 88
    Width = 105
    Height = 25
    Caption = 'Join (TProc)'
    TabOrder = 3
    OnClick = btnJoinTProcClick
  end
end
