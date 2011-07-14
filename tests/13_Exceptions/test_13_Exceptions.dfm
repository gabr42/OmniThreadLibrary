object frmTestExceptions: TfrmTestExceptions
  Left = 0
  Top = 0
  Caption = 'Exception tester'
  ClientHeight = 306
  ClientWidth = 611
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
  object lbLog: TListBox
    Left = 143
    Top = 0
    Width = 468
    Height = 306
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnAV: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Access violation'
    TabOrder = 1
    OnClick = RunTest
  end
  object btnRC: TButton
    Left = 8
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Range check'
    TabOrder = 2
    OnClick = RunTest
  end
  object btnCustomException: TButton
    Left = 8
    Top = 70
    Width = 129
    Height = 25
    Caption = 'Custom'
    TabOrder = 3
    OnClick = RunTest
  end
  object btnInitException: TButton
    Left = 8
    Top = 118
    Width = 129
    Height = 25
    Caption = 'Init exception'
    TabOrder = 4
    OnClick = RunObjectTest
  end
  object btnCleanupException: TButton
    Left = 8
    Top = 149
    Width = 129
    Height = 25
    Caption = 'Cleanup exception'
    TabOrder = 5
    OnClick = RunObjectTest
  end
  object cbThreadPool: TCheckBox
    Left = 8
    Top = 223
    Width = 97
    Height = 17
    Caption = 'Use thread pool'
    TabOrder = 6
  end
  object OmniTED: TOmniEventMonitor
    OnTaskMessage = OmniTEDTaskMessage
    Left = 8
    Top = 264
  end
end
