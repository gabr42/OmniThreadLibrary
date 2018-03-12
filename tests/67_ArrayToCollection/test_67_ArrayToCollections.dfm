object frmArrayToCollection: TfrmArrayToCollection
  Left = 0
  Top = 0
  Caption = 'TOmniBlockingCollection.FromArray tester'
  ClientHeight = 343
  ClientWidth = 610
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    610
    343)
  PixelsPerInch = 96
  TextHeight = 13
  object btnTestInteger: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = 'FromArray<integer>'
    TabOrder = 0
    OnClick = btnTestIntegerClick
  end
  object btnTestString: TButton
    Left = 8
    Top = 48
    Width = 113
    Height = 25
    Caption = 'FromArray<string>'
    TabOrder = 1
    OnClick = btnTestStringClick
  end
  object btnTestRecord: TButton
    Left = 8
    Top = 89
    Width = 113
    Height = 25
    Caption = 'FromArray<record>'
    TabOrder = 2
    OnClick = btnTestRecordClick
  end
  object btnTestObject: TButton
    Left = 8
    Top = 128
    Width = 113
    Height = 25
    Caption = 'FromArray<object>'
    TabOrder = 3
    OnClick = btnTestObjectClick
  end
  object btnTestInterface: TButton
    Left = 8
    Top = 168
    Width = 113
    Height = 25
    Caption = 'FromArray<intf>'
    TabOrder = 4
    OnClick = btnTestInterfaceClick
  end
  object lbLog: TListBox
    Left = 132
    Top = 8
    Width = 470
    Height = 327
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
  end
end
