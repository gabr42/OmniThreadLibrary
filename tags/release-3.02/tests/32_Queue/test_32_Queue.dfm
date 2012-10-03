object frmTestOmniQueue: TfrmTestOmniQueue
  Left = 0
  Top = 0
  Caption = 'TOmniQueue tester'
  ClientHeight = 467
  ClientWidth = 630
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    630
    467)
  PixelsPerInch = 96
  TextHeight = 13
  object lblBlockSize: TLabel
    Left = 8
    Top = 408
    Width = 49
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Block size:'
  end
  object lbLog: TListBox
    Left = 152
    Top = 0
    Width = 478
    Height = 467
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
  end
  object btnTest: TButton
    Left = 8
    Top = 8
    Width = 138
    Height = 25
    Caption = 'Single-threaded test'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object btn2to2: TButton
    Tag = 2
    Left = 8
    Top = 72
    Width = 138
    Height = 25
    Caption = '2 -> 2'
    TabOrder = 2
    OnClick = StartTest
  end
  object btn3to3: TButton
    Tag = 3
    Left = 8
    Top = 104
    Width = 138
    Height = 25
    Caption = '3 -> 3'
    TabOrder = 3
    OnClick = StartTest
  end
  object btn4to4: TButton
    Tag = 4
    Left = 8
    Top = 136
    Width = 138
    Height = 25
    Caption = '4 -> 4'
    TabOrder = 4
    OnClick = StartTest
  end
  object btn1to1: TButton
    Tag = 1
    Left = 8
    Top = 41
    Width = 138
    Height = 25
    Caption = '1 -> 1'
    TabOrder = 1
    OnClick = StartTest
  end
  object btnTestIntf: TButton
    Left = 8
    Top = 434
    Width = 138
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Send interfaces'
    TabOrder = 6
    OnClick = btnTestIntfClick
  end
  object cbRepeat: TCheckBox
    Left = 8
    Top = 270
    Width = 97
    Height = 17
    Caption = 'Repeat'
    TabOrder = 7
  end
  object btn8to8: TButton
    Tag = 8
    Left = 8
    Top = 168
    Width = 138
    Height = 25
    Caption = '8 -> 8'
    TabOrder = 8
    OnClick = StartTest
  end
  object rgCollectionType: TRadioGroup
    Left = 8
    Top = 293
    Width = 138
    Height = 60
    Caption = 'Queue class'
    ItemIndex = 0
    Items.Strings = (
      'TOmniBaseQueue'
      'TOmniQueue')
    TabOrder = 9
  end
  object btn1to7: TButton
    Left = 8
    Top = 200
    Width = 138
    Height = 25
    Caption = '1 -> 7'
    TabOrder = 10
    OnClick = btn1to7Click
  end
  object btn7to1: TButton
    Left = 8
    Top = 232
    Width = 138
    Height = 25
    Caption = '7 -> 1'
    TabOrder = 11
    OnClick = btn7to1Click
  end
  object inpBlockSize: TEdit
    Left = 63
    Top = 405
    Width = 63
    Height = 21
    Anchors = [akLeft, akBottom]
    ReadOnly = True
    TabOrder = 12
  end
  object spinBlockSize: TSpinButton
    Left = 126
    Top = 403
    Width = 20
    Height = 25
    Anchors = [akLeft, akBottom]
    DownGlyph.Data = {
      0E010000424D0E01000000000000360000002800000009000000060000000100
      200000000000D800000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000008080000080800000808000000000000080800000808000008080000080
      8000008080000080800000808000000000000000000000000000008080000080
      8000008080000080800000808000000000000000000000000000000000000000
      0000008080000080800000808000000000000000000000000000000000000000
      0000000000000000000000808000008080000080800000808000008080000080
      800000808000008080000080800000808000}
    TabOrder = 13
    UpGlyph.Data = {
      0E010000424D0E01000000000000360000002800000009000000060000000100
      200000000000D800000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000000000000000000000000000000000000000000000000000000000000080
      8000008080000080800000000000000000000000000000000000000000000080
      8000008080000080800000808000008080000000000000000000000000000080
      8000008080000080800000808000008080000080800000808000000000000080
      8000008080000080800000808000008080000080800000808000008080000080
      800000808000008080000080800000808000}
    OnDownClick = spinBlockSizeDownClick
    OnUpClick = spinBlockSizeUpClick
  end
  object cbTestIsEmpty: TCheckBox
    Left = 8
    Top = 359
    Width = 97
    Height = 17
    Caption = 'Test IsEmpty'
    TabOrder = 14
  end
end
