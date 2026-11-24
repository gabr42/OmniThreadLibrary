object frmTestDLL: TfrmTestDLL
  Left = 0
  Top = 0
  Caption = 'OmniThreadLibrary in a DLL'
  ClientHeight = 300
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    640
    300)
  PixelsPerInch = 96
  TextHeight = 13
  object lblProgress: TLabel
    Left = 8
    Top = 176
    Width = 3
    Height = 13
  end
  object btnLoad: TButton
    Left = 8
    Top = 8
    Width = 177
    Height = 25
    Caption = 'Load DLL'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object btnStart: TButton
    Left = 8
    Top = 40
    Width = 177
    Height = 25
    Caption = 'Start job'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStartStubborn: TButton
    Left = 8
    Top = 72
    Width = 177
    Height = 25
    Caption = 'Start job that ignores Cancel'
    TabOrder = 2
    OnClick = btnStartStubbornClick
  end
  object btnCancel: TButton
    Left = 8
    Top = 104
    Width = 177
    Height = 25
    Caption = 'Cancel jobs'
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object btnUnload: TButton
    Left = 8
    Top = 136
    Width = 177
    Height = 25
    Caption = 'Unload DLL'
    TabOrder = 4
    OnClick = btnUnloadClick
  end
  object lbLog: TListBox
    Left = 194
    Top = 8
    Width = 438
    Height = 284
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
  end
end
