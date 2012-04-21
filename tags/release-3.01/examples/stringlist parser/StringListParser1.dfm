object frmStringListParser: TfrmStringListParser
  Left = 0
  Top = 0
  Caption = 'StringList Parser'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblInstructions: TLabel
    Left = 8
    Top = 35
    Width = 410
    Height = 26
    Caption = 
      'Enter '#39'!'#39' in the edit above (anywhere in the string) to simulate' +
      ' cancellation from the task worker'
    WordWrap = True
  end
  object lbLog: TListBox
    Left = 0
    Top = 68
    Width = 635
    Height = 269
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object inpString: TEdit
    Left = 8
    Top = 8
    Width = 410
    Height = 21
    TabOrder = 1
    Text = 'DelphiDelphiDelphiDelphi'
  end
  object btnProcess: TButton
    Left = 424
    Top = 6
    Width = 122
    Height = 25
    Caption = 'Process (Low Level)'
    TabOrder = 2
    OnClick = btnProcessClick
  end
  object btnCancelLL: TButton
    Left = 552
    Top = 6
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelLLClick
  end
  object btnProcessHL: TButton
    Left = 424
    Top = 37
    Width = 122
    Height = 25
    Caption = 'Process (High Level)'
    TabOrder = 4
    OnClick = btnProcessHLClick
  end
  object btnCancelHL: TButton
    Left = 552
    Top = 37
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = btnCancelHLClick
  end
end
