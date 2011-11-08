object frmOmniValueArray: TfrmOmniValueArray
  Left = 0
  Top = 0
  Caption = 'OmniValueArray demo'
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
  object lbLog: TListBox
    Left = 120
    Top = 0
    Width = 515
    Height = 337
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnSendHash: TButton
    Left = 8
    Top = 8
    Width = 106
    Height = 25
    Caption = 'Send Hash'
    TabOrder = 1
    OnClick = btnSendHashClick
  end
  object btnSendArray: TButton
    Left = 8
    Top = 40
    Width = 106
    Height = 25
    Caption = 'Send Array'
    TabOrder = 2
    OnClick = btnSendArrayClick
  end
  object btnSendRecord: TButton
    Left = 8
    Top = 72
    Width = 106
    Height = 25
    Caption = 'Send Record'
    TabOrder = 3
    OnClick = btnSendRecordClick
  end
end
