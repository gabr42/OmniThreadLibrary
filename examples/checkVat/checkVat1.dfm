object frmCheckVat: TfrmCheckVat
  Left = 0
  Top = 0
  Caption = 'Check VAT'
  ClientHeight = 243
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    340
    243)
  PixelsPerInch = 96
  TextHeight = 13
  object inpCC: TLabeledEdit
    Left = 16
    Top = 32
    Width = 73
    Height = 21
    EditLabel.Width = 67
    EditLabel.Height = 13
    EditLabel.Caption = 'Country Code'
    TabOrder = 0
    Text = 'SI'
  end
  object inpVAT: TLabeledEdit
    Left = 104
    Top = 32
    Width = 129
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 19
    EditLabel.Height = 13
    EditLabel.Caption = 'VAT'
    TabOrder = 1
    Text = '52075800'
  end
  object btnCheckVat: TButton
    Left = 247
    Top = 30
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Check VAT'
    TabOrder = 2
    OnClick = btnCheckVatClick
  end
  object outVatInfo: TMemo
    Left = 16
    Top = 72
    Width = 306
    Height = 153
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
end
