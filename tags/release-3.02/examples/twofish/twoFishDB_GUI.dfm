object frmTwoFishDB_GUI: TfrmTwoFishDB_GUI
  Left = 0
  Top = 0
  Width = 512
  Height = 400
  TabOrder = 0
  object DBGrid1: TDBGrid
    Left = 0
    Top = 177
    Width = 512
    Height = 223
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 177
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object DBImage1: TDBImage
      Left = 0
      Top = 0
      Width = 209
      Height = 177
      Align = alLeft
      DataField = 'GRAPHIC'
      DataSource = DataSource1
      QuickDraw = False
      Stretch = True
      TabOrder = 0
    end
    object pnlMemo: TPanel
      Left = 209
      Top = 0
      Width = 303
      Height = 177
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object pnlCaption: TPanel
        Left = 0
        Top = 0
        Width = 303
        Height = 34
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object DBText1: TDBText
          Left = 6
          Top = 7
          Width = 211
          Height = 20
          Alignment = taCenter
          DataField = 'COMMON_NAME'
          DataSource = DataSource1
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Button1: TButton
          Left = 222
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Reload'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
      object DBMemo1: TDBMemo
        Left = 0
        Top = 34
        Width = 303
        Height = 143
        Align = alClient
        DataField = 'NOTES'
        DataSource = DataSource1
        TabOrder = 1
      end
    end
  end
  object DataSource1: TDataSource
    AutoEdit = False
    Left = 40
    Top = 8
  end
end
