object dmTwoFishDB: TdmTwoFishDB
  OldCreateOrder = False
  Height = 226
  Width = 215
  object IBDatabase1: TIBDatabase
    DatabaseName = 
      'C:\Users\Public\Documents\RAD Studio\9.0\Samples\Data\dbdemos.gd' +
      'b'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    DefaultTransaction = IBTransaction1
    ServerType = 'IBServer'
    Left = 48
    Top = 64
  end
  object IBTransaction1: TIBTransaction
    DefaultDatabase = IBDatabase1
    Left = 112
    Top = 64
  end
  object IBTable1: TIBTable
    Database = IBDatabase1
    Transaction = IBTransaction1
    FieldDefs = <
      item
        Name = 'SPECIES_NO'
        DataType = ftFloat
      end
      item
        Name = 'CATEGORY'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'COMMON_NAME'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'SPECIES_NAME'
        DataType = ftWideString
        Size = 40
      end
      item
        Name = 'LENGTH__CM_'
        DataType = ftFloat
      end
      item
        Name = 'LENGTH_IN'
        DataType = ftFloat
      end
      item
        Name = 'NOTES'
        DataType = ftWideMemo
        Size = 8
      end
      item
        Name = 'GRAPHIC'
        DataType = ftBlob
        Size = 8
      end>
    IndexDefs = <
      item
        Name = 'BIOLIFE0'
        Fields = 'SPECIES_NO'
        Options = [ixUnique]
      end>
    ReadOnly = True
    StoreDefs = True
    TableName = 'BIOLIFE'
    Left = 48
    Top = 120
    object IBTable1CATEGORY: TIBStringField
      DisplayLabel = 'Category'
      FieldName = 'CATEGORY'
      Size = 15
    end
    object IBTable1SPECIES_NAME: TIBStringField
      DisplayLabel = 'Species Name'
      FieldName = 'SPECIES_NAME'
      Size = 40
    end
    object IBTable1LENGTH__CM_: TFloatField
      DisplayLabel = 'Length (cm)'
      FieldName = 'LENGTH__CM_'
    end
    object IBTable1LENGTH_IN: TFloatField
      DisplayLabel = 'Length_In'
      FieldName = 'LENGTH_IN'
      DisplayFormat = '0.00'
    end
    object IBTable1COMMON_NAME: TIBStringField
      DisplayLabel = 'Common Name'
      FieldName = 'COMMON_NAME'
      Size = 30
    end
    object IBTable1NOTES: TMemoField
      DisplayLabel = 'Notes'
      FieldName = 'NOTES'
      BlobType = ftMemo
      Size = 8
    end
    object IBTable1GRAPHIC: TBlobField
      DisplayLabel = 'Graphic'
      FieldName = 'GRAPHIC'
      Size = 8
    end
  end
end
