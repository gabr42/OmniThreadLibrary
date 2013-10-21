unit twoFishDB;

interface

uses
  System.SysUtils, System.Classes, Data.DB, IBCustomDataSet, IBTable, IBDatabase;

type
  TdmTwoFishDB = class(TDataModule)
    IBDatabase1: TIBDatabase;
    IBTransaction1: TIBTransaction;
    IBTable1: TIBTable;
    IBTable1CATEGORY: TIBStringField;
    IBTable1SPECIES_NAME: TIBStringField;
    IBTable1LENGTH__CM_: TFloatField;
    IBTable1LENGTH_IN: TFloatField;
    IBTable1COMMON_NAME: TIBStringField;
    IBTable1NOTES: TMemoField;
    IBTable1GRAPHIC: TBlobField;
  private
  public
  end;

var
  dmTwoFishDB: TdmTwoFishDB;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

end.
