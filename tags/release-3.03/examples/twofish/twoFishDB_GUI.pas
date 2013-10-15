unit twoFishDB_GUI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  Datasnap.DBClient, Vcl.DBCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  OtlCommon,
  OtlSync,
  OtlParallel,
  twoFishDB;

type
  TNotify = reference to procedure(Sender: TObject; FatalException: Exception);

  TfrmTwoFishDB_GUI = class(TFrame)
    Button1    : TButton;
    DataSource1: TDataSource;
    DBGrid1    : TDBGrid;
    DBImage1   : TDBImage;
    DBMemo1    : TDBMemo;
    DBText1    : TDBText;
    pnlCaption : TPanel;
    pnlMemo    : TPanel;
    pnlTop     : TPanel;
    procedure Button1Click(Sender: TObject);
  private
    FDataModule : TdmTwoFishDB;
    FDataSet    : TClientDataSet;
    FWorker     : IOmniBackgroundWorker;
  strict protected
    procedure ConnectToDatabase(const workItem: IOmniWorkItem);
    procedure DisplayData(const Sender: IOmniBackgroundWorker;
      const workItem: IOmniWorkItem);
    procedure FinalizeDatabase(const taskState: TOmniValue);
    procedure InitializeDatabase(var taskState: TOmniValue);
    procedure LoadData(const workItem: IOmniWorkItem);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure CloseConnection;
    procedure OpenConnection(const databaseName: string; onConnectionOpen: TNotify = nil);
    procedure Reload;
  end;

var
  GTwoFishLock: TOmniCS;

implementation

uses
  Datasnap.Provider;

{$R *.dfm}

{ TfrmTwoFishDB_GUI }

procedure TfrmTwoFishDB_GUI.AfterConstruction;
begin
  inherited;
  FWorker := Parallel.BackgroundWorker
    .Initialize(InitializeDatabase)
    .Finalize(FinalizeDatabase)
    .Execute;
end;

procedure TfrmTwoFishDB_GUI.BeforeDestruction;
begin
  CloseConnection;
  inherited;
end;

procedure TfrmTwoFishDB_GUI.Button1Click(Sender: TObject);
begin
  Reload;
end;

procedure TfrmTwoFishDB_GUI.CloseConnection;
begin
  if assigned(FWorker) then begin
    FWorker.Terminate(INFINITE);
    FWorker := nil;
  end;
  FreeAndNil(FDataSet);
end;

procedure TfrmTwoFishDB_GUI.ConnectToDatabase(const workItem: IOmniWorkItem);
var
  dataModule: TdmTwoFishDB;
begin
  dataModule := (workItem.TaskState.AsObject as TdmTwoFishDB);
  GTwoFishLock.Acquire; //probably only necessary when using InterBase driver
  try
    dataModule.IBDatabase1.DatabaseName := workItem.Data.AsString;
    dataModule.IBDatabase1.Connected := true;
  finally GTwoFishLock.Release; end;
end;

procedure TfrmTwoFishDB_GUI.DisplayData(const Sender: IOmniBackgroundWorker;
  const workItem: IOmniWorkItem);
begin
  FreeAndNil(FDataSet);

  if workItem.IsExceptional then
    ShowMessage('Failed to retrieve data. ' + workItem.FatalException.Message)
  else begin
    FDataSet := workItem.Result.AsObject as TClientDataSet;
    DataSource1.DataSet := FDataSet;
  end;
end;

procedure TfrmTwoFishDB_GUI.FinalizeDatabase(const taskState: TOmniValue);
begin
  FreeAndNil(FDataModule)
end;

procedure TfrmTwoFishDB_GUI.InitializeDatabase(var taskState: TOmniValue);
begin
  FDataModule := TdmTwoFishDB.Create(nil);
  taskState := FDataModule;
end;

procedure TfrmTwoFishDB_GUI.LoadData(const workItem: IOmniWorkItem);
var
  dataModule  : TdmTwoFishDB;
  resultDS    : TClientDataSet;
  tempProvider: TDataSetProvider;
begin
  dataModule := (workItem.TaskState.AsObject as TdmTwoFishDB);
  if not dataModule.IBTable1.Active then
    dataModule.IBTable1.Active := true
  else
    dataModule.IBTable1.Refresh;

  resultDS := TClientDataSet.Create(nil);

  //http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/devwin32/fhxr18643_xml.html
  tempProvider := TDataSetProvider.Create(nil);
  try
    tempProvider.DataSet := dataModule.IBTable1;
    resultDS.Data := tempProvider.Data; //Exception "IBTable1: Field 'SPECIES_NO' not found" is expected. It is handled internally in Data.DB and in IBTable.
  finally FreeAndNil(tempProvider); end;

  workItem.Result := resultDS; // receiver will take ownershipt
end;

procedure TfrmTwoFishDB_GUI.OpenConnection(const databaseName: string;
  onConnectionOpen: TNotify);
begin
  FWorker.Schedule(
    FWorker.CreateWorkItem(databaseName),
    FWorker.Config.OnExecute(ConnectToDatabase).OnRequestDone(
      procedure (const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem)
      begin
        if assigned(onConnectionOpen) then
          onConnectionOpen(Self, workItem.FatalException);
      end
    ));
end;

procedure TfrmTwoFishDB_GUI.Reload;
begin
  FWorker.Schedule(
    FWorker.CreateWorkItem(TOmniValue.Null),
    FWorker.Config.OnExecute(LoadData).OnRequestDone(DisplayData)
  );
end;

end.
