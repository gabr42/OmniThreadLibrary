unit test_63_Service;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  OtlParallel;

type
  Tfrm63Service = class(TService)
    procedure ServiceStart(Sender: TService; var Started: boolean);
    procedure ServiceStop(Sender: TService; var Stopped: boolean);
  private
    FFuture: IOmniFuture<integer>;
    function CalculateResult: integer;
    procedure ReportResult;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  frm63Service: Tfrm63Service;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  frm63Service.Controller(CtrlCode);
end;

function Tfrm63Service.CalculateResult: integer;
begin
  Sleep(5000);
  Result := 42;
end;

function Tfrm63Service.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure Tfrm63Service.ReportResult;
var
  f: TextFile;
begin
  if not Assigned(FFuture) then
    Exit;

  AssignFile(f, ExtractFilePath(ParamStr(0)) + 'result.txt');
  Rewrite(f);
  Writeln(f, FFuture.Value);
  CloseFile(f);

  FFuture := nil;
end;

procedure Tfrm63Service.ServiceStart(Sender: TService; var Started: boolean);
begin
  FFuture := Parallel.Future<Integer>(CalculateResult,
    Parallel.TaskConfig.OnTerminated(ReportResult));
end;

procedure Tfrm63Service.ServiceStop(Sender: TService; var Stopped: boolean);
begin
  ReportResult;
end;

end.
