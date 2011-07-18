unit test_48_OtlParallelExceptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm34 = class(TForm)
    btnAsync: TButton;
    btnForeach: TButton;
    btnForkJoin: TButton;
    btnFuture1: TButton;
    btnFuture2: TButton;
    btnJoin: TButton;
    btnPipeline: TButton;
    lbLog: TListBox;
    btnFuture3: TButton;
    procedure btnFuture1Click(Sender: TObject);
    procedure btnFuture2Click(Sender: TObject);
    procedure btnFuture3Click(Sender: TObject);
    procedure btnJoinClick(Sender: TObject);
  private
    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const param: array of const); overload;
  end;

var
  Form34: TForm34;

implementation

uses
  OtlParallel;

{$R *.dfm}

type
  ETestException = class(Exception);

procedure TForm34.btnFuture1Click(Sender: TObject);
var
  future: IOmniFuture<integer>;
begin
  future := Parallel.Future<integer>(
    function: integer
    begin
      raise ETestException.Create('Exception in Parallel.Future');
    end
  );
  Log('Future is executing ...'); Sleep(1000);
  try
    Log('Future retured: %d', [future.Value]);
  except
    on E: Exception do
      Log('Future raised exception %s:%s', [E.ClassName, E.Message]);
  end;
end;

procedure TForm34.btnFuture2Click(Sender: TObject);
var
  future: IOmniFuture<integer>;
begin
  future := Parallel.Future<integer>(
    function: integer
    begin
      raise ETestException.Create('Exception in Parallel.Future');
    end
  );
  Log('Future is executing ...'); Sleep(1000);
  Log('Future is done: %d', [Ord(future.IsDone)]);
  if assigned(future.FatalException) then
    Log('Future raised exception %s:%s', [future.FatalException.ClassName, future.FatalException.Message])
  else
    Log('Future retured: %d', [future.Value]);
end;

procedure TForm34.btnFuture3Click(Sender: TObject);
var
  excFuture: Exception;
  future   : IOmniFuture<integer>;
begin
  future := Parallel.Future<integer>(
    function: integer
    begin
      raise ETestException.Create('Exception in Parallel.Future');
    end
  );
  Log('Future is executing ...'); Sleep(1000);
  Log('Future is done: %d', [Ord(future.IsDone)]);
  excFuture := future.DetachException;
  try
    if assigned(excFuture) then
      Log('Future raised exception %s:%s', [excFuture.ClassName, excFuture.Message])
    else
      Log('Future retured: %d', [future.Value]);
  finally FreeAndNil(excFuture); end;
end;

procedure TForm34.btnJoinClick(Sender: TObject);
var
  iInnerExc: integer;
begin
  try
    Parallel.Join([
      procedure begin
        raise ETestException.Create('Exception 1 in Parallel.Join');
      end,
      procedure begin
      end,
      procedure begin
        raise ETestException.Create('Exception 2 in Parallel.Join');
      end]);
  except
    on E: EJoinException do begin
      Log('Join raised exception %s:%s', [E.ClassName, E.Message]);
      for iInnerExc := 0 to E.Count - 1 do
        Log('  Task #%d raised exception: %s:%s', [E[iInnerExc].TaskNumber,
          E[iInnerExc].FatalException.ClassName,
          E[iInnerExc].FatalException.Message]);
    end;
  end;
end;

procedure TForm34.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
  lbLog.Update;
end;

procedure TForm34.Log(const msg: string; const param: array of const);
begin
  Log(Format(msg, param));
end;

end.
