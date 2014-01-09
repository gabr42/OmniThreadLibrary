unit test_50_OmniValueArray;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl;

type
  TfrmOmniValueArray = class(TForm)
    lbLog: TListBox;
    btnSendHash: TButton;
    btnSendArray: TButton;
    btnSendRecord: TButton;
    procedure btnSendArrayClick(Sender: TObject);
    procedure btnSendHashClick(Sender: TObject);
    procedure btnSendRecordClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FWorker: IOmniTaskControl;
    procedure ShowResult(const task: IOmniTaskControl; const msg: TOmniMessage);
  public
  end;

var
  frmOmniValueArray: TfrmOmniValueArray;

implementation

uses
  DSiWin32;

{$R *.dfm}
{$I OtlOptions.inc}

const
  MSG_ARRAY  = 1;
  MSG_HASH   = 2;
  MSG_RECORD = 3;

  MSG_RESULT = WM_USER;

type
  TTestRecord = record
    TestInt : integer;
    TestStr : string;
    TestObj : TOmniWaitableValue;
    TestIntf: IOmniCounter;
  end;

procedure Worker(const task: IOmniTask);
var
  i      : integer;
  msgData: TOmniValue;
  msgID  : word;
  rec    : TTestRecord;
  s      : string;
begin
  while DSiWaitForTwoObjects(task.TerminateEvent, task.Comm.NewMessageEvent, false, INFINITE) = WAIT_OBJECT_1 do
  begin
    task.Comm.Receive(msgID, msgData);
    case msgID of
      MSG_ARRAY:
        begin
          s := '';
          for i := 0 to msgData.AsArray.Count - 1 do
            if msgData[i].IsInterface then
              s := s + IntToStr((msgData[i].AsInterface as IOmniCounter).Value) + ' '
            else
              s := s + IntToStr(msgData[i]) + ' ';
          task.Comm.Send(MSG_RESULT, 'Received array with values: ' + s);
        end;
      MSG_HASH:
        begin
          s := Format('%s is %s', [msgData['Person'].AsString, msgData['Location'].AsString]);
          task.Comm.Send(MSG_RESULT, s);
        end;
      MSG_RECORD:
        begin
          {$IFDEF OTL_Generics}
          rec := msgData.ToRecord<TTestRecord>;
          task.Comm.Send(MSG_RESULT, 'Received record with values: ' +
            Format('%d, %s, %d, %d', [rec.TestInt, rec.TestStr,
              rec.TestObj.Value.AsInteger, rec.TestIntf.Value]));
          rec.TestObj.Free;
          {$ENDIF OTL_Generics}
        end;
    end;
  end;
end;

procedure TfrmOmniValueArray.btnSendArrayClick(Sender: TObject);
begin
  FWorker.Comm.Send(MSG_ARRAY, [CreateCounter(0), 1, 1, 2, 3, 5, 8]);
  //equivalent code:
  //FWorker.Comm.Send(MSG_ARRAY, TOmniValue.Create([CreateCounter(0), 1, 1, 2, 3, 5, 8]));
end;

procedure TfrmOmniValueArray.btnSendHashClick(Sender: TObject);
var
  ov: TOmniValue;
begin
  ov := TOmniValue.CreateNamed([
    'Person', 'Primoz',
    'Location', 'at home']);
  FWorker.Comm.Send(MSG_HASH, ov);
end;

procedure TfrmOmniValueArray.btnSendRecordClick(Sender: TObject);
var
  rec: TTestRecord;
begin
  {$IFDEF OTL_Generics}
  rec.TestInt := 42;
  rec.TestStr := 'OTL';
  rec.TestObj := TOmniWaitableValue.Create;
  rec.TestObj.Signal(17);
  rec.TestIntf := CreateCounter(127);
  FWorker.Comm.Send(MSG_RECORD, TOmniValue.FromRecord<TTestRecord>(rec));
  {$ENDIF OTL_Generics}
end;

procedure TfrmOmniValueArray.FormDestroy(Sender: TObject);
begin
  FWorker.Terminate;
  FWorker := nil;
end;

procedure TfrmOmniValueArray.FormCreate(Sender: TObject);
begin
  FWorker := CreateTask(Worker).OnMessage(MSG_RESULT, ShowResult).Run;
  {$IFNDEF OTL_Generics}
  btnSendRecord.Enabled := false;
  {$ENDIF ~OTL_Generics}
end;

procedure TfrmOmniValueArray.ShowResult(const task: IOmniTaskControl; const msg:
  TOmniMessage);
begin
  lbLog.Items.Add(msg.MsgData);
end;

end.
