unit test_49_Worker;

interface

uses
  OtlComm,
  OtlTaskControl,
  test_49_Common;

type
  TFramedWorker = class(TOmniWorker)
  strict private
    FMessage: string;
  public
    function Initialize: boolean; override;
    procedure MsgHello(var msg: TOmniMessage); message MSG_HELLO;
    procedure Timer1;
  end;

implementation

{ TFramedWorker }

function TFramedWorker.Initialize: boolean;
begin
  Result := inherited Initialize;
  if Result then begin
    FMessage := Task.Param['Msg'];
    Task.SetTimer(1, 1000 + Random(500), @TFramedWorker.Timer1);
  end;
end;

procedure TFramedWorker.MsgHello(var msg: TOmniMessage);
begin
  Task.Comm.Send(MSG_NOTIFY, 'Hello, ' + msg.MsgData);
end;

procedure TFramedWorker.Timer1;
begin
  Task.Comm.Send(MSG_NOTIFY, '... ' + FMessage);
end;

end.
