program app_62_console;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlParallel;

const
  MSG_STATUS = WM_USER;

  procedure ProcessMessages;
  var
    Msg: TMsg;
  begin
    while integer(PeekMessage(Msg, 0, 0, 0, PM_REMOVE)) <> 0 do begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;

  function DoTheCalculation(const task: IOmniTask): integer;
  var
    i: integer;
  begin
    for i := 1 to 5 do begin
      task.Comm.Send(MSG_STATUS, '... still calculating');
      Sleep(1000);
    end;
    Result := 42;
  end;

var
  calc: IOmniFuture<integer>;

begin
  try
    calc := Parallel.Future<integer>(DoTheCalculation,
      parallel.TaskConfig.OnMessage(MSG_STATUS,
        procedure(const task: IOmniTaskControl; const msg: TOmniMessage)
        begin
          Writeln(msg.MsgData.AsString);
        end));

    Writeln('Background thread is calculating ...');
    while not calc.IsDone do
      ProcessMessages;
    Writeln('And the answer is: ', calc.Value);

    if DebugHook <> 0 then
      Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
