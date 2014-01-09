unit test_20_QuickSort;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor,
  OtlThreadPool;

const
  CNumDataPoints = 10000000; //10.000.000

type
  TData = array of integer;
  PData = ^TData;

  TQuickSortTask = class(TOmniWorker)
  strict private
    FData         : PData;
    FSubdivideWork: boolean;
  strict protected
    procedure QuickSort(dataLow, dataHigh: integer);
  published
    procedure Sort(const msgData: TOmniValue);
  end;

  TfrmQuickSortDemo = class(TForm)
    btnSortOnAll     : TButton;
    btnSortOnOne     : TButton;
    lbLog            : TListBox;
    OtlEventMonitor1 : TOmniEventMonitor;
    procedure btnSort(Sender: TObject);
    procedure OtlEventMonitor1TaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
  strict private
    FSortStart: int64;
  private
    FCounter: IOmniCounter;
    FData   : TData;
    procedure GenerateData;
    procedure VerifyData;
  end;

var
  frmQuickSortDemo: TfrmQuickSortDemo;

implementation

uses
  DSiWin32;

{$R *.dfm}

const
  WM_STOP = WM_USER;
  WM_SCHEDULE_SORTER = WM_USER + 1;

{ TfrmTestOTL }

procedure TfrmQuickSortDemo.btnSort(Sender: TObject);
begin
  GenerateData;
  FCounter := CreateCounter(1);
  lbLog.ItemIndex := lbLog.Items.Add('Sorting...');
  FSortStart := DSiTimeGetTime64;
  CreateTask(TQuickSortTask.Create())
    .WithCounter(FCounter)
    .MonitorWith(OtlEventMonitor1)
    .Schedule
    .Invoke(@TQuickSortTask.Sort, [@FData, Low(FData), High(FData), Sender = btnSortOnAll]);
end;

procedure TfrmQuickSortDemo.GenerateData;
var
  iData: integer;
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('Generating %d pseudorandom numbers',
    [CNumDataPoints]));
  RandSeed := CNumDataPoints;
  SetLength(FData, CNumDataPoints);
  for iData := Low(FData) to High(FData) do
    FData[iData] := Random(High(integer));
end;

procedure TfrmQuickSortDemo.OtlEventMonitor1TaskMessage(
  const task: IOmniTaskControl; const msg: TOmniMessage);
var
  msgData0: TOmniValue;
  msgData1: TOmniValue;
begin
  if msg.MsgID = WM_STOP then begin
    FSortStart := DSiTimeGetTime64 - FSortStart;
    lbLog.ItemIndex := lbLog.Items.Add(Format('Sorted, elapsed time = %d ms', [FSortStart]));
    VerifyData;
  end
  else if msg.MsgID = WM_SCHEDULE_SORTER then begin
    msgData0 := msg.MsgData[0];
    msgData1 := msg.MsgData[1];
    CreateTask(TQuickSortTask.Create())
      .WithCounter(FCounter)
      .MonitorWith(OtlEventMonitor1)
      .Schedule
      .Invoke(@TQuickSortTask.Sort, [@FData, msgData0.AsInteger, msgData1.AsInteger, true]);
  end
  else
    lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s',
      [task.UniqueID, task.Name, msg.msgID, msg.msgData.AsString]));
end;

procedure TfrmQuickSortDemo.VerifyData;
var
  iData: integer;
begin
  lbLog.ItemIndex := lbLog.Items.Add('Verifying...');
  for iData := Low(FData) to High(FData) - 1 do
    if FData[iData] > FData[iData + 1] then
      raise Exception.CreateFmt('[%d]%d > [%d]%d', [iData, FData[iData], iData+1, FData[iData+1]]);
  lbLog.ItemIndex := lbLog.Items.Add('OK');
end; { TfrmQuickSortDemo.VerifyData }

{ TQuickSortTask }

procedure TQuickSortTask.QuickSort(dataLow, dataHigh: integer);
var
  idxHigh  : integer;
  idxLow   : integer;
  partition: integer;
  tmp      : integer;
begin
  repeat
    idxLow := dataLow;
    idxHigh := dataHigh;
    partition := FData^[(dataLow + dataHigh) shr 1];
    repeat
      while FData^[idxLow] < partition do
        Inc(idxLow);
      while FData^[idxHigh] > partition do
        Dec(idxHigh);
      if idxLow <= idxHigh then begin
        tmp := FData^[idxLow];
        FData^[idxLow] := FData^[idxHigh];
        FData^[idxHigh] := tmp;
        Inc(idxLow);
        Dec(idxHigh);
      end;
    until idxLow > idxHigh;
    if dataLow < idxHigh then
      if FSubdivideWork and ((idxHigh - dataLow) > 10000) and
         (Task.Counter.Value < GlobalOmniThreadPool.MaxExecuting) then
      begin
        Task.Counter.Increment;
        Task.Comm.Send(WM_SCHEDULE_SORTER, [dataLow, idxHigh]);
      end
      else
        QuickSort(dataLow, idxHigh);
    dataLow := idxLow;
  until idxLow >= dataHigh;
end;

procedure TQuickSortTask.Sort(const msgData: TOmniValue);
begin
  FData          := PData(msgData[0].AsPointer);
  FSubdivideWork := msgData[3];
  QuickSort(msgData[1], msgData[2]);
  if Task.Counter.Decrement = 0 then
    Task.Comm.Send(WM_STOP);
  Task.Terminate;
end;

initialization
  Randomize;
end.
