unit test_65_TimedTask;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  OtlCommon, OtlComm, OtlTask, OtlParallel;

const
  MSG_NEWDATA = WM_USER;

type
  TfrmTimedTask = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    inpInterval: TSpinEdit;
    lblinterval: TLabel;
    lblServerTime: TLabel;
    outServerTime: TEdit;
    outYourIP: TEdit;
    lblYourIP: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure inpIntervalChange(Sender: TObject);
  private
    FTimedTask: IOmniTimedTask;
    procedure SendRequest_asy(const task: IOmniTask);
    procedure MSGNewData(var msg: TOmniMessage); message MSG_NEWDATA;
  public
  end;

var
  frmTimedTask: TfrmTimedTask;

implementation

uses
  System.JSON,
  System.Net.HttpClient;

{$R *.dfm}

procedure TfrmTimedTask.btnStartClick(Sender: TObject);
begin
  FTimedTask.Active := true;
  //equivalent:
  //FTimedTask.Start;
  FTimedTask.ExecuteNow; //force timer event
end;

procedure TfrmTimedTask.btnStopClick(Sender: TObject);
begin
  FTimedTask.Active := false;
  //equivalent:
  //FTimedTask.Stop;
end;

procedure TfrmTimedTask.FormCreate(Sender: TObject);
begin
  FTimedTask := Parallel.TimedTask.Every(inpInterval.Value * 1000).Execute(SendRequest_asy);
  //Execute automatically activates the timer
  FTimedTask.Stop;
  FTimedTask.TaskConfig(Parallel.TaskConfig.OnMessage(Self));
end;

procedure TfrmTimedTask.inpIntervalChange(Sender: TObject);
begin
  FTimedTask.Interval := inpInterval.Value * 1000;
end;

procedure TfrmTimedTask.MSGNewData(var msg: TOmniMessage);
begin
  outServerTime.Text := msg.MsgData['Date'];
  outYourIP.Text := msg.MsgData['Origin'];
end;

procedure TfrmTimedTask.SendRequest_asy(const task: IOmniTask);
var
  client    : THttpClient;
  date      : string;
  jsonObject: TJSonObject;
  origin    : string;
  response  : IHTTPResponse;
begin
  //Asynchronous timer method, called from a background thread

  client := THttpClient.Create;
  try
    response := client.Get('https://httpbin.org/ip');
    date := response.GetHeaderValue('Date');

    jsonObject := TJSonObject.Create;
    try
      try
        origin := (jsonObject.ParseJSONValue(response.ContentAsString) as TJSONObject).Get('origin').JsonValue.Value;
      except //unexpected response
        origin := '';
      end;
    finally FreeAndNil(jsonObject); end;

    task.Comm.Send(MSG_NEWDATA, TOmniValue.CreateNamed(['Date', date, 'Origin', origin]));
  finally FreeAndNil(client); end;
end;

end.
