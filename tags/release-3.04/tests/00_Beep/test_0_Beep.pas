unit test_0_Beep;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, Forms,
  OtlTask,
  OtlTaskControl;

type
  TfrmTestSimple = class(TForm)
    btnBeep: TButton;
    procedure btnBeepClick(Sender: TObject);
  private
    procedure Beep(const task: IOmniTask);
  end;

var
  frmTestSimple: TfrmTestSimple;

implementation

uses
  DSiWin32;

{$R *.dfm}

{ TfrmTestOTL }

procedure TfrmTestSimple.btnBeepClick(Sender: TObject);
begin
  CreateTask(Beep, 'Beep').Run;
end;

procedure TfrmTestSimple.Beep(const task: IOmniTask);
begin
  //Executed in a background thread
  MessageBeep(MB_ICONEXCLAMATION);
end;

initialization
  Randomize;
end.
