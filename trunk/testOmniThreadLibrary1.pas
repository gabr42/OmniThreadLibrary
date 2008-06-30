unit testOmniThreadLibrary1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlCommon,
  OtlTask,
  OtlTaskEvents;

type
  TfrmTestOTL = class(TForm)
    btnBeep: TButton;
    procedure btnBeepClick(Sender: TObject);
  private
    procedure Beep(task: IOmniTask);
  end;

var
  frmTestOTL: TfrmTestOTL;

implementation

uses
  DSiWin32;

{$R *.dfm}

{ TfrmTestOTL }

procedure TfrmTestOTL.btnBeepClick(Sender: TObject);
begin
  CreateTask(Beep, 'Beep').Run;
end;

procedure TfrmTestOTL.Beep(task: IOmniTask);
begin
  //Executed in a background thread
  MessageBeep(MB_ICONEXCLAMATION);
end;

initialization
  Randomize;
end.
