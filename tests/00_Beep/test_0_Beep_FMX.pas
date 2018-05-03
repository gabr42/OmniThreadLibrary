unit test_0_Beep_FMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  OtlTask, OtlTaskControl;

type
  TfrmTestSimple = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure Beep(const task: IOmniTask);
  public
  end;

var
  frmTestSimple: TfrmTestSimple;

implementation

uses
  Winapi.Windows;

{$R *.fmx}

procedure TfrmTestSimple.Button1Click(Sender: TObject);
begin
  CreateTask(Beep, 'Beep').Run;
end;

procedure TfrmTestSimple.Beep(const task: IOmniTask);
begin
  //Executed in a background thread
  MessageBeep(MB_ICONEXCLAMATION);
end;

end.
