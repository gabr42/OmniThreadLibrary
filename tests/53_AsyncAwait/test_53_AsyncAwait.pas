unit test_53_AsyncAwait;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmAsyncAwaitTest = class(TForm)
    btnSleep1: TButton;
    btnSleep2: TButton;
    btnSleep3: TButton;
    procedure btnSleep1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAsyncAwaitTest: TfrmAsyncAwaitTest;

implementation

uses
  OtlParallel;

{$R *.dfm}

procedure TfrmAsyncAwaitTest.btnSleep1Click(Sender: TObject);
var
  button: TButton;
begin
  button := Sender as TButton;
  button.Caption := 'Working ...';
  button.Enabled := false;
  Async(
    procedure begin
      Sleep(5000);
    end)
  .Await(
    procedure begin
      button.Enabled := true;
      button.Caption := 'Done!';
    end);
end;

end.
