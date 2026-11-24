program app_68_DLL;

// Compile dll_68_DLL into the same folder before running this program.

uses
  Forms,
  test_68_DLL in 'test_68_DLL.pas' {frmTestDLL},
  api_68_DLL in 'api_68_DLL.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestDLL, frmTestDLL);
  Application.Run;
end.
