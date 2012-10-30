program app_40_Mandelbrot.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_40_Mandelbrot in 'test_40_Mandelbrot.pas' {frmParallelMandelbrot};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmParallelMandelbrot, frmParallelMandelbrot);
  Application.Run;
end.
