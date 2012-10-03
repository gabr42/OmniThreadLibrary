program app_40_Mandelbrot.XE3;

uses
  FastMM4,
  Forms,
  test_40_Mandelbrot in 'test_40_Mandelbrot.pas' {frmParallelMandelbrot};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmParallelMandelbrot, frmParallelMandelbrot);
  Application.Run;
end.
