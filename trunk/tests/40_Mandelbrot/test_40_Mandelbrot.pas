unit test_40_Mandelbrot;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OtlComm, OtlTaskControl, OtlEventMonitor;

type
  TfrmParallelMandelbrot = class(TForm)
    OmniEventMonitor: TOmniEventMonitor;
    procedure FormDblClick(Sender: TObject);
    procedure OmniEventMonitorTaskMessage(const task: IOmniTaskControl; const msg:
      TOmniMessage);
  private
    procedure DisplayLine(bitmap: TBitmap; y: integer);
    procedure PaintLine(width: integer; y: integer; var bitmap: TBitmap);
  public
  end;

var
  frmParallelMandelbrot: TfrmParallelMandelbrot;

implementation

uses
  OtlTask,
  OtlParallel;

{$R *.dfm}

const
  MSG_DISPLAY_LINE = 1;

procedure TfrmParallelMandelbrot.DisplayLine(bitmap: TBitmap; y: integer);
begin
  Canvas.CopyRect(Rect(0, y, ClientWidth, y + 1), bitmap.Canvas,
    Rect(0, 0, bitmap.Width, 1));
end;

procedure TfrmParallelMandelbrot.FormDblClick(Sender: TObject);
begin
  Invalidate; Update;
  Parallel
    .ForEach(0, ClientHeight - 1)
    .MonitorWith(OmniEventMonitor)
    .Execute(
      procedure (const task: IOmniTask; const y: integer)
      var
        bitmap: TBitmap;
      begin
        PaintLine(ClientWidth - 1, y, bitmap);
        task.Comm.Send(MSG_DISPLAY_LINE, [y, bitmap]);
      end
    );
end;

procedure TfrmParallelMandelbrot.OmniEventMonitorTaskMessage(const task: IOmniTaskControl; const msg:
  TOmniMessage);
var
  bitmap: TBitmap;
  y     : integer;
begin
  if msg.MsgID = MSG_DISPLAY_LINE then begin
    y := msg.MsgData[0];
    bitmap := TBitmap(cardinal(msg.MsgData[1]));
    DisplayLine(bitmap, y);
    bitmap.Free;
    Sleep(5);
  end;
end;

procedure TfrmParallelMandelbrot.PaintLine(width: integer; y: integer; var bitmap: TBitmap);
const
  aColors: array [0 .. 14] of TColor = (clBlack, clMaroon, clGreen, clNavy,
    clPurple, clTeal, clGray, clSilver, clRed, clLime, clBLue, clFuchsia,
    clAqua, clWhite, clBlack);
var
  color: integer;
  rU   : real;
  rV   : real;
  rX   : real;
  rY   : real;
  rZ   : real;
  x    : integer;
begin
  // code from http://www.delphifaq.net/how-to-draw-a-mandelbrot-fractal-on-the-forms-canvas/
  bitmap := TBitmap.Create;
  bitmap.Width := width;
  bitmap.Height := 1;
  bitmap.PixelFormat := pf32bit;
  for x := 0 to bitmap.Width - 1 do
  begin
    rX := -0.8 + 3 * x / ClientWidth;
    rY := -1.4 + 2.8 * y / ClientHeight;
    color := 0;
    rU := 0;
    rV := 0;
    repeat
      rZ := Sqr(rU) - Sqr(rV) - rX;
      rV := 2 * rU * rV - rY;
      rU := rZ;
      Inc(color);
    until (Sqr(rU) + Sqr(rV) > 9) or (color = 14);
    bitmap.Canvas.Pixels[x, 0] := aColors[color];
  end;
end;

end.
