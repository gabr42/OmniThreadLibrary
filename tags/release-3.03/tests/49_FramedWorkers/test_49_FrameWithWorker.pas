unit test_49_FrameWithWorker;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls,
  OtlComm,
  OtlTaskControl,
  test_49_Common;

type
  TfrmFrameWithWorker = class(TFrame)
    lbLog: TListBox;
    btnHello: TButton;
    procedure btnHelloClick(Sender: TObject);
  private
    FWorker: IOmniTaskControl;
  public
    property Worker: IOmniTaskControl read FWorker write FWorker;
    procedure MsgNotify(var msg: TOmniMessage); message MSG_NOTIFY;
  end;

implementation

{$R *.dfm}

procedure TfrmFrameWithWorker.btnHelloClick(Sender: TObject);
begin
  Worker.Comm.Send(MSG_HELLO, Name);
end;

procedure TfrmFrameWithWorker.MsgNotify(var msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg.MsgData);
end;

end.
