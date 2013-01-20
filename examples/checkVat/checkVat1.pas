unit checkVat1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  checkVatService,
  OtlParallel;

type
  TfrmCheckVat = class(TForm)
    inpCC: TLabeledEdit;
    inpVAT: TLabeledEdit;
    btnCheckVat: TButton;
    outVatInfo: TMemo;
    procedure btnCheckVatClick(Sender: TObject);
  strict private
    FRequest: IOmniFuture<checkVatResponse>;
  private
  public
  end;

var
  frmCheckVat: TfrmCheckVat;

implementation

uses
  OleAuto,
  ActiveX,
  OtlTaskControl;

{$R *.dfm}

procedure TfrmCheckVat.btnCheckVatClick(Sender: TObject);
begin
  btnCheckVat.Enabled := false;
  outVatInfo.Lines.Clear;
  FRequest := Parallel.Future<checkVatResponse>(
    function: checkVatResponse
    var
      request: checkVat;
    begin
      OleCheck(CoInitializeEx(nil, COINIT_MULTITHREADED));
      try
        request := checkVat.Create;
        try
          request.countryCode := Trim(inpCC.Text);
          request.vatNumber := Trim(inpVat.Text);
          Result := checkVatService.GetcheckVatPortType.checkVat(request);
        finally FreeAndNil(request); end;
      finally CoUninitialize; end;
    end,
    Parallel.TaskConfig.OnTerminated(
      procedure (const task: IOmniTaskControl)
      begin
        outVatInfo.Text := FRequest.Value.name_ + #13#10 + FRequest.Value.address;
        FRequest.Value.Free;
        FRequest := nil;
        btnCheckVat.Enabled := true;
      end
    )
  );
end;

end.
