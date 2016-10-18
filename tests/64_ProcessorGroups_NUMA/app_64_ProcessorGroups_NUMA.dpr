program app_64_ProcessorGroups_NUMA;

uses
  Vcl.Forms,
  test_64_ProcessorGroups_NUMA in 'test_64_ProcessorGroups_NUMA.pas' {frmProcessorGroupsNUMA};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmProcessorGroupsNUMA, frmProcessorGroupsNUMA);
  Application.Run;
end.
