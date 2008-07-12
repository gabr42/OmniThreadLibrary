unit OtlRegister;

interface

uses Classes, OtlTaskEvents;

procedure Register;

implementation

{ exports }

procedure Register;
begin
  RegisterComponents('OmniThreadLibrary', [TOmniTaskEventDispatch]);
end; { Register }

end.
