unit TestValue;

interface

uses
  GpStuff;

var
  GTestValueCount: integer;

type
  ITestValue = interface(IGpTraceable) ['{48E54332-2E07-47F5-A6AE-63CF75168CA6}']
    function  GetValue: integer;
    procedure SetValue(const value: integer);
    property Value: integer read GetValue write SetValue;
  end;

  TTestValue = class(TGpTraceable, ITestValue)
  strict private
    FValue: integer;
  protected
    function  GetValue: integer;
    procedure SetValue(const value: integer);
  public
    constructor Create(val: integer);
    destructor  Destroy; override;
    property Value: integer read GetValue write SetValue;
  end;

implementation

{ TTestValue }

constructor TTestValue.Create(val: integer);
begin
  inherited Create;
  FValue := val;
  Inc(GTestValueCount);
end;

destructor TTestValue.Destroy;
begin
  Dec(GTestValueCount);
  inherited;
end;

function TTestValue.GetValue: integer;
begin
  Result := FValue;
end;

procedure TTestValue.SetValue(const value: integer);
begin
  FValue := value;
end;

end.
