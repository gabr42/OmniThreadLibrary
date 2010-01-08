unit OtlParallel;

interface

uses
  OtlCommon;

type
  IOmniParallelLoop = interface;

  TOmniLoopDelegate = reference to procedure(loop: IOmniParallelLoop;
    const value: TOmniValue);

  TOmniSimpleLoopDelegate = reference to procedure(const value: TOmniValue);

  IOmniParallelLoop = interface ['{ACBFF35A-FED9-4630-B442-1C8B6AD2AABF}']
    function  MaxTasks(numTasks: integer): IOmniParallelLoop;
    procedure Execute(loopBody: TOmniSimpleLoopDelegate); overload;
    procedure Execute(loopBody: TOmniLoopDelegate); overload;
    procedure Stop;
    function  Timeout(timeout_ms: integer): IOmniParallelLoop;
  end;

  Parallel = class
    class function ForEach(enum: IOmniValueEnumerator): IOmniParallelLoop;
  end;

implementation

class function Parallel.ForEach(enum: IOmniValueEnumerator): IOmniParallelLoop;
begin
  Result := nil;
end;

end.
