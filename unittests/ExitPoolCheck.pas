unit ExitPoolCheck;

// Helper for TestExitPool1. Must be initialized before OTL units (so it must be the first unit in
// the project) - it is then finalized after them and can check that no pool task is running.

interface

var
  GExitPoolActive : boolean; // program is running as a child process, started from TestExitPool1
  GExitPoolError  : integer; // set by the tasks
  GExitPoolRunning: integer; // number of tasks that are being executed

const
  CExitPoolOK      = 42;
  CExitPoolRunning = 43;

implementation

initialization
finalization
  if GExitPoolActive then begin
    if GExitPoolRunning <> 0 then
      ExitCode := CExitPoolRunning
    else if GExitPoolError <> 0 then
      ExitCode := GExitPoolError
    else
      ExitCode := CExitPoolOK;
  end;
end.
