library dll_68_DLL;

// Shows how to use OmniThreadLibrary in a DLL. See worker_68_DLL.pas for the rules.

uses
  worker_68_DLL in 'worker_68_DLL.pas',
  api_68_DLL in 'api_68_DLL.pas';

exports
  StartWork,
  CancelWork,
  PrepareForUnload;

begin
  // Don't start tasks here. This code is executed while Windows owns the loader lock.
end.
