unit UnloadOwner;

// Owns a thread pool; finalized before OTL units.

interface

uses
  OtlThreadPool;

var
  GOwnedPool: IOmniThreadPool;

implementation

initialization
finalization
  GOwnedPool := nil;
end.
