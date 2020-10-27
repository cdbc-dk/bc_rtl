unit bcmemman;

{$mode objfpc}{$H+}

interface

type
  Tbcmm_GetMemMan = procedure(out MemMan : TMemoryManager);
  Tbcmm_SetMemMan = procedure(MemMan : TMemoryManager);

procedure bcmm_GetMemMan(out MemMan: TMemoryManager); { export memory manager from library }
procedure bcmm_SetMemMan(MemMan: TMemoryManager); { uses memory manager from host app }

implementation
var
  OldMemMan: TMemoryManager;
  HostMemManIsSet: boolean;

{ export memory manager from library }
procedure bcmm_GetMemMan(out MemMan: TMemoryManager);
begin
  GetMemoryManager(MemMan);
end;

{ uses memory manager from host app }
procedure bcmm_SetMemMan(MemMan: TMemoryManager);
begin
  GetMemoryManager(OldMemMan);
  SetMemoryManager(MemMan);
  HostMemManIsSet:= true;
end;

initialization
  HostMemManIsSet:= false;
finalization
  if HostMemManIsSet then SetMemoryManager(OldMemMan);
end.

