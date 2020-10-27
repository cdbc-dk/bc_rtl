unit semaphore;
 
{$mode objfpc}{$H+}
 
interface
 
uses
  Contnrs,
  Classes,
  SysUtils;
 
type
 
  { TSemaphore }
 
  TSemaphore = class
  private
    fMaxPermits: Cardinal;
    fPermits: Cardinal;
    fLock: TRTLCriticalSection;
    FBlockQueue: Contnrs.TQueue;
    function GetWaitCount: Cardinal;
  public
    procedure Wait;
    procedure Post;
    function Used: Boolean;
    constructor Create(MaxPermits: Cardinal);
    destructor Destroy; override;
    property WaitCount: Cardinal read GetWaitCount;
    property Permits: Cardinal read fPermits;
    property MaxPermits: Cardinal read fMaxPermits;
  end;
 
 
implementation
 
{ TSemaphore }
 
function TSemaphore.GetWaitCount: Cardinal;
begin
  EnterCriticalSection(fLock);
  try
    Result:= FBlockQueue.Count;
  finally
    LeaveCriticalSection(fLock);
  end;
end;
 
procedure TSemaphore.Wait;
var
  aWait: Boolean;
  aEvent: PRTLEvent;
begin
  //writeln('Sem:');
  //writeln('  locking...');
  EnterCriticalSection(fLock);
  try
    //writeln('  locked');
    if (fPermits > 0) then begin
      Dec(fPermits);
      aWait:= False;
    end else begin
      aEvent:= RTLEventCreate;
      FBlockQueue.Push(aEvent);
      aWait:= True;
    end;
  finally
    LeaveCriticalSection(fLock);
  end;
  if aWait then begin
    //writeln('  waiting...');
    RTLeventWaitFor(aEvent);
    RTLEventDestroy(aEvent);
  end;
  //writeln('  aquired');
end;
 
procedure TSemaphore.Post;
begin
  EnterCriticalSection(fLock);
  try
    if FBlockQueue.Count > 0 then
      RTLEventSetEvent(PRTLEvent(FBlockQueue.Pop))
    else
      Inc(fPermits);
  finally
    LeaveCriticalSection(fLock);
  end;
end;
 
function TSemaphore.Used: Boolean;
begin
  EnterCriticalSection(fLock);
  try
    Result := fPermits < fMaxPermits;
  finally
    LeaveCriticalSection(fLock);
  end;
end;
 
constructor TSemaphore.Create(MaxPermits: Cardinal);
begin
  fMaxPermits := MaxPermits;
  fPermits := MaxPermits;
  InitCriticalSection(fLock);
  FBlockQueue:= TQueue.Create;
end;
 
destructor TSemaphore.Destroy;
begin
  DoneCriticalSection(fLock);
  FBlockQueue.Free;
  inherited Destroy;
end;
 
end.
 
