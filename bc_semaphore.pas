unit bc_semaphore;
{$mode objfpc}{$H+}
{$interfaces corba}
{.$define debug}
interface
uses Classes, SysUtils, Contnrs,bc_mtlinklist;
 
type
  { TbcSemaphore }
  TbcSemaphore = class
  private
    fMaxPermits: Cardinal;
    fPermits: cardinal;
    fLock: TRTLCriticalSection;
    fBlockQueue: Contnrs.TQueue;
    function GetWaitCount: cardinal;
  public
    procedure Wait;
    procedure Signal; { used to be post! }
    function Used: Boolean;
    constructor Create(MaxPermits: cardinal);
    destructor Destroy; override;
    property WaitCount: cardinal read GetWaitCount;
    property Permits: cardinal read fPermits;
    property MaxPermits: cardinal read fMaxPermits;
  end;
 
 
implementation
uses bc_errorlog;
{ TbcSemaphore }
function TbcSemaphore.GetWaitCount: cardinal;
begin
  EnterCriticalSection(fLock); try Result:= fBlockQueue.Count; finally LeaveCriticalSection(fLock); end;
end;
 
procedure TbcSemaphore.Wait;
var
  aWait: boolean;
  aEvent: PRTLEvent;
begin
  {$ifdef debug} ErrorLog.LogLn(format('[%s]TbcSemaphore.Wait  : locking... ThreadID = %d',[timetostr(now),ThreadID])); {$endif}
  EnterCriticalSection(fLock);
  try
    {$ifdef debug} ErrorLog.LogLn(format('[%s]TbcSemaphore.Wait  : locked     ThreadID = %d',[timetostr(now),ThreadID])); {$endif}
    if (fPermits > 0) then begin
      dec(fPermits);
      aWait:= false;
    end else begin
      aEvent:= RTLEventCreate;
      fBlockQueue.Push(aEvent);
      aWait:= True;
    end;
  finally
    LeaveCriticalSection(fLock);
  end;
  if aWait then begin
    {$ifdef debug} ErrorLog.LogLn(format('[%s]TbcSemaphore.Wait  : waiting... ThreadID = %d',[timetostr(now),ThreadID])); {$endif}
    RTLeventWaitFor(aEvent);
    RTLEventDestroy(aEvent);
  end;
  {$ifdef debug} ErrorLog.LogLn(format('[%s]TbcSemaphore.Wait  : acquired    ThreadID = %d',[timetostr(now),ThreadID])); {$endif}
end;
 
procedure TbcSemaphore.Signal;
begin
  EnterCriticalSection(fLock);
  try
    if fBlockQueue.Count > 0 then
      RTLEventSetEvent(PRTLEvent(fBlockQueue.Pop))
    else
      inc(fPermits);
  finally
    LeaveCriticalSection(fLock);
  end;
  {$ifdef debug} ErrorLog.LogLn(format('[%s]TbcSemaphore.Signal: released    ThreadID = %d',[timetostr(now),ThreadID])); {$endif}
end;
 
function TbcSemaphore.Used: boolean;
begin
  EnterCriticalSection(fLock);
  try
    Result := fPermits < fMaxPermits;
  finally
    LeaveCriticalSection(fLock);
  end;
end;
 
constructor TbcSemaphore.Create(MaxPermits: Cardinal);
begin
  fMaxPermits := MaxPermits;
  fPermits := MaxPermits;
  InitCriticalSection(fLock);
  fBlockQueue:= TQueue.Create;
end;
 
destructor TbcSemaphore.Destroy;
begin
  DoneCriticalSection(fLock);
  fBlockQueue.Free;
  inherited Destroy;
end;
{$interfaces com}
end.
 
