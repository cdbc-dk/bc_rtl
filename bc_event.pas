unit bc_event;
{$mode ObjFPC}{$H+}
{$interfaces corba}  { NO reference counting! }
{$define debug}
interface
uses
  Classes, SysUtils{, bc_guardian};
const
  { string-GUID for IbcEvent }
  SGUIDIbcEvent  = '{E80C509F-337A-4278-9BA0-B328A7400AA2}';
type
  { IbcEvent is a corba interface for TbcEvent i.e: the contract TbcEvent fulfills }
  IbcEvent = interface [SGUIDIbcEvent]
    { reset, waitfor will sleep until signaled or timeout (could be called sleep) }
    procedure Reset;
    { signal whomever is waiting, they can go on, wake the sleeping (could be called wakeup) }
    procedure Signal;
    { waiting/sleeping indefinitely until signaled ~ released }
    procedure Waitfor; overload;
    { waiting/sleeping maximum aTimeout or being signaled ~ released }
    procedure Waitfor(aTimeout: ptrint); overload;
  end; { IbcEvent }
  { TbcEvent encapsulates the event mechanism in fpc }
  TbcEvent = class(TObject,IbcEvent)
  private
    fEvent: PRTLEvent;
    fLock: TRTLCriticalSection;
  public
    constructor Create; { fevent starts life as "reset" ie. sleeping, you can wait for it }
    Destructor Destroy; override;
    { reset, "waitfor" will sleep until signaled or timeout (could be called sleep) }
    procedure Reset;
    { signal whomever is waiting, they can go on, wake the sleeping (could be called wakeup) }
    procedure Signal;
    { waiting/sleeping indefinitely until signaled ~ released }
    procedure Waitfor; overload;
    { waiting/sleeping maximum aTimeout or being signaled ~ released }
    procedure Waitfor(aTimeout: ptrint); overload;
  end;

implementation

{ TbcEvent }
constructor TbcEvent.Create;
begin
  inherited Create;
  InitCriticalSection(fLock);
  fEvent:= RTLEventCreate; { fevent starts life as "reset" ie. sleeping, you can wait for it }
end;

destructor TbcEvent.Destroy;
begin
  RTLEventDestroy(fEvent);
  DoneCriticalSection(fLock);
  inherited Destroy;
end;

procedure TbcEvent.Reset;
begin { reset, you can wait for it }
  EnterCriticalSection(fLock); try RTLEventResetEvent(fEvent); finally LeaveCriticalSection(fLock); end;
end;

procedure TbcEvent.Signal;
begin { signal whomever is waiting, they can go on }
  EnterCriticalSection(fLock); try RTLEventSetEvent(fEvent); finally LeaveCriticalSection(fLock); end;
end;

procedure TbcEvent.Waitfor;
begin { waiting to be signaled ~ released }
  RTLeventWaitFor(fEvent);
end;

procedure TbcEvent.Waitfor(aTimeout: ptrint);
begin { waiting maximum aTimeout to be signaled ~ released }
  RTLeventWaitFor(fEvent,aTimeout);
end;
{$interfaces com}
end.

