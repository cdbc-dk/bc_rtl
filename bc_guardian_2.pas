

            {************************************************
            * Unit name : bc_guardian.pas                   *
            * Copyright : (c) 2020 cdbc.dk                  *
            * Programmer: Benny Christensen /bc             *
            * Created   : 2020.04.13 /bc                    *
            * Updated   : 2020.04.14 /bc                    *
            *************************************************
            * Abstract:                                     *
            * Guardian is a global singleton, that provides *
            * locking for threads.                          *
            * IGuardian is a contract interface, that an    *
            * implementing class must fulfill.              *
            ************************************************}

unit bc_guardian_2;
{$mode objfpc}{$H+}
{$define debug}
interface
uses
  Classes, SysUtils,
  syncobjs;

const
  UnitVersion = '1.13.04.2020';
    
type
  { granulated locking, ie: none, read all you can, write, block everybody else }
  TLockType = (ltNone, ltReading, ltWriting);

  IGuardian = interface ['{344C70E4-E6F8-4CDA-BDBE-682A511975D1}']
    function getId: ptrint; // private
    procedure setId(aValue: ptrint); // private
    procedure Lock; // api
    procedure UnLock; // api
    property Id: ptrint read getId write setId; // hmmm...
  end; { IGuardian }

  { TGuardian is a global, that provides locking for threads }
  TGuardian = class(TInterfacedObject,IGuardian)
  private
    fLocked: TLockType; //ææ
    fId: ptrint;
    fLock: syncobjs.TCriticalSection;
    function getId: ptrint;
    procedure setId(aValue: ptrint);
  public
    constructor Create;
    destructor Destroy; override;
    {$ifdef debug}
    procedure bc_Lock; { -> bc_lock -> check if lock is open, if so obtain the lock and block! if not, wait. }
    procedure bc_UnLock; { -> bc_unlock -> done! release lock and carry on }
    {$endif}
    procedure Lock;
    procedure UnLock;
    property Id: ptrint read getId write setId;
  end; { TGuardian }

{ factory provides a global guardian, for thread syncronization ~ singleton on demand }
function Guardian: TGuardian;

implementation
var 
  Singleton: TGuardian; // global locking mechanism

function Guardian: TGuardian; { singleton }
begin
  if not assigned(Singleton) then Singleton:= TGuardian.Create;
  Result:= Singleton;
end; { gets released on progam end }

{ *** TGuardian *** }
function TGuardian.getId: ptrint;
begin
  Result:= fId;
end;

procedure TGuardian.setId(aValue: ptrint);
begin
  fId:= aValue;
end;

constructor TGuardian.Create;
begin
  {$ifdef debug}
  fLocked:= ltNone; { no locking }
  {$endif}
  fLock:= syncobjs.TCriticalSection.Create;
end;

destructor TGuardian.Destroy;
begin
  {$ifdef debug}
  fLocked:= ltNone; { no locking }
  {$endif}
  fLock.Free;
end;

procedure TGuardian.bc_Lock;
begin
  {$ifdef debug}
  while fLocked = ltWriting do begin
    sleep(1);
    if fLocked = ltNone then break;
  end;
  fLocked:= ltWriting;
  {$endif}
end;

procedure TGuardian.bc_UnLock;
begin
  {$ifdef debug}
  fLocked:= ltNone;
  {$endif}
end;

procedure TGuardian.Lock;
begin
  fLock.Acquire;
end;

procedure TGuardian.UnLock;
begin
  fLock.Release;
end;

initialization
  Singleton:= nil;
finalization
  FreeAndNil(Singleton);
end.

    
