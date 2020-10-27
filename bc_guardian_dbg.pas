

          {******************************************************
          * Unit name : bc_guardian_dbg.pas                     *
          * Copyright : (c) 2020 cdbc.dk                        *
          * Programmer: Benny Christensen /bc                   *
          * Created   : 2020.09.19 /bc                          *
          * Updated   : 2020.09.19 /bc: forked from bc_guardian *
          *           : 2020.09.19 /bc: added debug-state       *
          *                                                     *
          *                                                     *
          *******************************************************
          *                                                     *
          * Abstract:                                           *
          * Guardian is a global singleton, that provides       *
          * locking for threads.                                *
          * IGuardian is a contract interface, that an          *
          * implementing class must fulfill.                    *
          ******************************************************}

unit bc_guardian_dbg;
{$mode objfpc}{$H+}
{$define use_lcltype}
{$define debug}
interface
uses
  Classes, SysUtils,
  {$ifdef use_lcltype}
    LCLType,
    LCLIntf;
  {$else}
    syncobjs;
  {$endif}

const
  UnitVersion = '2.19.09.2020';
  SGUIDGuardian = '{344C70E4-E6F8-4CDA-BDBE-682A511975D1}';

type
  IGuardian = interface [SGUIDGuardian]
    procedure Lock;   // api
    procedure UnLock; // api
  end; { IGuardian }

  { TGuardian is a global, that provides locking for threads }
  TGuardian = class(TInterfacedObject,IGuardian)
  private
    fId: ptrint;
    {$ifdef use_lcltype}
      fLock: LCLType.TCriticalSection; { from lcltype.pp }
    {$else}
      fLock: syncobjs.TCriticalSection; { from syncobjs.pas }
    {$endif}
    function getVersion: string;
    function getId: ptrint;
    procedure setId(aValue: ptrint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    property Id: ptrint read getId write setId;
    property Version: string read getVersion;
  end; { TGuardian }

{ factory provides a global guardian, for thread syncronization ~ singleton on demand }
function Guardian: TGuardian;

implementation
uses bc_errorlog; { provides error logging by ways of "ErrorLog" }

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
  {$ifdef debug}
    ErrorLog.LogLn('TGuardian.getId ['+DateTimeToStr(now)+'] Id = '+IntToStr(Result));// log progress
  {$endif}
end;

function TGuardian.getVersion: string;
begin
  Result:= UnitVersion;
  {$ifdef debug}
    ErrorLog.LogLn('TGuardian.getVersion ['+DateTimeToStr(now)+'] Version = '+Result);// log progress
  {$endif}
end;

procedure TGuardian.setId(aValue: ptrint);
begin
  fId:= aValue;
  {$ifdef debug}
    ErrorLog.LogLn('TGuardian.setId ['+DateTimeToStr(now)+'] Id = '+IntToStr(fId));// log progress
  {$endif}
end;

constructor TGuardian.Create;
begin
  inherited Create;
  fId:= 8723;
  {$ifdef use_lcltype}
    LCLIntf.InitializeCriticalSection(fLock); // create the CriticalSection
  {$else}
    fLock:= syncobjs.TCriticalSection.Create;
  {$endif}
  {$ifdef debug}
    ErrorLog.LogLn('TGuardian.Create ['+DateTimeToStr(now)+'] Id = '+IntToStr(fId));// log progress
  {$endif}
end;

destructor TGuardian.Destroy;
begin
  {$ifdef debug}
    ErrorLog.LogLn('TGuardian.Destroy ['+DateTimeToStr(now)+'] Id = '+IntToStr(fId));// log progress
  {$endif}
  {$ifdef use_lcltype}
    LCLIntf.DeleteCriticalSection(fLock); // free the CriticalSection
  {$else}
    fLock.Free;
  {$endif}
  inherited Destroy;
end;

procedure TGuardian.Lock;
begin
  {$ifdef debug}
    ErrorLog.LogLn('TGuardian.Lock ['+DateTimeToStr(now)+'] Locking Thread = '+GetCurrentThreadId.ToString);// log progress
  {$endif}
  {$ifdef use_lcltype}
    LCLIntf.EnterCriticalSection(fLock); // enter the critical section
  {$ifdef debug}
    ErrorLog.LogLn('TGuardian.Lock ['+DateTimeToStr(now)+'] In Lock Thread = '+GetCurrentThreadId.ToString);// log progress
  {$endif}
  {$else}
    fLock.Acquire;
  {$endif}
end;

procedure TGuardian.UnLock;
begin
  {$ifdef use_lcltype}
    LCLIntf.LeaveCriticalSection(fLock); // leave the critical section
  {$else}
    fLock.Release;
  {$endif}
  {$ifdef debug}
    ErrorLog.LogLn('TGuardian.UnLock ['+DateTimeToStr(now)+'] UnLocking Thread = '+GetCurrentThreadId.ToString);// log progress
  {$endif}
end;

initialization
  Singleton:= nil;
finalization
  FreeAndNil(Singleton);
end.

    
