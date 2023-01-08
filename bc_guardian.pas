
{*******************************************************************************
* Unit name : bc_guardian.pas                                                  *
* Copyright : (c) 2020 - 2021 cdbc.dk                                          *
* Programmer: Benny Christensen /bc                                            *
* Created   : 2020.04.13 /bc                                                   *
* Updated   : 2020.04.17 /bc: const SGUIDGuardian                              *
*           : 2020.09.19 /bc: removed debug-state & forked to bc_guardian_dbg  *
*                        /bc: reduced IGuardian                                *
*           : 2021.04.13 /bc: changed IGuardian interface to corba             *
*                                                                              *
*                                                                              *
********************************************************************************
*                                                                              *
* Abstract:                                                                    *
* Guardian is a global singleton, that provides                                *
* locking for threads.                                                         *
* IGuardian is a contract interface, that an                                   *
* implementing class must fulfill.                                             *
*                                                                              *
*******************************************************************************}

unit bc_guardian;
{$mode objfpc}{$H+}
{$define use_lcltype}
{.$define debug}
interface
uses
  Classes, SysUtils,
  bc_types,
  {$ifdef use_lcltype}
    LCLType,
    LCLIntf;
  {$else}
    syncobjs;
  {$endif}

const
  UnitVersion = '4.13.04.2021';
  SGUIDGuardian = '{344C70E4-E6F8-4CDA-BDBE-682A511975D1}';

type
  { corba interfaces do not ref count, i.e: you free them yourself! }
  {$interfaces corba}  
  IGuardian = interface [SGUIDGuardian]
    function BeginWrite: boolean; // api
    procedure EndWrite;           // api
    procedure Lock;               // api
    procedure UnLock;             // api
  end; { IGuardian }

  { TGuardian is a global, that provides locking for threads }
  TGuardian = class(TObject,IGuardian)
  private
    fVersion: TVersion;
    fId: ptrint;
    {$ifdef use_lcltype}
      fLock: LCLType.TCriticalSection; { from lcltype.pp }
    {$else}
      fLock: syncobjs.TCriticalSection; { from syncobjs.pas }
    {$endif}
    function getVersion: TVersion;
//    function getVersion: string;
    function getId: ptrint;
    procedure setId(aValue: ptrint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    function BeginWrite: boolean; { impl. stolen from TSimpleRWSync in SysUtils }
    procedure UnLock;
    procedure EndWrite; { impl. stolen from TSimpleRWSync in SysUtils }
    property Id: ptrint read getId write setId;
    property Version: TVersion read getVersion;
  end; { TGuardian }
  {$interfaces com}
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
end;

function TGuardian.getVersion: TVersion;
begin
  Result:= fVersion;
end;

procedure TGuardian.setId(aValue: ptrint);
begin
  fId:= aValue;
end;

constructor TGuardian.Create;
begin
  inherited Create;
  fVersion:= TVersion.Create(bc_guardian.UnitVersion);
  fId:= 8723;
  {$ifdef use_lcltype}
    LCLIntf.InitializeCriticalSection(fLock); // create the CriticalSection
  {$else}
    fLock:= syncobjs.TCriticalSection.Create;
  {$endif}
end;

destructor TGuardian.Destroy;
begin
  {$ifdef use_lcltype}
    LCLIntf.DeleteCriticalSection(fLock); // free the CriticalSection
  {$else}
    fLock.Free;
  {$endif}
  FreeAndNil(fVersion);
  inherited Destroy;
end;

procedure TGuardian.Lock;
begin
  {$ifdef use_lcltype}
    LCLIntf.EnterCriticalSection(fLock); // enter the critical section
  {$else}
    fLock.Acquire;
  {$endif}
end;

function TGuardian.BeginWrite: boolean;
begin
  LCLIntf.EnterCriticalSection(fLock); // enter the critical section
  Result:= true;
end;

procedure TGuardian.UnLock;
begin
  {$ifdef use_lcltype}
    LCLIntf.LeaveCriticalSection(fLock); // leave the critical section
  {$else}
    fLock.Release;
  {$endif}
end;

procedure TGuardian.EndWrite;
begin
  LCLIntf.LeaveCriticalSection(fLock); // leave the critical section
end;

initialization
  Singleton:= nil;
finalization
  FreeAndNil(Singleton);
end.

    
