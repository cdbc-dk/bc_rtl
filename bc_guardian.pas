
{*******************************************************************************
* Unit name : bc_guardian.pas                                                  *
* Copyright : (c) 2020 cdbc.dk                                                 *
* Programmer: Benny Christensen /bc                                            *
* Created   : 2020.04.13 /bc                                                   *
* Updated   : 2020.04.17 /bc: const SGUIDGuardian                              *
*           : 2020.09.19 /bc: removed debug-state & forked to bc_guardian_dbg  *
*                        /bc: reduced IGuardian                                *
*                                                                              *
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
  UnitVersion = '3.02.02.2021';
  SGUIDGuardian = '{344C70E4-E6F8-4CDA-BDBE-682A511975D1}';

type
  {$interfaces corba}  
  IGuardian = interface [SGUIDGuardian]
    procedure Lock;   // api
    procedure UnLock; // api
  end; { IGuardian }

  { TGuardian is a global, that provides locking for threads }
  TGuardian = class(TInterfacedObject,IGuardian)
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
    procedure UnLock;
    property Id: ptrint read getId write setId;
    property Version: TVersion read getVersion;
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

procedure TGuardian.UnLock;
begin
  {$ifdef use_lcltype}
    LCLIntf.LeaveCriticalSection(fLock); // leave the critical section
  {$else}
    fLock.Release;
  {$endif}
end;

initialization
  Singleton:= nil;
finalization
  FreeAndNil(Singleton);
end.

    
