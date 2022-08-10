
{**************************************************************************
* Unit name : bc_types.pas                                                *
* Copyright : (c) 2020-2022 cdbc.dk                                       *
* Programmer: Benny Christensen /bc                                       *
* Created   : 2020.04.18 /bc: TVersion created.                           *
* Updated   : 2020.04.20 /bc: Refactored the math in both                 *
*                             TVersion.EncodeFromString and               *
*                             TVersion.DecodeToString. it                 *
*                             now relies on pointer magic :-)             *
*                             Implemented 2 class methods for conversion. *
*           : 2021.02.26 /bc: Implemented the observer pattern.           *
*                             TbcSubject & TbcObserver.                   *
*           : 2022.07.20 /bc: Implemented TbcNamedMemoryStream.           *
*                                                                         *
*                                                                         *
*                                                                         *
*                                                                         *
***************************************************************************
*                                                                         *
* Abstract:                                                               *
* Provides different types for use in bc-codelibrary,                     *
* sort of a global type-repository.                                       *
*                                                                         *
***************************************************************************
* License:                                                                *
* Beer-license, if we meet one day, you'll buy me a beer :-)              *
* I'm NOT liable for anything!!! Use at your own risk!!!                  *
*                                                                         *
**************************************************************************}

unit bc_types;
{$mode objfpc}{$H+}
{$define debug}

interface
uses
  Classes, SysUtils,
  RtlConsts;
  {$ifdef use_lcltype}

  {$else}

  {$endif}
const
  UnitVersion = '03.02.08.2022'; { 4.th version }
  { GUID for IbcSubject and IbcObserver }
  SGUIDIbcSubject  = '{A036909B-DB8A-4426-843E-24DC452A7100}';
  SGUIDIbcObserver = '{D013282B-CA63-497D-AF2D-BF10D2C34A82}';

type
  { *** TVersion *** }
  TVersion = class
  private
    fVersionNumber: ptruint;
    function get_AsPtrUint: ptruint;
    function get_AsString: string;
    function EncodeFromString(const aVersionString: string): ptruint;
    function DecodeToString(const aVersionNumber: ptruint): string;
  public
    constructor Create(const aVersionNumber: ptruint); overload;
    constructor Create(const aUnitVersion: string); overload;
    destructor Destroy; override;
    class function VersionNumberToString(const aVersionNumber: ptruint): string;
    class function VersionStringToPtrUint(const aVersionString: string): ptruint;
    property AsPtrUint: ptruint read get_AsPtrUint;
    property AsString: string read get_AsString;
  end; { TVersion }

  { Notification operations : }
  { Subject has changed, is freed, item added to/deleted from list, custom event. }
  TbcSubjectOperation = (ooChange,ooFree,ooAddItem,ooDeleteItem,ooCustom);
  {$interfaces corba} { NO reference counting! }
  { IbcSubject to be observed }
  IbcSubject = interface [SGUIDIbcSubject]
    { attach a new observer }
    Procedure bcAttachObserver(anObserver: TObject);
    { Detach an observer }
    Procedure bcDetachObserver(anObserver: TObject);
    { Notify all observers of a change. }
    Procedure bcNotifyObservers(aSender: TObject;anOperation: TbcSubjectOperation;aData: pointer);
  end; { IbcSubject }
  { IbcObserver }
  IbcObserver = interface [SGUIDIbcObserver]
    { Called by subject when observers are notified. }
    Procedure bcSubjectChanged(aSender: TObject;anOperation: TbcSubjectOperation;aData: pointer); virtual;
  end;
  {$interfaces com} { reference counted! }
  { TSubject to be observed ~ observer pattern }
  TbcSubject = class(TObject,IbcSubject)
  private
    fObservers: TFPList;
  protected
    fOwner: TObject;
    fExtra: ptruint;
  public
    constructor Create(anOwner: TObject); { the subject object you wish to monitor }
    destructor Destroy; override;
    Procedure bcAttachObserver(anObserver: TObject); { attach a new observer }
    Procedure bcDetachObserver(anObserver: TObject); { detach an observer }
    Procedure bcNotifyObservers(aSender: TObject;anOperation: TbcSubjectOperation;aData: pointer); { Notify all observers of a change }
    property Extra: ptruint read fExtra write fExtra; { well, you never know :-) }
    property Owner: TObject read fOwner write fOwner;
  end; { TbcSubject }

  { TbcObserver "I'm watching you" ~ observer pattern }
  TbcObserver = class(TObject,IbcObserver)
  protected
    fOwner: TObject; { depends on the name of the observer, what class of object it is }
  public
    constructor Create(const anOwner: TObject);
    destructor Destroy; override;
    Procedure bcSubjectChanged(aSender: TObject;anOperation: TbcSubjectOperation;aData: pointer); virtual;
    property Owner: TObject read fOwner write fOwner;
  end; { TbcObserver }

  { TbcNamedMemoryStream ~ knows its own name, useful in "blobs" }
  TbcNamedMemoryStream = class(TMemoryStream)
  protected
    fName: string; { where am i coming from or going to??? }
  public
    constructor Create; overload;
    constructor Create(const aName: string); overload;
    destructor Destroy; override;
//    Procedure bcSubjectChanged(aSender: TObject;anOperation: TbcSubjectOperation;aData: pointer); virtual;
    property Name: string read fName write fName;
  end; { TbcNamedMemoryStream }

{ factory provides a global object, singleton on demand }
//Function Factory: TFactory;

implementation
uses bc_strings; { provides StringWorkshop, requires UnitVersion >= '3.20.04.2020' /bc }

type
  PPtruint = ^ptruint;        { pointer magic }
  PVersionRec = ^TVersionRec; { pointer magic }
  TVersionRec = packed record
    vrBuild_Number,
    vrMicro_Number,
    vrMinor_Number,
    vrMajor_Number: word; { 2 bytes each = 8 bytes -> size of ptruint on 64 bit }
  end;

{ *** TVersion *** }
function TVersion.get_AsPtrUint: ptruint;
begin
  Result:= fVersionNumber;
end;

function TVersion.get_AsString: string;
begin
  Result:= DecodeToString(fVersionNumber);
end;

function TVersion.EncodeFromString(const aVersionString: string): ptruint;
var
  S: string;
  VRec: TVersionRec;
begin
  Result:= 0; { it's unsigned, hence the zero }
  { first off, pick the string apart, we know the format :-) }
  S:= StringWorkshop.GetFieldToken(1,aVersionString,'.');
  VRec.vrMajor_Number:= StrToInt(S);
  S:= StringWorkshop.GetFieldToken(2,aVersionString,'.');
  VRec.vrMinor_Number:= StrToInt(S);
  S:= StringWorkshop.GetFieldToken(3,aVersionString,'.');
  VRec.vrMicro_Number:= StrToInt(S);
  S:= StringWorkshop.GetFieldToken(4,aVersionString,'.');
  VRec.vrBuild_Number:= StrToInt(S);
  { now calculate the result }
  Result:= PPtruint(@VRec)^; { pointer magic }
end;

function TVersion.DecodeToString(const aVersionNumber: ptruint): string;
var
  VRec: TVersionRec;
begin
  Result:= '';                          { initialize result }
  VRec:= PVersionRec(@aVersionNumber)^; { pointer magic, tversionrec is 8 bytes & ptruint is also 8 bytes }
  { now encode the result string, single digits will be zero-padded }
  Result:= StringWorkshop.IntToStrPad0(VRec.vrMajor_Number)+'.'+
           StringWorkshop.IntToStrPad0(VRec.vrMinor_Number)+'.'+
           StringWorkshop.IntToStrPad0(VRec.vrMicro_Number)+'.'+
           StringWorkshop.IntToStrPad0(VRec.vrBuild_Number);
end;

constructor TVersion.Create(const aVersionNumber: ptruint);
begin
  inherited Create;
  fVersionNumber:= aVersionNumber;
end;

constructor TVersion.Create(const aUnitVersion: string);
begin
  inherited Create;
  fVersionNumber:= EncodeFromString(aUnitVersion);
end;

destructor TVersion.Destroy;
begin
  inherited Destroy;
end;

class function TVersion.VersionNumberToString(const aVersionNumber: ptruint): string;
begin
  with TVersion.Create(aVersionNumber) do begin
    Result:= AsString;
    Free;
  end;
end;

class function TVersion.VersionStringToPtrUint(const aVersionString: string): ptruint;
begin
  with TVersion.Create(aVersionString) do begin
    Result:= AsPtrUint;
    Free;
  end;
end;
{ *** TVersion *** }

{ *** TbcObserver *** }
constructor TbcObserver.Create(const anOwner: TObject);
begin
  inherited Create;
  fOwner:= anOwner; { link }
end;

destructor TbcObserver.Destroy;
begin
  fOwner:= nil; { just unlink }
  inherited Destroy;
end;

procedure TbcObserver.bcSubjectChanged(aSender: TObject;
                                       anOperation: TbcSubjectOperation;
                                       aData: pointer);
begin
  case anOperation of
    ooAddItem: begin
                 // to be overridden
               end;
    ooChange:  begin
                 // to be overridden
               end;
    ooDeleteItem: begin
                    // to be overridden
                  end;
    ooFree:       begin
                    // to be overridden
                  end;
    ooCustom:     begin
                    // to be overridden
                  end;
  end; { case }
end;

{ *** TbcSubject *** }
constructor TbcSubject.Create(anOwner: TObject);
begin
  inherited Create;
  fOwner:= anOwner;
end;

destructor TbcSubject.Destroy;
begin
  if assigned(fObservers) then begin
    bcNotifyObservers(Owner,ooFree,nil);
    FreeAndNil(fObservers);
  end;
  inherited Destroy;
end;

procedure TbcSubject.bcAttachObserver(anObserver: TObject);
var I: IbcObserver;
begin
  if not anObserver.GetInterface(SGUIDIbcObserver,I) then
    raise EObserver.CreateFmt(SErrNotObserver,[anObserver.ClassName]);
  if not assigned(fObservers) then fObservers:= TFPList.Create;
  fObservers.Add(I);
end;

procedure TbcSubject.bcDetachObserver(anObserver: TObject);
var I: IbcObserver;
begin
  if not anObserver.GetInterface(SGUIDIbcObserver,I) then
    raise EObserver.CreateFmt(SErrNotObserver,[anObserver.ClassName]);
  if assigned(fObservers) then begin
    fObservers.Remove(I);
    if (fObservers.Count=0) then FreeAndNil(fObservers);
  end;
end;

procedure TbcSubject.bcNotifyObservers(aSender: TObject;
                                       anOperation: TbcSubjectOperation;
                                       aData: pointer);
var
  Idx: integer;
  Obs: IbcObserver;
begin
  if assigned(fObservers) then for Idx:= fObservers.Count-1 downto 0 do begin
    Obs:= IbcObserver(fObservers[Idx]);
    Obs.bcSubjectChanged(Owner,anOperation,aData);
  end;
end;
{ *** TbcObserver & TbcSubject *** }

{ *** TbcNamedMemoryStream *** }
constructor TbcNamedMemoryStream.Create;
begin
  inherited Create;
  fName:= '';
end;

constructor TbcNamedMemoryStream.Create(const aName: string);
begin
  inherited Create;
  fName:= aName;
end;

destructor TbcNamedMemoryStream.Destroy;
begin
  inherited Destroy;
end;
{ *** TbcNamedMemoryStream *** }

(*
var
  Singleton: TSingleton; { a mix of the factory and the singleton patterns }

Function Factory: TFactory;
begin
  if not assigned(Singleton) then Singleton:= TFactory.Create;
  Result:= Singleton;
end; { gets released on progam end }
*)












initialization
//  Singleton:= nil;
finalization
//  FreeAndNil(Singleton);
end.

