
{**************************************************************************
* Unit name : bc_types.pas                                                *
* Copyright : (c) 2020-2021 cdbc.dk                                       *
* Programmer: Benny Christensen /bc                                       *
* Created   : 2020.04.18 /bc: TVersion created.                           *
* Updated   : 2020.04.20 /bc: Refactored the math in both                 *
*                             TVersion.EncodeFromString and               *
*                             TVersion.DecodeToString. it                 *
*                             now relies on pointer magic :-)             *
*                             Implementation moved to include.            *
*           : 2021.02.26 /bc: Implemented the observer pattern            *
*                             Headers here and implementation             *
*                             in observer.inc                             *
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
  UnitVersion = '01.02.27.2021'; { 2.nd version }
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

{ factory provides a global object, singleton on demand }
//Function Factory: TFactory;

implementation

{$i version.inc} { moved the implementation part to an include-file }
{$i observer.inc} { moved the implementation part to an include-file }


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

