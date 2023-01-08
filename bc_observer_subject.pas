{------------------------------------------------------------------------------|
| Project name: bc_observer_subject                                            |
| Unit name   : bc_observer_subject.pas                                        |
| Copyright   : (c) 2021-2022 cdbc.dk                                          |
| Programmer  : Benny Christensen /bc                                          |
| Created     : 2021.02.25 /bc initial design and coding,(observer pattern)    |
| Updated     : 2021.02.25 /bc Setting up environment, structure and vision    |
|                          /bc refactored observer to live in it's own unit    |
|             : 2021.01.09 /bc refactored observer added fActiveObject         |
|             : 2021.01.10 /bc refactored observed added fSubject              |
|             : 2021.02.07 /bc added const to tobserver.create                 |
|                                                                              |
|                                                                              |
|------------------------------------------------------------------------------|
| Abstract:                                                                    |
|   This unit implements the observer pattern.                                 |
|   TbcSubject, a subject to observe                                           |
|   TbcObserver, one or more observers connected to the subject.               |
|                                                                              |
|------------------------------------------------------------------------------|
| License:                                                                     |
|   Beer-license, that means, if we meet, you'll buy me a beer :-D             |
|   I'm not liable for anything, Use at your own risk!!!                       |
|                                                                              |
-------------------------------------------------------------------------------}

unit bc_observer_subject;
{$mode objfpc}{$H+}
{$define debug}
interface
uses
  Classes, SysUtils;

const
  UnitVersion = '00.02.25.2021'; { 1.st version }
  { GUID for IbcSubject and IbcObserver }
  SGUIDIbcSubject  = '{A036909B-DB8A-4426-843E-24DC452A7100}';
  SGUIDIbcObserver = '{D013282B-CA63-497D-AF2D-BF10D2C34A82}';

type
  { Notification operations : }
  { Subject has changed, is freed, item added to/deleted from list, custom event. }
  TbcSubjectOperation = (ooChange,ooFree,ooAddItem,ooDeleteItem,ooCustom);
  {$interfaces corba} { NO reference counting! }
  { IbcSubject to be observed }
  IbcSubject = interface [SGUIDIbcSubject]
    { attach a new observer }
    procedure bcAttachObserver(anObserver: TObject);
    { Detach an observer }
    procedure bcDetachObserver(anObserver: TObject);
    { Notify all observers of a change. }
    procedure bcNotifyObservers(aSender: TObject;anOperation: TbcSubjectOperation;aData: pointer);
  end; { IbcSubject }
  { IbcObserver }
  IbcObserver = interface [SGUIDIbcObserver]
    { Called by subject when observers are notified. }
    procedure bcSubjectChanged(aSender: TObject;anOperation: TbcSubjectOperation;aData: pointer); virtual;
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
    procedure bcAttachObserver(anObserver: TObject); { attach a new observer }
    procedure bcDetachObserver(anObserver: TObject); { detach an observer }
    procedure bcNotifyObservers(aSender: TObject;anOperation: TbcSubjectOperation;aData: pointer); { Notify all observers of a change }
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
    procedure bcSubjectChanged(aSender: TObject;anOperation: TbcSubjectOperation;aData: pointer); virtual;
    property Owner: TObject read fOwner write fOwner;
  end; { TbcObserver }

implementation
uses RtlConsts;

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


end.

