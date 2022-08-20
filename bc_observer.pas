{------------------------------------------------------------------------------|
| Project name: Daily Diary                                                    |
| Unit name   : bc_observer.pas                                                |
| Copyright   : (c) 2020-2021 cdbc.dk                                          |
| Programmer  : Benny Christensen /bc                                          |
| Created     : 2020.09.30 /bc initial design and coding,(observer pattern)    |
| Updated     : 2020.09.30 /bc Setting up environment, structure and vision    |
|                          /bc refactored observer to live in his own unit     |
|             : 2021.01.09 /bc refactored observer added fActiveObject         |
|             : 2021.01.10 /bc refactored observed added fSubject
|             : 2021.02.07 /bc added const to tobserver.create
|                                                                              |
|------------------------------------------------------------------------------|
| Abstract:                                                                    |
|   This unit implements the observer pattern.                                 |
|   TObserved, a subject to observe                                            |
|   TObserver, one or more observers connected to the subject.                 |
|                                                                              |
|------------------------------------------------------------------------------|
| License:                                                                     |
|   Beer-license, that means, if we meet, you buy me a beer :-D                |
|   I'm not liable for anything, Use at your own risk!                         |
|                                                                              |
-------------------------------------------------------------------------------}

unit bc_observer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils;

const
  UnitVersion = '03.07.02.2021'; { 4.th version }

type
  { datarecord for opbservers }
  PObserverRec = ^TObserverRec;
  TObserverRec = packed record
    orMin: ptruint;
    orMax: ptruint;
    orPos: ptruint;
    orText: string;
    orReserve: ptruint;
  end;

  { TObserved ~ observer pattern }
  TObserved = class(TInterfacedObject,IFPObserved)
  private
    fObservers: TFPList;
  protected
    fSubject: TObject;
    fExtra: ptruint;
  public
    constructor Create(aSubject: TObject); { the subject object you wish to monitor }
    destructor Destroy; override;
    Procedure FPOAttachObserver(AObserver : TObject); { attach a new observer }
    Procedure FPODetachObserver(AObserver : TObject); { detach an observer }
    Procedure FPONotifyObservers(ASender : TObject; AOperation : TFPObservedOperation; Data : Pointer); { Notify all observers of a change }
    property Extra: ptruint read fExtra write fExtra; { well, you never know :-) }
    property Subject: TObject read fSubject write fSubject;
  end; { TObserved }

  { TObserver ~ observer pattern }
  TObserver = class(TInterfacedObject,IFPObserver)
  protected
    fActiveObject: TObject; { depends on the name of the observer, what class of object it is }
  public
    constructor Create(const anActiveobject: TObject);
    destructor Destroy; override;
    Procedure FPOObservedChanged(aSender: TObject;Operation: TFPObservedOperation;Data: Pointer); virtual;
    property ActiveObject: TObject read fActiveObject write fActiveObject;
  end; { TObserver }

implementation
uses RtlConsts;

{ *** TObserver *** }
constructor TObserver.Create(const anActiveobject: TObject);
begin
  inherited Create;
  fActiveObject:= anActiveobject; { assign the working object }
end;

destructor TObserver.Destroy;
begin
  fActiveObject:= nil; { release the link }
  inherited Destroy;
end;

procedure TObserver.FPOObservedChanged(aSender: TObject;Operation: TFPObservedOperation;Data: Pointer);
var
  PObsRec: PObserverRec;
begin
  { example code to be overridden! }
  raise EObserver.Create('ERROR: TObserver Method "FPOObservedChanged" not overridden!');
end;

{ *** TObserved *** }
constructor TObserved.Create(aSubject: TObject);
begin
  inherited Create;
  fSubject:= aSubject; { just typecast to say, a TEdit, or your own creation :-) }
  fObservers:= TFPList.Create; { 06.02.2021 /bc }
end;

destructor TObserved.Destroy;
begin
  fSubject:= nil;
  if assigned(fObservers) then FreeAndNil(fObservers);
  inherited Destroy;
end;

procedure TObserved.FPOAttachObserver(aObserver: TObject);
begin
  { fobservers has already been created in constructor }
//  if not assigned(fObservers) then fObservers:= TFPList.Create; { lazy creation }
  fObservers.Add(pointer(aObserver));
end;

procedure TObserved.FPODetachObserver(aObserver: TObject);
begin
  if assigned(fObservers) then begin
    fObservers.Remove(pointer(aObserver));
//    if (fObservers.Count = 0) then FreeAndNil(fObservers);
  end;
end;

{ notify each observer in the list of observers, aSender is the subject, NOT Self }
procedure TObserved.FPONotifyObservers(aSender: TObject;aOperation: TFPObservedOperation;Data: pointer);
var
  I: integer;
  Obs: TObserver;
begin
  if assigned(fObservers) then begin
    for I:= fObservers.Count-1 downto 0 do begin
      Obs:= TObserver(fObservers[I]);
      Obs.FPOObservedChanged(fSubject,aOperation,Data);
      Obs:= nil;
    end;
  end;
end;

end.

