unit uObserver;

{$MODE Delphi}

interface

uses
  SysUtils, Classes;

type
  ISubject = interface; // forward decl.
  IObserver = interface ['{4578802F-6E7E-4C48-90A8-25A12D26F598}']
    procedure Update(aSubject: ISubject); // IInterface
  end;

  ISubject = interface ['{A3C906A1-0F90-486C-937D-07F91B58C8F8}']
    procedure Register_Observer(anObserver: IObserver);
    procedure UnRegister_Observer(anObserver: IObserver);
    procedure Notify;
    function get_State: integer;
    procedure set_State(const Value: integer);
    property State: integer read get_State write set_State;
  end;

  // updateevent to be fed in constructor
  TUpdate_Method = procedure(aSubject: ISubject) of object;

  { concrete objects implementing the interfaces }
  TObserver = class(TInterfacedObject,IObserver)
  private
    fUpdate_Event: TUpdate_Method;
    procedure Update(aSubject: ISubject);
  public
    constructor Create(anUpdateMEthod: TUpdate_Method); overload;
    constructor Create(aSubject: ISubject;
                       anUpdateMEthod: TUpdate_Method); overload;
  end;

  TSubject = class(TInterfacedObject,ISubject)
  private
    fObservers: TList;
    fState: integer;
    procedure Register_Observer(anObserver: IObserver);
    procedure UnRegister_Observer(anObserver: IObserver);
    procedure Notify;
    function get_State: integer;
    procedure set_State(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;
    property State: integer read get_State write set_State;
  end;


implementation

{ TObserver }
constructor TObserver.Create(anUpdateMEthod: TUpdate_Method);
begin
  inherited Create;
  fUpdate_Event:= anUpdateMEthod;
end;

constructor TObserver.Create(aSubject: ISubject;
                             anUpdateMEthod: TUpdate_Method);
begin
  inherited Create;
  fUpdate_Event:= anUpdateMEthod;
  aSubject.Register_Observer(Self);
end;

procedure TObserver.Update(aSubject: ISubject);
begin
  if assigned(fUpdate_Event) then fUpdate_Event(aSubject); 
end;

{ TSubject }
constructor TSubject.Create;
begin
  inherited;
  fObservers:= TList.Create;
end;

destructor TSubject.Destroy;
begin
  fObservers.Clear;
  fObservers.Free;
  inherited;
end;

function TSubject.get_State: integer;
begin
  Result:= fState;
end;

procedure TSubject.Notify;
var I: integer;
begin
  for I:= 0 to fObservers.Count - 1 do IObserver(fObservers[I]).Update(Self);    
end;

procedure TSubject.Register_Observer(anObserver: IObserver);
begin
  if fObservers.IndexOf(pointer(anObserver)) = -1 then fObservers.Add(pointer(anObserver));
end;

procedure TSubject.set_State(const Value: integer);
begin
   fState:= Value;
end;

procedure TSubject.UnRegister_Observer(anObserver: IObserver);
var I: integer;
begin
  I:= fObservers.IndexOf(pointer(anObserver));
  if I <> -1 then fObservers.Delete(I);
end;

end.