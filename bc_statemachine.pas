{ this unit contains the TbcStateMachine, it needs state-classes to function,
  * see csv_states.pas for an example.
  one registers them in order to parse stuff, that one parses csv-files.
  E.g:
  prs: TbcStateMachine;
  prs:= TbcStateMachine.Create;
  prs.RegisterState(TCSVStartFieldState.Create(prs)); // idx 0 "start" must be registered first!
  prs.RegisterState(TCSVScanFieldState.Create(prs)); // idx 1 etc...
  for I:= 0 to sl.count-1 do prs.Execute(sl[i],fl); // fl is a fieldlist ~ TSTringList, sl too
  prs.Free;
  The idea is, you create a unit with the new states you need and registers them with the statemachine
  Have fun ;-)
}
unit bc_statemachine;
{$mode objfpc}{$H+}
{.$define debug}
interface
uses
  Classes, SysUtils, Contnrs;
const
  unitVersion = '02.05.01.2023';
type                                        (* when you see {%H-} in the code *)
  TbcStateMachine = class;                    (* it's there to suppress hints *)
  TbcStateClass = class of TbcState;
  { TbcState abstract State object ancestor }
  TbcState = class(TObject)
  protected
    fMachine: TbcStateMachine;
    procedure AddCharToCurrentField(Ch: char); virtual;
    procedure AddCurrentFieldToList; virtual;
    { loose coupling, it doesn't know the state, just the type of state, to come next }
    procedure ChangeState(NewState: TbcStateClass); virtual;
    { tells the state-machine to fire its callback with data accumulated and clear buffers }
    procedure FireDataEvent;
    { used in descendant classes when they choose not to implement a method }
    procedure RaiseErrorNotImplemented(aSender: TObject;aMethodName: string);
  public
    constructor Create(aMachine: TbcStateMachine);
    { Must be implemented in the concrete sub-classes to handle the input
      character and decide on the next state. }
    procedure ProcessChar(Ch: AnsiChar;Pos: integer); virtual;
    { Must be implemented in the concrete sub-classes to handle the input
      character as a pchar and decide on the next state. }
    procedure ProcessPchar(P: pchar;Pos: integer); virtual;
  end; { TbcState }
  { TOnStateDataEvent is a data event for executing entire files }
  TOnStateDataEvent = procedure(aSender: TbcState;aFieldList: TStrings;var Cancel: boolean) of object;
  { TbcStateList is an internal list of registered states, used in TbcStateMachine }
  TbcStateList = class
  private
    fList: TFPObjectList;
    fOwnsObjects: boolean;
    function get_Count: integer;
    function get_Item(Index: integer): TbcState;
    procedure set_Item(Index: integer;aValue: TbcState);
  public
    constructor Create(OwnsObjects: boolean = true);
    destructor Destroy; override;
    function AddState(aState: TbcState): integer;
    procedure Clear;
    function GetStateByClass(aClass: TbcStateClass): TbcState;
    function GetStateByName(const aName: string): TbcState;
    property Count: integer read get_Count;
    property Items[Index: integer]: TbcState read get_Item write set_Item; default;
  end; { TbcStateList }

  { TbcStateMachine the state machine... }
  TbcStateMachine = class(TObject)
  private
    fCancel: boolean;
    { cached state classes registered for performance }
    fCachedStates: TbcStateList;
    { Fields used during parsing }
    fCurrentField: string;
    fCurrentLine: string;
    { used to ascertain eol }
    fCurrLineLen: integer;
    { the resulting fields from current line }
    fFieldList: TStrings;
    fOnStateData: TOnStateDataEvent;
    { our chameleon, changes state according to data }
    fState: TbcState;
    fTag: ptrint; { sort of like a cookie }
    fUserData: pointer; { sort of like a cookie }
    function get_State: TbcStateClass;
    procedure set_State(const aValue: TbcStateClass);
  protected
    procedure AddCharToCurrentField(Ch: char); virtual;
    procedure AddCurrentFieldToList; virtual;
    procedure DoOnStateData(aSender: TbcState);
  public
    constructor Create;
    destructor Destroy; override;
    { internally deals with ansichar }
    procedure ExecuteLine(const S: string;aFieldList: TStrings); virtual;
    { internally deals with pchar }
    procedure ExecuteFile(aFile: TStringList); virtual;
    { registers the different states, machine can not run without them }
    procedure RegisterState(aState: TbcState);
    property Cancelled: boolean read fCancel write fCancel;
    property CurrentLine: string read FCurrentLine;
    property LineLen: integer read fCurrLineLen;
    property OnStateData: TOnStateDataEvent read fOnStateData write fOnStateData; { fires when executing entire files }
    property State: TbcStateClass read get_State write set_State; { !!!DO NOT USE DIRECTLY!!! }
    property Tag: ptrint read fTag write fTag; { cookie for user use }
    property UserData: pointer read fUserData write fUserData; { cookie for user use }
  end; { TbcStateMachine }

implementation
{ TbcState }
procedure TbcState.ChangeState(NewState: TbcStateClass);
begin
  fMachine.State:= NewState;
end;

procedure TbcState.AddCharToCurrentField(Ch: char);
begin
  fMachine.AddCharToCurrentField(Ch);
end;

procedure TbcState.AddCurrentFieldToList;
begin
  fMachine.AddCurrentFieldToList;
end;

procedure TbcState.FireDataEvent;
begin
  fMachine.DoOnStateData(Self);
end;

procedure TbcState.RaiseErrorNotImplemented(aSender: TObject;aMethodName: string);
begin
  raise ENotImplemented.CreateFmt('Sorry, %s.%s is not implemented yet.',[aSender.ClassName,aMethodName]);
end;

constructor TbcState.Create(aMachine: TbcStateMachine);
begin
  inherited Create;
  fMachine:= aMachine;
end;

procedure TbcState.ProcessChar(Ch: AnsiChar; Pos: integer);
begin
  RaiseErrorNotImplemented(Self,'ProcessChar');
end;

procedure TbcState.ProcessPchar(P: pchar; Pos: integer);
begin
  RaiseErrorNotImplemented(Self,'ProcessPchar');
end;

{ TbcStateMachine }
function TbcStateMachine.get_State: TbcStateClass;
begin
  Result:= TbcStateClass(fState.ClassType);
end; { TbcStateMachine.get_State }

procedure TbcStateMachine.set_State(const aValue: TbcStateClass);
begin
  fState:= fCachedStates.GetStateByClass(aValue);
  if fState = nil then raise Exception.CreateFmt('TbcStateMachine.set_State, Error: %s is not a registered state!',[aValue.ClassName]);
end; { TbcStateMachine.set_State }

procedure TbcStateMachine.AddCharToCurrentField(Ch: char);
begin
  fCurrentField+= Ch;
end; { TbcStateMachine.AddCharToCurrentField }

procedure TbcStateMachine.AddCurrentFieldToList;
begin
  fFieldList.Add(fCurrentField);
  fCurrentField:= '';
end; { TbcStateMachine.AddCurrentFieldToList }

procedure TbcStateMachine.DoOnStateData(aSender: TbcState);
begin
  if Assigned(fOnStateData) then begin
    fOnStateData(aSender,fFieldList,fCancel);
    fFieldList.Clear;
    fCurrentField:= '';
  end;
end; { TbcStateMachine.DoOnStateData }

constructor TbcStateMachine.Create;
begin
  inherited Create;
  fFieldList:= TStringList.Create;
  fCachedStates:= TbcStateList.Create(true); { owns the objects within and will free them on exit }
  fState:= nil; { we've got no registered state as our "start" state, thus nil }
end; { TbcStateMachine.Create }

destructor TbcStateMachine.Destroy;
begin
  fCachedStates.Free; { owns the objects within and will free them }
  fFieldList.Free;
  inherited Destroy;
end; { TbcStateMachine.Destroy }

procedure TbcStateMachine.ExecuteLine(const S: string; aFieldList: TStrings);
var I: integer;
begin
  { if any registered, assume the first registered state is our "start" state }
  if fCachedStates.Count > 0 then fState:= fCachedStates[0]
  else raise Exception.Create('TbcStateMachine.Execute: Error, NO states registered!'+#10+' Please use RegisterState before calling Execute. E.g.: SM.RegisterState(TCSVStartFieldState.Create(SM));');
  fFieldList.Clear; fCancel:= false;
  fCurrentLine:= S;
  fCurrLineLen:= Length(fCurrentLine);
  { read through all the characters in the string }
  for I:= 1 to Length(S) do begin
    { get the next character and process it }
    fState.ProcessChar(S[i],I);
  end;
  aFieldList.Text:= fFieldList.Text; { assign our results }
end; { TbcStateMachine.Execute }

procedure TbcStateMachine.ExecuteFile(aFile: TStringList);
var P,Ps,Pe: pchar;
begin
  { if any registered, assume the first registered state is our "start" state }
  if fCachedStates.Count > 0 then fState:= fCachedStates[0]
  else raise Exception.Create('TbcStateMachine.ExecuteFile: Error, NO states registered!'+#10+' Please use RegisterState before calling Execute. E.g.: SM.RegisterState(TCSVStartFieldState.Create(SM));');
  fFieldList.Clear; fCancel:= false;
  fCurrentLine:= aFile.Text;
  fCurrLineLen:= Length(fCurrentLine);
  Ps:= pchar(fCurrentLine); P:= Ps; Pe:= Ps + fCurrLineLen;
  while P <= Pe do begin
    fState.ProcessPchar(P,(P-Ps)); { pchars are 0-based... could add 1 to pos, let us see }
    if fCancel then break;
    inc(P);
  end;
end; { TbcStateMachine.ExecuteFile }

procedure TbcStateMachine.RegisterState(aState: TbcState);
begin
  fCachedStates.AddState(aState); { add to our internal list of cached parserstates }
end; { TbcStateMachine.RegisterState }

{ TbcStateList }
function TbcStateList.get_Count: integer;
begin
  Result:= fList.Count;
end;

function TbcStateList.get_Item(Index: integer): TbcState;
begin
  Result:= TbcState(fList.Items[Index]);
end;

procedure TbcStateList.set_Item(Index: integer; aValue: TbcState);
begin
  if fOwnsObjects then fList.Items[Index].Free;
  fList.Items[Index]:= aValue;
end;

constructor TbcStateList.Create(OwnsObjects: boolean);
begin
  inherited Create;
  fList:= TFPObjectList.Create(OwnsObjects);
  fOwnsObjects:= OwnsObjects;
end;

destructor TbcStateList.Destroy;
begin
  Clear; { frees objects if owned }
  inherited Destroy;
end;

function TbcStateList.AddState(aState: TbcState): integer;
begin
  Result:= fList.Add(aState);
end;

procedure TbcStateList.Clear;
begin
  fList.Clear; { frees objects per default }
end;

function TbcStateList.GetStateByClass(aClass: TbcStateClass): TbcState;
var I: integer;
begin
  Result:= nil;
  for I:= 0 to fList.Count-1 do begin
    if fList[I].ClassType = aClass.ClassType then begin
      Result:= TbcState(fList[I]);
      break;
    end;
  end;
end;

function TbcStateList.GetStateByName(const aName: string): TbcState;
var I: integer;
begin
  Result:= nil;
  for I:= 0 to fList.Count-1 do begin
    if fList[I].ClassName = aName then begin
      Result:= TbcState(fList[I]);
      break;
    end;
  end;
end;

end.
