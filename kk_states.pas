{ this unit contains the typedeclarations for states used in TbcStateMachine,
  one registers them in order to parse stuff, this one parses kk-files.
  E.g:
  prs: TbcStateMachine;
  prs:= TbcStateMachine.Create;
  prs.Tag:= 0;
  prs.OnStateData:= @THackStateData.HandleStateData; // callback event, when data is ready
  prs.RegisterState(TkkDataState.Create(prs)); // idx 0 "start" must be registered first!
  prs.RegisterState(TkkHeadState.Create(prs)); // idx 1 etc...
  for I:= 0 to sl.count-1 do prs.ExecuteLine(sl[i],fl); // uses no events 
  OR prs.ExecuteFile(sl); // sl is a text-file loaded TSTringList
  prs.Free;
  P.s: the implementation is here too ;-)
  The idea is, you create a unit with the new states you need and registers them with the statemachine
}
unit kk_states;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, bc_statemachine;
const
  unitVersion = '01.08.01.2023';
type
  { support record to be used with state-machine, optionally }
  TSupportRec = record
    Head,Version,ColHead,ColFoot: string;
  end;
  { TkkDataState A concrete state object used when starting or idle'ing }
  TkkDataState = class(TbcState)
  public
    procedure ProcessPchar(P: pchar; Pos: integer); override;
  end;
  { TkkHeadState A concrete state object used to extract header of file }
  TkkHeadState = class(TbcState)
  public
    procedure ProcessPchar(P: pchar; Pos: integer); override;
  end;
  { TkkVersState A concrete state object used to extract version of file }
  TkkVersState = class(TbcState)
  public
    procedure ProcessPchar(P: pchar; Pos: integer); override;
  end;
  { TkkColumnHeadState A concrete state object used to extract column headers from file }
  TkkColumnHeadState = class(TbcState)
  public
    procedure ProcessPchar(P: pchar; Pos: integer); override;
  end;
  { TkkDateState A concrete state object used while scanning date fields }
  TkkDateState = class(TbcState)
  public
    procedure ProcessPchar(P: pchar; Pos: integer); override;
  end;

  { TkkDescState A concrete state object used while scanning descrition fields }
  TkkDescState = class(TbcState)
  public
    procedure ProcessPchar(P: pchar; Pos: integer); override;
  end;

  { TkkPriceState A concrete state object used while scanning amount fields }
  TkkPriceState = class(TbcState)
  public
    procedure ProcessPchar(P: pchar; Pos: integer); override;
  end;
  { TkkColumnFootState A concrete state object used to extract column footers from file }
  TkkColumnFootState = class(TbcState)
  public
    procedure ProcessPchar(P: pchar; Pos: integer); override;
  end;
  { TkkErrorState A concrete state object used when some error occurred like an invalid kk structure }
  TkkErrorState = class(TbcState)
  public
    procedure ProcessPchar(P: pchar; Pos: integer); override;
  end;
{ registers the states, needed to parse kk-files, with the statemachine and spits out the statenames as well }
procedure RegisterKKStates(aStateMachine: TbcStateMachine;out StateNames: TStringArray);

implementation

procedure RegisterKKStates(aStateMachine: TbcStateMachine;out StateNames: TStringArray);
begin
  SetLength(StateNames,9);
  aStateMachine.RegisterState(TkkDataState.Create(aStateMachine)); // idx 0 "start-state" must be registered first!
  StateNames[0]:= TkkDataState.ClassName;
  aStateMachine.RegisterState(TkkHeadState.Create(aStateMachine)); // idx 1
  StateNames[1]:= TkkHeadState.ClassName;
  aStateMachine.RegisterState(TkkVersState.Create(aStateMachine)); // idx 2
  StateNames[2]:= TkkVersState.ClassName;
  aStateMachine.RegisterState(TkkColumnHeadState.Create(aStateMachine)); // idx 3
  StateNames[3]:= TkkColumnHeadState.ClassName;
  aStateMachine.RegisterState(TkkDateState.Create(aStateMachine)); // idx 4
  StateNames[4]:= TkkDateState.ClassName;
  aStateMachine.RegisterState(TkkDescState.Create(aStateMachine)); // idx 5
  StateNames[5]:= TkkDescState.ClassName;
  aStateMachine.RegisterState(TkkPriceState.Create(aStateMachine)); // idx 6
  StateNames[6]:= TkkPriceState.ClassName;
  aStateMachine.RegisterState(TkkColumnFootState.Create(aStateMachine)); // idx 7
  StateNames[7]:= TkkColumnFootState.ClassName;
  aStateMachine.RegisterState(TkkErrorState.Create(aStateMachine)); // idx 8
  StateNames[8]:= TkkErrorState.ClassName;
end; { RegisterKKStates }

{ TkkDataState }
procedure TkkDataState.ProcessPchar(P: pchar; Pos: integer);
begin
  if ((P^ in ['A'..'Z','a'..'z']) and (fMachine.Tag = 0)) then begin
    AddCharToCurrentField(P^);
    fMachine.Tag:= 1;
    ChangeState(TkkHeadState);
  end;
  if ((P^ = '|') and (fMachine.Tag = 3)) then ChangeState(TkkColumnHeadState);
  if ((P^ in ['0'..'9']) and (fMachine.Tag >= 4)) then begin
    if fMachine.Tag >= 8 then FireDataEvent; { item finished, let the user know, clears fieldlist & currentfield }
    AddCharToCurrentField(P^);
    fMachine.Tag:= fMachine.Tag+1; { ~ 5+ }
    ChangeState(TkkDateState);
  end;
  if ((P^ = '+') and (P[1] = '-') and (fMachine.Tag >= 8)) then begin
    FireDataEvent; { last item finished, let the user know }
    ChangeState(TkkColumnFootState); { pick off the footer as a last chore }
  end;
end;

{ TkkHeadState }
procedure TkkHeadState.ProcessPchar(P: pchar; Pos: integer);
begin
  if P^ <> #10 then AddCharToCurrentField(P^);
  if ((P^ = #10) and (P[1] = #10)) then begin
    AddCurrentFieldToList;
    FireDataEvent; { item finished, let the user know, clears fieldlist & currentfield }
    fMachine.Tag:= fMachine.Tag+1; { ~ 2 }
    ChangeState(TkkVersState);
  end;
end;

{ TkkVersState }
procedure TkkVersState.ProcessPchar(P: pchar; Pos: integer);
begin
  if P^ <> #10 then AddCharToCurrentField(P^);
  if ((P^ = #10) and (P[1] = #10)) then begin
    AddCurrentFieldToList;
    FireDataEvent; { item finished, let the user know, clears fieldlist & currentfield }
    fMachine.Tag:= fMachine.Tag+1; { ~ 3 }
    ChangeState(TkkDataState);
  end;
end;

{ TkkColumnHeadState }
procedure TkkColumnHeadState.ProcessPchar(P: pchar; Pos: integer);
begin
  if P^ <> '|' then AddCharToCurrentField(P^);
  if P^ = '|' then AddCurrentFieldToList;
  if P^ = #10 then begin
    FireDataEvent; { item finished, let the user know, clears fieldlist & currentfield }
    fMachine.Tag:= fMachine.Tag+1; { ~ 4 }
    ChangeState(TkkDataState);
  end;
end;

{ TkkDateState }
procedure TkkDateState.ProcessPchar(P: pchar; Pos: integer);
begin
  if P^ <> '|' then AddCharToCurrentField(P^);
  if P^ = '|' then begin
    AddCurrentFieldToList;
    fMachine.Tag:= fMachine.Tag+1; { ~ 6+ }
    ChangeState(TkkDescState);
  end;
end;

{ TkkDescState }
procedure TkkDescState.ProcessPchar(P: pchar; Pos: integer);
begin
  if P^ <> '|' then AddCharToCurrentField(P^);
  if P^ = '|' then begin
    AddCurrentFieldToList;
    fMachine.Tag:= fMachine.Tag+1; { ~ 7+ }
    ChangeState(TkkPriceState);
  end;
end;

{ TkkPriceState }
procedure TkkPriceState.ProcessPchar(P: pchar; Pos: integer);
begin
  if P^ <> '|' then AddCharToCurrentField(P^);
  if P^ = '|' then begin
    AddCurrentFieldToList;
    fMachine.Tag:= fMachine.Tag+1; { ~ 8+ }
    ChangeState(TkkDataState); { | #10 | -> difference between price and next date, data will eat them }
  end;
end;

{ TkkColumnFootState }
procedure TkkColumnFootState.ProcessPchar(P: pchar; Pos: integer);
begin
  if not (P^ in ['+','-','|',#10,'=']) then AddCharToCurrentField(P^);
  if P^ = '|' then AddCurrentFieldToList;
  if P^ = #0 then FireDataEvent; { file & item finished, let the user know, we're done, exit stage left }
end;

{ TkkErrorState }
procedure TkkErrorState.ProcessPchar(P: pchar; Pos: integer);
const Err = 'Error in line at position %d: ' + #10 + '<%s>';
begin
  raise Exception.Create(Format(Err,[Pos,fMachine.CurrentLine]));
end;

end.

