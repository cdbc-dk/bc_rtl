{ this unit contains the typedeclarations for states used in TbcStateMachine,
  one registers them in order to parse stuff, this one parses csv-files.
  E.g:
  prs: TbcStateMachine;
  prs:= TbcStateMachine.Create;
  prs.RegisterState(TCSVStartFieldState.Create(prs)); // idx 0 "start" must be registered first!
  prs.RegisterState(TCSVScanFieldState.Create(prs)); // idx 1 etc...
  for I:= 0 to sl.count-1 do prs.Execute(sl[i],fl); // fl is a fieldlist ~ TSTringList, sl too
  prs.Free;
  P.s: the implementation is here too ;-)
  The idea is, you create a unit with the new states you need and registers them with the statemachine
}
unit csv_states;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, bc_statemachine;
const
  unitVersion = '02.31.12.2022';
type
  { TCSVStartFieldState A concrete state object used when starting a new field }
  TCSVStartFieldState = class(TbcState)
  public
    procedure ProcessChar(Ch: AnsiChar;Pos: integer); override;
    procedure ProcessPchar(P: pchar; Pos: integer); override; { you can choose not to implement it }
  end;

  { TCSVScanFieldState A concrete state object used while scanning a field }
  TCSVScanFieldState = class(TbcState)
  public
    procedure ProcessChar(Ch: AnsiChar;Pos: integer); override;
  end;

  { TCSVScanQuotedState A concrete state object used while scanning double quoted fields }
  TCSVScanQuotedState = class(TbcState)
  public
    procedure ProcessChar(Ch: AnsiChar;{%H-}Pos: integer); override;
  end;

  { TCSVEndQuotedState A concrete state object used when we found the ending double quote }
  TCSVEndQuotedState = class(TbcState)
  public
    procedure ProcessChar(Ch: AnsiChar;Pos: integer); override;
  end;

  { TCSVGotErrorState A concrete state object used when some error occurred like an invalid CSV structure }
  TCSVGotErrorState = class(TbcState)
  public
    procedure ProcessChar({%H-}Ch: AnsiChar;Pos: integer); override;
  end;

implementation

{ TCSVGotErrorState }
procedure TCSVGotErrorState.ProcessChar(Ch: AnsiChar; Pos: integer);
const Err = 'Error in line at position %d: ' + #10 + '<%s>';
begin
  raise Exception.Create(Format(Err,[Pos,fMachine.CurrentLine]));
end;

{ TCSVEndQuotedState }
procedure TCSVEndQuotedState.ProcessChar(Ch: AnsiChar; Pos: integer);
begin
  if ((Ch = ',') or (Pos = fMachine.LineLen)) then begin
    AddCurrentFieldToList;
    ChangeState(TCSVStartFieldState);
  end else ChangeState(TCSVGotErrorState);
end;

{ TCSVScanQuotedState }
procedure TCSVScanQuotedState.ProcessChar(Ch: AnsiChar; Pos: integer);
begin
  if (Ch = '"') then begin
    if (Pos = fMachine.LineLen) then begin
      AddCurrentFieldToList;
      ChangeState(TCSVStartFieldState);
    end else ChangeState(TCSVEndQuotedState);
  end else AddCharToCurrentField(Ch);
end;

{ TCSVScanFieldState }
procedure TCSVScanFieldState.ProcessChar(Ch: AnsiChar; Pos: integer);
begin
  if ((Ch = ',') or (Pos = fMachine.LineLen)) then begin
    if (Pos = fMachine.LineLen) then AddCharToCurrentField(Ch);
    AddCurrentFieldToList;
    ChangeState(TCSVStartFieldState);
  end else AddCharToCurrentField(Ch);
end;

{ TCSVStartFieldState }
procedure TCSVStartFieldState.ProcessChar(Ch: AnsiChar; Pos: integer);
begin
  if (Ch = '"') then ChangeState(TCSVScanQuotedState)
  else if ((Ch = ',') or (Pos = fMachine.LineLen)) then AddCurrentFieldToList
  else begin
    AddCharToCurrentField(Ch);
    ChangeState(TCSVScanFieldState);
  end;
end;

procedure TCSVStartFieldState.ProcessPchar(P: pchar; Pos: integer);
begin
  RaiseErrorNotImplemented(Self,'ProcessPchar');
(*
  if (P^ = '"') then ChangeState(TCSVScanQuotedState)
  else if ((P^ = ',') or (P[1] = #0)) then AddCurrentFieldToList { pchars are null-terminated }
  else begin
    AddCharToCurrentField(P^);
    ChangeState(TCSVScanFieldState);
  end;
*)
end;

end.

