(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstState                                                         *)
(* State machine test suite                                         *)
(********************************************************************)

program TstState;

{$I TDDefine.inc}

{$IFDEF Delphi2Plus}
{$apptype console}
{$ENDIF}

uses
  {$IFDEF Delphi1}
  WinCrt,
  {$ENDIF}
  SysUtils,
  Classes,
  TDBasics,
  TDStates;

var
  S : string;
  i : integer;
  SL : TStringList;

begin
  SL := TStringList.Create;
  try
    writeln('Testing extract words routine...');
    S := 'He said, "State Machines?"';
    writeln('[', S, '] is split into...');
    TDExtractWords(S, SL);
    for i := 0 to pred(SL.Count) do
      writeln('[', SL[i], ']');

    S := 'He said, "State Machines?';
    writeln('[', S, '] cannot be split');
    try
      TDExtractWords(S, SL);
    except
      on E: exception do
        writeln('Error: ', E.Message);
    end;

    writeln('Testing extract fields routine...');
    S := 'Julian,Bucknall,,43,"Author, and Columnist"';
    writeln('[', S, '] is split into...');
    TDExtractFields(S, SL);
    for i := 0 to pred(SL.Count) do
      writeln('[', SL[i], ']');

    S := 'Julian,Bucknall,,43, "Author, and Columnist"';
    writeln('[', S, '] is split into...');
    TDExtractFields(S, SL);
    for i := 0 to pred(SL.Count) do
      writeln('[', SL[i], ']');


    S := 'Julian,Bucknall,,43,"Author, and Columnist';
    writeln('[', S, '] cannot be split');
    try
      TDExtractFields(S, SL);
    except
      on E: exception do
        writeln('Error: ', E.Message);
    end;

  finally
    SL.Free;
  end;

  writeln('Testing NFA is-valid-number routine...');
  S := '+1';
  writeln('[', S, '] :', IsValidNumberNFA(S));
  S := '-12';
  writeln('[', S, '] :', IsValidNumberNFA(S));
  S := '12.';
  writeln('[', S, '] :', IsValidNumberNFA(S));
  S := '+12.3';
  writeln('[', S, '] :', IsValidNumberNFA(S));
  S := '-.1234';
  writeln('[', S, '] :', IsValidNumberNFA(S));
  S := '0.0.0';
  writeln('[', S, '] :', IsValidNumberNFA(S));
  S := '0.123A';
  writeln('[', S, '] :', IsValidNumberNFA(S));

  writeln('Testing DFA is-valid-number routine...');
  S := '+1';
  writeln('[', S, '] :', IsValidNumber(S));
  S := '-12';
  writeln('[', S, '] :', IsValidNumber(S));
  S := '12.';
  writeln('[', S, '] :', IsValidNumber(S));
  S := '+12.3';
  writeln('[', S, '] :', IsValidNumber(S));
  S := '-.1234';
  writeln('[', S, '] :', IsValidNumber(S));
  S := '0.0.0';
  writeln('[', S, '] :', IsValidNumber(S));
  S := '0.123A';
  writeln('[', S, '] :', IsValidNumber(S));

  writeln('Done...');
  readln;
end.
