(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstRegex                                                         *)
(* Regular expression test suite                                    *)
(********************************************************************)

program TstRegex;

{$I TDDefine.inc}

{$IFNDEF Delphi1}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF Delphi1}
  WinCrt,
  {$ENDIF}
  SysUtils,
  TDRegex;

var
  RC : TtdRegexEngine;
  i : integer;
  S : string;
  F : text;
  ec : TtdRegexError;
begin
  S := '^ *(function|procedure) +[^.]*$';

{$IFDEF OtherSelections}
  S := '.*(true|false);[ ]*$';
  S := '^ *(function|procedure) +[^.]*$';
  S := 'if.*then( *| begin *)$';
  S := 'while.*do( *| begin *)$';
  S := '^[^ A-Z]*$';
  S := '^( *|.*:= *)rcSetState';
  S := '\{(.+)+\}';  {DANGER DON"T USE--EXPONENTIAL)}
  S := '([a-zA-Z]+\^)|([A-Za-z]+\.[A-Za-z])';
  S := '\{.+\}';
{$ENDIF}
  RC := TtdRegexEngine.Create(S);
  RC.Name := 'Test regex engine';
  if not RC.Parse(i, ec) then begin
    writeln('error found at ', i);
    writeln(S);
    for i := 1 to length(S) do
      write(i mod 10);
    writeln;
  end
  else begin
    RC.IgnoreCase := true;
    System.Assign(F, 'TDRegex.pas');
    System.Reset(F);
    while not eof(F) do begin
      readln(F, S);
      i := RC.MatchString(S);
      if (i <> 0) then
        writeln(i:2, ' ', S);
    end;
    System.Close(F);
  end;
  RC.Free;
  readln;
end.
