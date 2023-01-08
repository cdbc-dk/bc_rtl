(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999                *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstSkip                                                          *)
(* Skip list test suite                                             *)
(********************************************************************)

program TstSkip;

{$I TDDefine.inc}

{$IFNDEF Delphi1}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF Delphi1}
  WinTypes,
  WinProcs,
  WinCrt,
  {$ENDIF}
  {$IFDEF Delphi2Plus}
  Windows,
  {$ENDIF}
  {$IFDEF Kylix1Plus}
  Types,
  Libc,
  {$ENDIF}
  TDSkpLst;

const
  ItemCount = 1000;

function CompareItems(aItem1, aItem2 : pointer) : integer; far;
var
  I1 : longint absolute aItem1;
  I2 : longint absolute aItem2;
begin
  Result := I1 - I2;
end;

var
  SkipList : TtdSkipList;
  i        : integer;
  Prev     : integer;
begin
  Randomize;
  try
    writeln('SKIP LIST TEST');
    SkipList := TtdSkipList.Create(CompareItems, nil);
    try
      {add itemcount items}
      writeln('Adding ', ItemCount, ' items');
      for i := 1 to ItemCount do
        SkipList.Add(pointer(i));
      {check order of the skip list}
      writeln('Checking order of items in skip list');
      SkipList.MoveBeforeFirst;
      SkipList.MoveNext;
      Prev := 0;
      while not SkipList.IsAfterLast do begin
        i := integer(SkipList.Examine);
        if (i-Prev <> 1) then
          writeln('error');
        Prev := i;
        SkipList.MoveNext;
      end;
      SkipList.MoveAfterLast;
      SkipList.MovePrior;
      Prev := integer(SkipList.Examine);
      SkipList.MovePrior;
      while not SkipList.IsBeforeFirst do begin
        i := integer(SkipList.Examine);
        if (Prev-i <> 1) then
          writeln('error');
        Prev := i;
        SkipList.MovePrior;
      end;
      {search for each item}
      writeln('Searching for all items...');
      for i := 1 to ItemCount do
        if not SkipList.Search(pointer(i)) then
          writeln('***Cannot find ', i);
      writeln('Done searching');
      writeln('MaxLevel ', SkipList.MaxLevel);
      readln;
      SkipList.Print;
      readln;
      {delete bunch of items}
      writeln('Deleting some items...');
      SkipList.MoveBeforeFirst;
      SkipList.MoveNext;
      for i := 1 to ItemCount do begin
        if (i mod 13) = 0 then
          SkipList.Delete
        else
          SkipList.MoveNext;
      end;
      writeln('Done deleting');
      SkipList.Print;
    finally
      SkipList.Free;
    end;
  except
    on E : Exception do
      writeln(E.Message);
  end;
  readln;
end.
