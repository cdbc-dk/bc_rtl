(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstHshLP                                                         *)
(* Hash table (linear probe) test suite                             *)
(********************************************************************)

program TstHshLP;

{$I TDDefine.inc}

{$IFNDEF Delphi1}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  Classes,
  {$IFDEF Delphi1}
  WinCrt,
  {$ENDIF}
  TDBasics,
  TDHshBse,
  TDHshLnP;


function RandomName(aLen : integer) : string;
var
  i : integer;
begin
  SetLength(Result, aLen);
  Result[1] := char(Random(26) + ord('A'));
  for i := 2 to aLen do
    Result[i] := char(Random(26) + ord('a'));
end;

var
  i, j : integer;
  HashTable : TtdHashTableLinear;
  StrList : TStringList;
  DummyItem : pointer;
begin
  writeln('Testing hash table with linear probing');
  try
    StrList := TStringList.Create;
    RandSeed := $12345678;
    for i := 0 to 1999 do
      StrList.Add(RandomName(Random(19) + 1));

    HashTable := TtdHashTableLinear.Create(3000, TDPJWHash, nil);
    try
      i := 0;
      j := 0;
      while (i<2000) or (j<2000) do begin
        if (i<2000) and (Random(10) <= 5) then begin
          if not HashTable.Find(StrList[i], DummyItem) then
            HashTable.Insert(StrList[i], nil);
          inc(i);
        end
        else begin
          if HashTable.Find(StrList[j], DummyItem) then
            HashTable.Delete(StrList[j]);
          inc(j);
        end;
      end;

    finally
      HashTable.Free;
      StrList.Free;
    end;
  except
    on E: Exception do
      writeln(E.Message);
  end;
  writeln('Done');
  readln;
end.
