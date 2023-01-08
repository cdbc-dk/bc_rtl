(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstRecFl                                                         *)
(* Record file test suite                                           *)
(********************************************************************)

program TstRecFl;

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
  TDRecFil;

{$IFDEF Kylix1Plus}
const
  Test1Name = '../test1.dat';
  Test2Name = '../test2.dat';
  Test3Name = '../test3.dat';
{$ELSE}
const
  Test1Name = 'c:\test1.dat';
  Test2Name = 'c:\test2.dat';
  Test3Name = 'c:\test3.dat';
{$ENDIF}



function RandomName(aLen : integer) : string;
var
  i : integer;
begin
  {$IFDEF Windows}
  Result[0] := char(aLen);
  {$ELSE}
  SetLength(Result, aLen);
  {$ENDIF}
  Result[1] := char(Random(26) + ord('A'));
  for i := 2 to aLen do
    Result[i] := char(Random(26) + ord('a'));
end;

procedure TestBasics;
type
  TMyRecord = packed record
    Name : string[31];
    Age  : integer;
    Sex  : char;
  end;
  procedure InitMyRec(var MyRec : TMyRecord);
  begin
    with MyRec do begin
      Name := RandomName(Random(12) + 20);
      Age := Random(10) + 21;
      if Random(10) < 5 then
        Sex := 'M'
      else
        Sex := 'F';
    end;
  end;
var
  MyRec : TMyRecord;
  DF : file of TMyRecord;
  i  : integer;
begin
  System.Assign(DF, 'C:\MyData.DAT');
  System.Rewrite(DF);
  try
    for i := 0 to 9 do begin
      InitMyRec(MyRec);
      System.Write(DF, MyRec);
    end;
    System.Seek(DF, 0);
    for i := 0 to 9 do begin
      System.Read(DF, MyRec);
      writeln(MyRec.Name:40, MyRec.Age:4, MyRec.Sex:3);
    end;
  finally
    System.Close(DF);
  end;
end;


var
  i : integer;
  S : string;
  DataFile : TtdRecordFile;
  IsDeleted : boolean;
  StrZ : array [0..255] of char;


begin
  writeln('Testing record file class');
  try
    writeln('creating new file');
    DataFile := TtdRecordFile.Create(Test1Name, fmCreate, 60);
    try
      writeln('adding 100 random records');
      for i := 0 to 99 do begin
        S := RandomName(59);
        DataFile.Add(S[1]);
      end;
      writeln('reading the just added records');
      for i := 0 to pred(DataFile.Capacity) do begin
        DataFile.Read(i, StrZ, IsDeleted);
        if IsDeleted then begin
          writeln('deleted error at ', i);
          readln;
        end
        else
          {writeln(StrZ)};
      end;
      writeln('writing a new record for odd numbers, deleting even ones');
      for i := 0 to pred(DataFile.Capacity) do begin
        if Odd(i) then begin
          S := RandomName(59);
          DataFile.Write(i, S[1]);
        end
        else
          DataFile.Delete(i);
      end;
    finally
      DataFile.Free;
    end;

    writeln('creating new file');
    DataFile := TtdRecordFile.Create(Test2Name, fmCreate, 60);
    try
      writeln('adding 100 random records');
      for i := 0 to 99 do begin
        S := RandomName(59);
        DataFile.Add(S[1]);
      end;
      writeln('clearing the file');
      DataFile.Clear;
    finally
      DataFile.Free;
    end;

    writeln('creating new file');
    DataFile := TtdRecordFile.Create(Test3Name, fmCreate, 60);
    try
      writeln('setting its capacity to 100');
      DataFile.Capacity := 100;
      DataFile.Flush;
      writeln('adding 50 random records');
      for i := 0 to 49 do begin
        S := RandomName(59);
        DataFile.Add(S[1]);
      end;
      DataFile.Flush;
    finally
      DataFile.Free;
    end;

  except
    on E: Exception do
      writeln(E.Message);
  end;
  writeln('Done');
  readln;
end.
