(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstHshEx                                                         *)
(* Extendible hashing test suite                                    *)
(********************************************************************)

program TstHshEx;

{$I TDDefine.inc}

{$IFNDEF Delphi1}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF Delphi1}
  WinCrt,
  {$ENDIF}
  Classes,
  TDBasics,
  TDRecFil,
  TDHshBse,
  TDHshExt;

const
  {$IFDEF Kylix1Plus}
  DirName = '../hashtbl.dir';
  BucketName = '../hashtbl.bkt';
  FileName = '../hashtbl.rec';
  {$ELSE}
  DirName = 'c:\hashtbl.dir';
  BucketName = 'c:\hashtbl.bkt';
  FileName = 'c:\hashtbl.rec';
  {$ENDIF}


type
  TMyRec = packed record
    Name : string[63];
    Other: string[35];
  end;

function CompareKey(var aRecord; const aKey : string) : boolean; far;
begin
  Result := TMyRec(aRecord).Name = aKey;
end;

function RandomName(aLen : integer) : string;
var
  i : integer;
begin
  SetLength(Result, aLen);
  for i := 1 to aLen do
    Result[i] := char(Random(26) + ord('a'));
end;

const
  RecCount = 10000;

var
  Directory : TFileStream;
  Buckets   : TtdRecordFile;
  Records   : TtdRecordFile;
  HashTable : TtdHashTableExtendible;
  MyRec     : TMyRec;
  MyRec2    : TMyRec;
  i         : integer;
begin
  writeln('Testing extendible hash table');
  Directory := nil;
  Buckets := nil;
  Records := nil;
  HashTable := nil;
  try
    write('Creating objects... ');
    Directory := TFileStream.Create(DirName, fmCreate);
    Buckets := TtdRecordFile.Create(
                  BucketName, fmCreate, tdcBucketSize);
    Records := TtdRecordFile.Create(
                  FileName, fmCreate, sizeof(TMyRec));
    HashTable := TtdHashTableExtendible.Create(
                    TDPJWHashEx, CompareKey,
                    Directory, Buckets, Records);
    writeln('(done)');

    write('Adding ', RecCount, ' records.');
    RandSeed := $1234;
    for i := 1 to RecCount do begin
      FillChar(MyRec, sizeof(MyRec), 0);
      MyRec.Name := RandomName(Random(10) + 50);
      HashTable.Insert(MyRec.Name, MyRec);
      if (i mod (RecCount div 10) = 0) then
        write('.');
    end;
    writeln(' (done)');

    write('Checking records to be there.');
    RandSeed := $1234;
    for i := 1 to RecCount do begin
      FillChar(MyRec, sizeof(MyRec), 0);
      MyRec.Name := RandomName(Random(10) + 50);
      if not HashTable.Find(MyRec.Name, MyRec2) then begin
        writeln('***');
        readln;
      end;
      if (i mod (RecCount div 10) = 0) then
        write('.');
    end;
    writeln(' (done)');
  finally
    write('Freeing objects... ');
    HashTable.Free;
    Buckets.Free;
    Records.Free;
    Directory.Free;
    writeln('(done)');
  end;

  Directory := nil;
  Buckets := nil;
  Records := nil;
  HashTable := nil;
  try
    write('Reopening objects... ');
    Directory := TFileStream.Create(
                    DirName, fmOpenReadWrite or fmShareDenyWrite);
    Buckets := TtdRecordFile.Create(
                  BucketName, fmOpenReadWrite or fmShareDenyWrite,
                  tdcBucketSize);
    Records := TtdRecordFile.Create(
                  FileName, fmOpenReadWrite or fmShareDenyWrite,
                  sizeof(TMyRec));
    HashTable := TtdHashTableExtendible.Create(
                    TDPJWHashEx, CompareKey,
                    Directory, Buckets, Records);
    writeln('(done)');

    write('Checking records to be there.');
    RandSeed := $1234;
    for i := 1 to RecCount do begin
      FillChar(MyRec, sizeof(MyRec), 0);
      MyRec.Name := RandomName(Random(10) + 50);
      if not HashTable.Find(MyRec.Name, MyRec2) then begin
        writeln('***');
        readln;
      end;
      if (i mod (RecCount div 10) = 0) then
        write('.');
    end;
    writeln(' (done)');
  finally
    write('Freeing objects... ');
    HashTable.Free;
    Buckets.Free;
    Records.Free;
    Directory.Free;
    writeln('(done)');
  end;
  writeln('Press enter to finish...');
  readln;
end.
