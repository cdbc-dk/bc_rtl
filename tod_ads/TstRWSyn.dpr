(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstRWSyn                                                         *)
(* Readers/writers synchronization class test program               *)
(********************************************************************)

program TstRWSyn;

{$I TDDefine.inc}

{$IFNDEF Delphi2Plus}
Error! This test program is for Win32 Delphi compilers only.
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  Classes,
  TDRWSync;

type
  TWriterThread = class(TThread)
    private
      FId : integer;
    protected
      procedure Execute; override;
    public
      property Id : integer read FId write FId;
  end;

  TReaderThread = class(TThread)
    private
      FId : integer;
    protected
      procedure Execute; override;
    public
      property Id : integer read FId write FId;
  end;

var
  ReadWriteSync : TtdReadWriteSync;
  Log : text;
  LogCS : TRTLCriticalSection;
  IntList : TList;

procedure SafeWriteln(aMsg : string);
begin
  EnterCriticalSection(LogCS);
  writeln(Log, aMsg);
  LeaveCriticalSection(LogCS);
end;

procedure TWriterThread.Execute;
var
  i : integer;
begin
  for i := 1 to 100 do begin
    ReadWriteSync.StartWriting;
    writeln(Log, 'writer active: ', Id);
    write(Id);
    IntList.Insert(Random(IntList.Count), pointer(Random(32767)));
    ReadWriteSync.StopWriting;
  end;
end;

procedure TReaderThread.Execute;
var
  i, j : integer;
  Max : integer;
begin
  for i := 1 to 50 do begin
    ReadWriteSync.StartReading;
    Max := 0;
    for j := 0 to pred(IntList.Count) do
      if (integer(IntList[j]) > Max) then
        Max := integer(IntList[j]);
    ReadWriteSync.StopReading;
  end;
end;

var
  Readers : array [0..6] of TReaderThread;
  Writers : array [0..2] of TWriterThread;
  i : integer;
  Handles : array [0..9] of THandle;

begin
  writeln('Starting test');
  writeln('(Writers show their id in this window when writing)');

  {create the log file}
  Assign(Log, 'C:\Thread.LOG');
  Rewrite(Log);
  InitializeCriticalSection(LogCS);

  {create the list}
  IntList := TList.Create;

  {create the sync object}
  ReadWriteSync := TtdReadWriteSync.Create;

  {set up 7 readers and 3 writers}
  for i := 0 to 6 do begin
    Readers[i] := TReaderThread.Create(true);
    Readers[i].Id := i;
  end;
  for i := 0 to 2 do begin
    Writers[i] := TWriterThread.Create(true);
    Writers[i].Id := i;
  end;

  {save the handles for the wait for multiple objects call}
  for i := 0 to 6 do
    Handles[i] := Readers[i].Handle;
  for i := 0 to 2 do
    Handles[7+i] := Writers[i].Handle;

  {start the whole lot going}
  for i := 0 to 2 do
    Writers[i].Resume;
  for i := 0 to 6 do
    Readers[i].Resume;

  {wait for them all to finish}
  WaitForMultipleObjects(10, @Handles, true, INFINITE);

  {destroy the thread objects (they've all completed)}
  for i := 0 to 6 do
    Readers[i].Free;
  for i := 0 to 2 do
    Writers[i].Free;

  {destroy the list}
  IntList.Free;

  {close the log file}
  DeleteCriticalSection(LogCS);
  Close(Log);
  writeln;
  writeln('Done');
  readln;
end.
