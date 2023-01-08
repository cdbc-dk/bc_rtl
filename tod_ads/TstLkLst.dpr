(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstLkLst                                                         *)
(* Linked list, stack and queue test suite                          *)
(********************************************************************)

program TstLkLst;

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
  TDBasics,
  TDTList,
  TDLnkLst,
  TDStkQue;

function CompareValues(aItem1, aItem2 : pointer) : integer; far;
var
  I1 : longint absolute aItem1;
  I2 : longint absolute aItem2;
begin
  Result := I1 - I2;
end;

const
  {$IFDEF Delphi1}
  SpeedCount = 10000;
  {$ELSE}
  SpeedCount = 15000;
  {$ENDIF}

{$IFDEF Delphi1}
type
  DWORD = longint;
{$ENDIF}

var
  SList : TtdSingleLinkList;
  DList : TtdDoubleLinkList;
  Stack : TtdStack;
  Queue : TtdQueue;
  i     : longint;
  Start : DWORD;
  Value : longint;
  StartValue : longint;

begin
  writeln('Start...');
  try
    writeln('SINGLE LINKED LIST TEST');
    SList := TtdSingleLinkList.Create(nil);
    try
      {add 15 items}
      writeln('adding 15 items');
      SList.MoveBeforeFirst;
      for i := 1 to 15 do begin
        Value := Random(100);
        SList.InsertAtCursor(pointer(Value));
        SList.MoveNext;
      end;
      SList.MoveBeforeFirst;
      SList.MoveNext;
      while not SList.IsAfterLast do begin
        write(integer(SList.Examine):3);
        SList.MoveNext;
      end;
      writeln;
      readln;
      {sort them}
      writeln('sorting items');
      SList.InsertionSort(CompareValues);
      for i := 0 to pred(SList.Count) do
        write(integer(SList[i]):3);
      writeln;
      readln;
      {delete every other item}
      writeln('deleting every other item');
      SList.MoveBeforeFirst;
      SList.MoveNext;
      while not SList.IsAfterLast do begin
        SList.DeleteAtCursor;
        SList.MoveNext;
      end;
      SList.MoveBeforeFirst;
      SList.MoveNext;
      while not SList.IsAfterLast do begin
        write(integer(SList.Examine):3);
        SList.MoveNext;
      end;
      writeln;
      readln;
      {delete every other item}
      writeln('deleting every other item again');
      SList.MoveBeforeFirst;
      SList.MoveNext;
      while not SList.IsAfterLast do begin
        SList.DeleteAtCursor;
        SList.MoveNext;
      end;
      SList.MoveBeforeFirst;
      SList.MoveNext;
      while not SList.IsAfterLast do begin
        write(integer(SList.Examine):3);
        SList.MoveNext;
      end;
      writeln;
      readln;

      {add 15 items}
      SList.Clear;
      writeln('adding 15 items');
      for i := 1 to 15 do begin
        Value := Random(100);
        SList.Add(pointer(Value));
      end;
      for i := 0 to pred(SList.Count) do
        write(integer(SList[i]):3);
      writeln;
      readln;
      {sort them}
      writeln('sorting items');
      SList.InsertionSort(CompareValues);
      for i := 0 to pred(SList.Count) do
        write(integer(SList[i]):3);
      writeln;
      readln;
      {delete every other item}
      writeln('deleting every other item');
      for i := pred(SList.Count) downto 0 do
        if Odd(i) then
          SList.Delete(i);
      for i := 0 to pred(SList.Count) do
        write(integer(SList[i]):3);
      writeln;
      readln;
      {delete every other item}
      writeln('removing every other item');
      for i := pred(SList.Count) downto 0 do
        if Odd(i) then
          SList.Remove(SList[i]);
      for i := 0 to pred(SList.Count) do
        write(integer(SList[i]):3);
      writeln;
      readln;

      {add 15 items}
      SList.Clear;
      writeln('adding 15 items, SORTED');
      StartValue := 0; {fool the compiler's warning generator} {!!.01}
      for i := 1 to 15 do begin
        Value := Random(100);
        if i = 1 then
          StartValue := Value;
        SList.InsertSorted(pointer(Value), TDCompareLongint);
      end;
      for i := 0 to pred(SList.Count) do
        write(integer(SList[i]):3);
      writeln;
      readln;
      {locate the first one we added}
      writeln('locate the first one we added');
      if (SList.Locate(pointer(StartValue), TDCompareLongint) = -1) or
         (longint(SList.Examine) <> StartValue) then
        writeln('Error: Sorted Locate failed')
      else
        writeln('Success');
      readln;

      {add 15,000 items}
      SList.Clear;
      writeln('adding 15,000 items');
      for i := 1 to 15000 do begin
        Value := Random(32000);
        SList.Add(pointer(Value));
      end;
      writeln('sorting them');
      SList.Sort(CompareValues);
      writeln('checking sort order');
      SList.MoveBeforeFirst;
      Value := 0;
      SList.MoveNext;
      while not SList.IsAfterLast do begin
        if (Value > integer(SList.Examine)) then begin
          writeln('Sorting error');
          readln;
        end;
        Value := integer(SList.Examine);
        SList.MoveNext;
      end;
      writeln('done');
      readln;

    finally
      SList.Free;
    end;

    writeln('DOUBLE LINKED LIST TEST');
    DList := TtdDoubleLinkList.Create(nil);
    try
      {add 15 items}
      writeln('adding 15 items');
      DList.MoveBeforeFirst;
      for i := 1 to 15 do begin
        Value := Random(100);
        DList.InsertAtCursor(pointer(Value));
        DList.MoveNext;
      end;
      writeln('--fwd');
      DList.MoveBeforeFirst;
      DList.MoveNext;
      while not DList.IsAfterLast do begin
        write(integer(DList.Examine):3);
        DList.MoveNext;
      end;
      writeln;
      writeln('--bkwd');
      DList.MoveAfterLast;
      DList.MovePrior;
      while not DList.IsBeforeFirst do begin
        write(integer(DList.Examine):3);
        DList.MovePrior;
      end;
      writeln;
      readln;
      {sort them}
      writeln('sorting items');
      DList.InsertionSort(CompareValues);
      writeln('--fwd');
      DList.MoveBeforeFirst;
      DList.MoveNext;
      while not DList.IsAfterLast do begin
        write(integer(DList.Examine):3);
        DList.MoveNext;
      end;
      writeln;
      writeln('--bkwd');
      DList.MoveAfterLast;
      DList.MovePrior;
      while not DList.IsBeforeFirst do begin
        write(integer(DList.Examine):3);
        DList.MovePrior;
      end;
      writeln;
      readln;
      {delete every other item}
      writeln('deleting every other item');
      DList.MoveBeforeFirst;
      DList.MoveNext;
      while not DList.IsAfterLast do begin
        DList.DeleteAtCursor;
        DList.MoveNext;
      end;
      writeln('--fwd');
      DList.MoveBeforeFirst;
      DList.MoveNext;
      while not DList.IsAfterLast do begin
        write(integer(DList.Examine):3);
        DList.MoveNext;
      end;
      writeln;
      writeln('--bkwd');
      DList.MoveAfterLast;
      DList.MovePrior;
      while not DList.IsBeforeFirst do begin
        write(integer(DList.Examine):3);
        DList.MovePrior;
      end;
      writeln;
      readln;
      {delete every other item}
      writeln('deleting every other item again');
      DList.MoveBeforeFirst;
      DList.MoveNext;
      while not DList.IsAfterLast do begin
        DList.DeleteAtCursor;
        DList.MoveNext;
      end;
      writeln('--fwd');
      DList.MoveBeforeFirst;
      DList.MoveNext;
      while not DList.IsAfterLast do begin
        write(integer(DList.Examine):3);
        DList.MoveNext;
      end;
      writeln;
      writeln('--bkwd');
      DList.MoveAfterLast;
      DList.MovePrior;
      while not DList.IsBeforeFirst do begin
        write(integer(DList.Examine):3);
        DList.MovePrior;
      end;
      writeln;
      readln;

      {add 15 items}
      DList.Clear;
      writeln('adding 15 items');
      for i := 1 to 15 do begin
        Value := Random(100);
        DList.Add(pointer(Value));
      end;
      for i := 0 to pred(DList.Count) do
        write(integer(DList[i]):3);
      writeln;
      readln;
      {sort them}
      writeln('sorting items');
      DList.InsertionSort(CompareValues);
      for i := 0 to pred(DList.Count) do
        write(integer(DList[i]):3);
      writeln;
      readln;
      {delete every other item}
      writeln('deleting every other item');
      for i := pred(DList.Count) downto 0 do
        if Odd(i) then
          DList.Delete(i);
      for i := 0 to pred(DList.Count) do
        write(integer(DList[i]):3);
      writeln;
      readln;
      {delete every other item}
      writeln('removing every other item');
      for i := pred(DList.Count) downto 0 do
        if Odd(i) then
          DList.Remove(DList[i]);
      for i := 0 to pred(DList.Count) do
        write(integer(DList[i]):3);
      writeln;
      readln;

      {add 15 items}
      DList.Clear;
      writeln('adding 15 items, SORTED');
      for i := 1 to 15 do begin
        Value := Random(100);
        if i = 1 then
          StartValue := Value;
        DList.InsertSorted(pointer(Value), TDCompareLongint);
      end;
      for i := 0 to pred(DList.Count) do
        write(integer(DList[i]):3);
      writeln;
      readln;
      {locate the first one we added}
      writeln('locate the first one we added');
      if (DList.Locate(pointer(StartValue), TDCompareLongint) = -1) or
         (longint(DList.Examine) <> StartValue) then
        writeln('Error: Sorted Locate failed')
      else
        writeln('Success');
      readln;

      {add 15,000 items}
      DList.Clear;
      writeln('adding 15,000 items');
      for i := 1 to 15000 do begin
        Value := Random(32000);
        DList.Add(pointer(Value));
      end;
      writeln('sorting them');
      DList.Sort(CompareValues);
      writeln('checking sort order forwards');
      DList.MoveBeforeFirst;
      Value := 0;
      DList.MoveNext;
      while not DList.IsAfterLast do begin
        if (Value > integer(DList.Examine)) then begin
          writeln('Sorting error');
          readln;
        end;
        Value := integer(DList.Examine);
        DList.MoveNext;
      end;
      writeln('checking sort order backwards');
      DList.MoveAfterLast;
      Value := 32000;
      DList.MovePrior;
      while not DList.IsBeforeFirst do begin
        if (Value < integer(DList.Examine)) then begin
          writeln('Sorting error');
          readln;
        end;
        Value := integer(DList.Examine);
        DList.MovePrior;
      end;
      writeln('done');
      readln;

    finally
      DList.Free;
    end;

    writeln('STACK TEST');
    Stack := TtdStack.Create(nil);
    try
      writeln('pushing ', SpeedCount, ' items');
      Start := GetTickCount;
      for i := 1 to SpeedCount do
        Stack.Push(pointer(i));
      writeln('popping all items');
      i := SpeedCount;
      while Stack.Count <> 0 do begin
        if (i <> longint(Stack.Pop)) then
          raise Exception.Create('bad item in stack');
        dec(i);
      end;
      writeln('done (', GetTickCount-Start, ')');
    finally
      Stack.Free;
    end;

    writeln('QUEUE TEST');
    Queue := TtdQueue.Create(nil);
    try
      writeln('enqueuing ', SpeedCount, ' items');
      Start := GetTickCount;
      for i := 1 to SpeedCount do
        Queue.Enqueue(pointer(i));
      writeln('dequeuing all items');
      i := 1;
      while Queue.Count <> 0 do begin
        if (i <> longint(Queue.Dequeue)) then
          raise Exception.Create('bad item in queue');
        inc(i);
      end;
      writeln('done (', GetTickCount-Start, ')');
    finally
      Queue.Free;
    end;


  except
    on E : Exception do
      writeln(E.Message);
  end;
  writeln('...done');
  readln;
end.
