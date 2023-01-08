(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstPriQ                                                          *)
(* Priority queue test suite                                        *)
(********************************************************************)

program TstPriQ;

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
  TDPriQue;

type
  TMyItem = class
    public
      miString   : string[31];
      miPriority : integer;
  end;

var
  CompareCount : integer;  

function CreateRandomItem : TMyItem;
var
  i : integer;
begin
  Result := TMyItem.Create;
  with Result do begin
    miString[0] := char(Random(10) + 20);
    for i := 1 to length(miString) do
      miString[i] := char(Random(26) + ord('A'));
    miPriority := Random(200);
  end;
end;

function MyCompare(aItem1, aItem2 : pointer) : integer; far;
var
  One : TMyItem absolute aItem1;
  Two : TMyItem absolute aItem2;
begin
  if (One.miPriority < Two.miPriority) then
    Result := -1
  else if (One.miPriority > Two.miPriority) then
    Result := 1
  else
    Result := -CompareText(One.miString, Two.miString);
  inc(CompareCount);
end;

procedure MyDispose(aItem : pointer);                          {!!.01}
begin                                                          {!!.01}
  TMyItem(aItem).Free;                                         {!!.01}
end;                                                           {!!.01}

function MyLessThan(const aItem1, aItem2 : pointer) : boolean; far;
var
  One : TMyItem absolute aItem1;
  Two : TMyItem absolute aItem2;
begin
  Result := (One.miPriority < Two.miPriority);
end;

var
  i     : integer;
  MyPQA : TtdSimplePriQueue1;
  MyPQB : TtdSimplePriQueue2;
  MyPQ  : TtdPriorityQueue;
  MyPQEx : TtdPriorityQueueEx;
  Item  : TMyItem;
  MyItems   : array [1..20] of TMyItem;
  MyHandles : array [1..20] of TtdPQHandle;

  Line : integer;
begin
  Randomize;
  writeln('---Testing priority queue A---');
  MyPQA := TtdSimplePriQueue1.Create(MyCompare);
  writeln('   add 20 items');
  for i := 1 to 20 do
    MyPQA.Enqueue(CreateRandomItem);
  writeln('   remove all items');
  while (MyPQA.Count > 0) do begin
    Item := TMyItem(MyPQA.Dequeue);
    writeln('      ', Item.miPriority:2, ' ', Item.miString);
    Item.Free;                                                 {!!.01}
  end;
  MyPQA.Free;
  readln;


  writeln('---Testing priority queue B---');
  MyPQB := TtdSimplePriQueue2.Create(MyCompare);
  writeln('   add 20 items');
  for i := 1 to 20 do
    MyPQB.Enqueue(CreateRandomItem);
  writeln('   remove all items');
  while (MyPQB.Count > 0) do begin
    Item := TMyItem(MyPQB.Dequeue);
    writeln('      ', Item.miPriority:2, ' ', Item.miString);
    Item.Free;                                                 {!!.01}
  end;
  MyPQB.Free;
  readln;

  writeln('---Testing final priority queue---');
  MyPQ := TtdPriorityQueue.Create(MyCompare, MyDispose);       {!!.01}
  writeln('   add 2000 items');
  for i := 1 to 2000 do
    MyPQ.Enqueue(CreateRandomItem);
  {
  for i := 1 to 20 do begin
    Item := TMyItem(MyPQ.List[i-1]);
    writeln('      ', Item.miPriority:2, ' ', Item.miString);
  end;
  readln;
  }
  writeln('   remove all items');
  Line := 0;
  CompareCount := 0;
  while (MyPQ.Count > 0) do begin
    Item := TMyItem(MyPQ.Dequeue);
    writeln('      ', Item.miPriority:2, ' ', Item.miString);
    Item.Free;                                                 {!!.01}
    inc(Line);
    if (Line = 20) then begin
      readln;
      Line := 0;
    end;
  end;
  MyPQ.Free;
  writeln('Number of compares: ', CompareCount);
  readln;

  writeln('---Testing extended priority queue---');
  MyPQEx := TtdPriorityQueueEx.Create(MyCompare, MyDispose);   {!!.01}
  writeln('   add 20 items');
  for i := 1 to 20 do
    MyPQEx.Enqueue(CreateRandomItem);
  {
  for i := 1 to 20 do begin
    Item := TMyItem(MyPQ.List[i-1]);
    writeln('      ', Item.miPriority:2, ' ', Item.miString);
  end;
  }
  writeln('   remove all items');
  while (MyPQEx.Count > 0) do begin
    Item := TMyItem(MyPQEx.Dequeue);
    writeln('      ', Item.miPriority:2, ' ', Item.miString);
    Item.Free;                                                 {!!.01}
  end;
  MyPQEx.Free;
  readln;


  writeln('---Testing extended priority queue delete and replace---');
  MyPQEx := TtdPriorityQueueEx.Create(MyCompare, MyDispose);   {!!.01}
  writeln('   add 20 items');
  for i := 1 to 20 do begin
    MyItems[i] := CreateRandomItem;
    MyHandles[i] := MyPQEx.Enqueue(MyItems[i]);
  end;
  writeln('   delete every 4th item');
  Item := TMyItem(MyPQEx.Remove(MyHandles[4]));                {!!.01}
  Item.Free;                                                   {!!.01}
  Item := TMyItem(MyPQEx.Remove(MyHandles[8]));                {!!.01}
  Item.Free;                                                   {!!.01}
  Item := TMyItem(MyPQEx.Remove(MyHandles[12]));               {!!.01}
  Item.Free;                                                   {!!.01}
  Item := TMyItem(MyPQEx.Remove(MyHandles[16]));               {!!.01}
  Item.Free;                                                   {!!.01}
  Item := TMyItem(MyPQEx.Remove(MyHandles[20]));               {!!.01}
  Item.Free;                                                   {!!.01}
  writeln('   repriortize every 5th item (note the leaks)');
  if MyItems[5].miPriority < 100 then
    MyItems[5].miPriority := MyItems[5].miPriority * 2
  else
    MyItems[5].miPriority := MyItems[5].miPriority div 2;
  MyPQEx.ChangePriority(MyHandles[5]);
  if MyItems[10].miPriority < 100 then
    MyItems[10].miPriority := MyItems[10].miPriority * 2
  else
    MyItems[10].miPriority := MyItems[10].miPriority div 2;
  MyPQEx.ChangePriority(MyHandles[10]);
  if MyItems[15].miPriority < 100 then
    MyItems[15].miPriority := MyItems[15].miPriority * 2
  else
    MyItems[15].miPriority := MyItems[15].miPriority div 2;
  MyPQEx.ChangePriority(MyHandles[15]);
  writeln('   free queue and items remaining');
  MyPQEx.Free;
  readln;

end.
