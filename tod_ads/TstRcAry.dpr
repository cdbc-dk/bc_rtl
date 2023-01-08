(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstRcAry                                                         *)
(* Record list/array test suite                                     *)
(********************************************************************)

program TstRcAry;

{$I TDDefine.inc}

{$IFNDEF Delphi1}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF Delphi1}
  WinCrt,
  {$ENDIF}
  TDRecLst;

type
  PMyRec = ^TMyRec;
  TMyRec = packed record
    Value : integer;
    Name  : string[5];
  end;

procedure PressEnter;
begin
  write('Press Enter to continue...');
  readln;
end;

function CompareMyRec(aItem1, aItem2 : pointer) : integer; far;
var
  MyRec1 : PMyRec absolute aItem1;
  MyRec2 : PMyRec absolute aItem2;
begin
  if MyRec1^.Value < MyRec2^.Value then
    Result := -1
  else if MyRec1^.Value = MyRec2^.Value then
    Result := 0
  else
    Result := 1;
end;

procedure InitMyRec(i : integer; var MyRec : TMyRec);
const
  Names : array [1..10] of string[5] =
    ('one', 'two', 'three', 'four', 'five',
     'six', 'seven', 'eight', 'nine', 'ten');
begin
  MyRec.Value := i;
  MyRec.Name := Names[i];
end;

procedure ListRA(RA : TtdRecordList);
var
  i : integer;
begin
  writeln('MaxCount: ', RA.MaxCount);
  writeln('Capacity: ', RA.Capacity);
  writeln('Count:    ', RA.Count);
  for i := 0 to pred(RA.Count) do begin
    with PMyRec(RA.Items[i])^ do
      writeln(Value:5, ' ', Name);
  end;
  PressEnter;
end;

var
  RA : TtdRecordList;
  i  : integer;
  MyRec : TMyRec;

begin
  try
  writeln('Start test');
  RA := TtdRecordList.Create(sizeof(TMyRec));
  try
    RA.Name := 'TestArray';
    {add a few elements}
    for i := 1 to 5 do begin
      InitMyRec(i, MyRec);
      RA.Add(@MyRec);
    end;
    writeln('should be 1,2,3,4,5');
    ListRA(RA);
    {insert a few more}
    for i := 6 to 10 do begin
      InitMyRec(i, MyRec);
      RA.Insert((i-6) * 2, @MyRec);
    end;
    writeln('should be 6,1,7,2,8,3,9.4,10,5');
    ListRA(RA);
    {check the First and Last methods}
    writeln('should be 6,5');
    with PMyRec(RA.First)^ do
      writeln(Value:5, ' ', Name);
    with PMyRec(RA.Last)^ do
      writeln(Value:5, ' ', Name);
    PressEnter;
    {move a few elements around}
    for i := 0 to 9 do begin
      if (i mod 3) = 0 then
        RA.Move(i, (i + 5) mod 10);
    end;
    writeln('should be 1,4,7,2,5,3,6,9.10,8');
    ListRA(RA);
    {exchange a few elements}
    for i := 0 to 9 do begin
      if (i mod 4) = 0 then
        RA.Exchange(i, (i + 5) mod 10);
    end;
    writeln('should be 3,4,7,10,8,1,6,9.2,5');
    ListRA(RA);
    {delete a few elements}
    for i := 9 downto 0 do begin
      if (i mod 3) = 0 then
        RA.Delete(i);
    end;
    writeln('should be 4,7,8,1,9.2');
    ListRA(RA);
    {remove a few elements}
    for i := 1 to 10 do begin
      if (i mod 2) = 0 then begin
        InitMyRec(i, MyRec);
        RA.Remove(@MyRec, CompareMyRec);
      end;
    end;
    writeln('should be 7,1,9');
    ListRA(RA);

    RA.Clear;
    {add a few elements, SORTED}
    for i := 10 downto 1 do begin
      if Odd(i) then begin
        InitMyRec(i, MyRec);
        RA.InsertSorted(@MyRec, CompareMyRec);
      end;
    end;
    for i := 1 to 10 do begin
      if not Odd(i) then begin
        InitMyRec(i, MyRec);
        RA.InsertSorted(@MyRec, CompareMyRec);
      end;
    end;
    writeln('should be 1,2,3,4,5,6,7,8,9,10');
    ListRA(RA);
    {find 6}
    MyRec.Value := 6;
    writeln('MyRec.Value 6 was found at element ',
            RA.IndexOf(@MyRec, CompareMyRec));
  finally
    RA.Free;
  end;
  except
    on E:Exception do
      writeln(E.Message);
  end;
  writeln('Done test');
  PressEnter;
end.
