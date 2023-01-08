(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstObjLs                                                         *)
(* Object list test suite                                           *)
(********************************************************************)

program TstObjLs;

{$I TDDefine.inc}

{$IFNDEF Delphi1}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF Delphi1}
  WinCrt,
  {$ENDIF}
  TDObjLst;

type
  TMyObject = class
    private
      FValue : integer;
      FName  : string;
    protected
    public
      property Value : integer read FValue write FValue;
      property Name : string read FName write FName;
  end;

procedure PressEnter;
begin
  write('Press Enter to continue...');
  readln;
end;

procedure InitMyObj(i : integer; MyObj : TMyObject);
const
  Names : array [1..10] of string[5] =
    ('one', 'two', 'three', 'four', 'five',
     'six', 'seven', 'eight', 'nine', 'ten');
begin
  MyObj.Value := i;
  MyObj.Name := Names[i];
end;

procedure ListObjList(OL : TtdObjectList);
var
  i : integer;
begin
  writeln('Capacity: ', OL.Capacity);
  writeln('Count:    ', OL.Count);
  for i := 0 to pred(OL.Count) do begin
    with OL.Items[i] as TMyObject do
      writeln(Value:5, ' ', Name);
  end;
  PressEnter;
end;

var
  OL : TtdObjectList;
  i  : integer;
  MyObj : TMyObject;
  Obj   : TObject;
begin
  try
  writeln('Start test');
  OL := TtdObjectList.Create(TMyObject, true);
  try
    OL.Name := 'TestObjectList';
    {add a few elements}
    for i := 1 to 5 do begin
      MyObj := TMyObject.Create;
      InitMyObj(i, MyObj);
      OL.Add(MyObj);
    end;
    writeln('should be 1,2,3,4,5');
    ListObjList(OL);
    {insert a few more}
    for i := 6 to 10 do begin
      MyObj := TMyObject.Create;
      InitMyObj(i, MyObj);
      OL.Insert((i-6) * 2, MyObj);
    end;
    writeln('should be 6,1,7,2,8,3,9.4,10,5');
    ListObjList(OL);
    {check the First and Last methods}
    writeln('should be 6,5');
    with OL.First as TMyObject do
      writeln(Value:5, ' ', Name);
    with OL.Last as TMyObject do
      writeln(Value:5, ' ', Name);
    PressEnter;
    {move a few elements around}
    for i := 0 to 9 do begin
      if (i mod 3) = 0 then
        OL.Move(i, (i + 5) mod 10);
    end;
    writeln('should be 1,4,7,2,5,3,6,9.10,8');
    ListObjList(OL);
    {exchange a few elements}
    for i := 0 to 9 do begin
      if (i mod 4) = 0 then
        OL.Exchange(i, (i + 5) mod 10);
    end;
    writeln('should be 3,4,7,10,8,1,6,9.2,5');
    ListObjList(OL);
    {delete a few elements}
    for i := 9 downto 0 do begin
      if (i mod 3) = 0 then
        OL.Delete(i);
    end;
    writeln('should be 4,7,8,1,9.2');
    ListObjList(OL);
    {remove a few elements}
    for i := pred(OL.Count) downto 0 do begin
      if (i mod 2) = 0 then begin
        OL.Remove(OL[i]);
      end;
    end;
    writeln('should be 7,1,2');
    ListObjList(OL);
    {test the type safety}
    Obj := TObject.Create;
    try
      OL.Add(Obj);
    except
      on E:Exception do begin
        writeln('Trapped the following typesafety exception...');
        writeln(E.Message);
      end;
    end;
    Obj.Free;
  finally
    OL.Free;
  end;
  except
    on E:Exception do
      writeln(E.Message);
  end;
  writeln('Done test');
  PressEnter;
end.
