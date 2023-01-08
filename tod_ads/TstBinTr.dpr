(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstBinTr                                                         *)
(* Binary trees test suite                                          *)
(********************************************************************)

program TstBinTr;

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
  TDBinTre,
  PSProcs;

const
  ColumnWidth = 25;
  StripHeight = 25;
  MarginLeft  = 20;

type
  PMyRec = ^TMyRec;
  TMyRec = packed record
    Name : string[31];
    Age  : integer;
  end;

function CompareMyRec(aItem1, aItem2 : pointer) : integer; far;
var
  MyRec1 : PMyRec absolute aItem1;
  MyRec2 : PMyRec absolute aItem2;
begin
  if (MyRec1^.Name < MyRec2^.Name) then
    Result := -1
  else if (MyRec1^.Name > MyRec2^.Name) then
    Result := 1
  else
    Result := MyRec1^.Age - MyRec2^.Age;
end;

procedure RandomMyRec(var MyRec : TMyRec);
var
  i : integer;
begin
  MyRec.Name[0] := #3 {char(Random(3) + 1)};
  for i := 1 to length(MyRec.Name) do
    MyRec.Name[i] := char(Random(26) + ord('a'));
  MyRec.Age := Random(20) + 30;
end;

procedure DisposeMyRec(aItem : pointer); far;
begin
  Dispose(PMyRec(aItem));
end;

procedure PrintItem(aData      : pointer;
                    aExtraData : pointer;
                var aStopVisits: boolean); far;
begin
  writeln(PMyRec(aData)^.Name);
end;

function CheckItem(aNode      : PtdBinTreeNode;
                   aExtraData : pointer) : boolean;
begin
  if (aNode^.btChild[ctLeft] <> nil) then
    if aNode^.btChild[ctLeft]^.btParent <> aNode then
      raise Exception.Create('node error');
  if (aNode^.btChild[ctRight] <> nil) then
    if aNode^.btChild[ctRight]^.btParent <> aNode then
      raise Exception.Create('node error');
  Result := true;
end;


procedure DrawNode(aNode   : PtdBinTreeNode;
                   aStrip  : integer;
                   aColumn : integer;
                   aParentStrip  : integer;
                   aParentColumn : integer;
                   aExtraData    : pointer); far;
var
  X, Y : integer;
  ParentX, ParentY : integer;
  HasParent : boolean;
begin
  {calculate the X, Y position of the bottom left corner of the box}
  X := MarginLeft + aColumn * ColumnWidth;
  Y := 720 - (aStrip * StripHeight);
  {do the same for the parent box}
  if (aParentStrip = -1) then begin
    HasParent := false;
    ParentX := -1;
    ParentY := -1;
  end
  else begin
    HasParent := true;
    ParentX := MarginLeft + aParentColumn * ColumnWidth;
    ParentY := 720 - (aParentStrip * StripHeight);
  end;
  {if the node is red, draw a filled box}
  if (aNode^.btColor = rbRed) then
    AAPSDrawRectFill(TStringList(aExtraData),
                     X, Y, ColumnWidth, StripHeight - 10, 0.9)
  {otherwise draw the box for the node}
  else
    AAPSDrawRect(TStringList(aExtraData),
                 X, Y, ColumnWidth, StripHeight - 10);
  {draw the text for the node}
  AAPSDrawText(TStringList(aExtraData),
               PMyRec(aNode^.btData)^.Name,
               X+3, Y+5, 10);
  {draw a line from our parent to ourselves}
  if HasParent then begin
    AAPSDrawLine(TStringList(aExtraData),
                 ParentX + (ColumnWidth div 2),
                 ParentY,
                 X + (ColumnWidth div 2),
                 Y + (StripHeight - 10));
  end;
end;


procedure DrawTestTree(aBinTree : TtdBinarySearchTree;
                       aID      : integer);
var
  SList : TStringList;
begin
  SList := TStringList.Create;
  try
    AAPSOutputProlog(SList);
    DrawBinaryTree(aBinTree, DrawNode, pointer(SList));
    AAPSOutputEpilog(SList);
    SList.SaveToFile(Format('d:\BinTre%.2d.EPS', [aID]));
  finally
    SList.Free;
  end;
end;

const
  NodeCount = 15;
var
{ BinTree : TtdSplayTree;}
{ BinTree : TtdRedBlackTree;}
  BinTree : TtdBinarySearchTree;
  MyRec   : PMyRec;
  i       : integer;
  MyRecQuery : TMyRec;
begin
  writeln('Testing binary tree...');
  try
{   BinTree := TtdSplayTree.Create(CompareMyRec, DisposeMyRec);}
{   BinTree := TtdRedBlackTree.Create(CompareMyRec, DisposeMyRec);}
    BinTree := TtdBinarySearchTree.Create(CompareMyRec, DisposeMyRec);
    try
      writeln('inserting');
      RandSeed := $12345678;
      for i := 1 to NodeCount do begin
        New(MyRec);
        RandomMyRec(MyRec^);
        writeln(MyRec^.Name);
        BinTree.Insert(MyRec);
        DrawTestTree(BinTree, i);
      end;
      readln;
      writeln('--pre-order');
      BinTree.Traverse(tmPreOrder, PrintItem, nil, true);
      readln;
      writeln('--in-order');
      BinTree.Traverse(tmInOrder, PrintItem, nil, true);
      readln;
      writeln('--post-order');
      BinTree.Traverse(tmPostOrder, PrintItem, nil, true);
      readln;
      writeln('--level-order');
      BinTree.Traverse(tmLevelOrder, PrintItem, nil, true);
      readln;

      RandSeed := $12345678;
      for i := 1 to NodeCount do begin
        RandomMyRec(MyRecQuery);
        MyRec := BinTree.Find(@MyRecQuery);
        if (MyRec = nil) or (MyRec^.Name <> MyRecQuery.Name) then
          writeln('error: cannot find ', MyRecQuery.Name);
      end;

      writeln('deleting');
      RandSeed := $12345678;
      for i := 1 to NodeCount do begin
        RandomMyRec(MyRecQuery);
        if Odd(i) then begin
          BinTree.Delete(@MyRecQuery);
          DrawTestTree(BinTree, i+20);
        end;
      end;
      RandSeed := $12345678;
      for i := 1 to NodeCount do begin
        RandomMyRec(MyRecQuery);
        if not Odd(i) then begin
          BinTree.Delete(@MyRecQuery);
          (*BinTree.Traverse(tmInOrder, CheckItem, nil, true);*)
          DrawTestTree(BinTree, i+40);
        end;
      end;

      (*
      {generate degenerate trees}
      BinTree.Clear;
      New(MyRec);
      MyRec^.Name := '  a ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  b ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  c ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  d ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  e ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  f ';
      BinTree.Insert(MyRec);
      DrawTestTree(BinTree, 90);

      BinTree.Clear;
      New(MyRec);
      MyRec^.Name := '  a ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  f ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  b ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  e ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  c ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  d ';
      BinTree.Insert(MyRec);
      DrawTestTree(BinTree, 91);

      {generate bushy tree}
      BinTree.Clear;
      New(MyRec);
      MyRec^.Name := '  d ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  b ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  f ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  a ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  c ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  e ';
      BinTree.Insert(MyRec);
      New(MyRec);
      MyRec^.Name := '  g ';
      BinTree.Insert(MyRec);
      DrawTestTree(BinTree, 92);
      *)

    finally
      BinTree.Free;
    end;
  except
    on E: Exception do
      writeln(E.Message);
  end;
  writeln('Done');
  readln;
end.
