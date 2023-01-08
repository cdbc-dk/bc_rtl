

{******************************************************************************$
$          Unit name : bc_baselist.pas                                         $
$          Copyright : (C)2022 cdbc.dk                                         $
$          Programmer: Benny Christensen                                       $
$          Created   : 10.12.2022 /bc Anscester lists with pointers            $
$          Updated   : 08.12.2022 /bc created TbcThreadQueue                   $
$                      30.11.2022 /bc added "bcCloneImgRec" function           $
$                      01.12.2022 /bc                                          $
$                                                                              $
$                                                                              $
$ **************************************************************************** $
$  Purpose:                                                                    $
$  Implements a single and a doubly linked list                                $
$  for use with multi-threading communication.                                 $
$                                                                              $
$                                                                              $
$******************************************************************************}

unit bc_baselist;
{$mode ObjFPC}{$H+}
{$define UseNodeManager}
{$define debug}
interface
uses
  Classes, SysUtils, bc_guardian, bc_observer_subject;

const
  UnitVersion = '0.11.12.2022';
  PageNodeCount = 30;


type
  { single-list node }
  PsllNode = ^TsllNode;
  TsllNode = packed record
    sllnNext: PsllNode;
    sllnData: pointer;
    sllnIdx: ptruint; { 0..max(QWord) }
  end;
  { double-list node }
  PdllNode = ^TdllNode;
  TdllNode = packed record
    dllnNext: PdllNode;
    dllnPrev: PdllNode;
    dllnData: pointer;
  end;
  TbcCompareFunction = function (anItem1, anItem2: pointer): ptrint;
  { TbcBaseSingleList is a linked list with some added functionality }
  TbcBaseSingleList = class
  private
    fCount: ptrint;
    fCursor: PsllNode;
    fHead: PsllNode;
    fHalf: PsllNode; { halfway through the list }
    fGen: ptruint; { remembers the unique idx }
    function get_Items(Index: ptrint): pointer;
    procedure set_Items(Index: ptrint;aValue: pointer);
  protected
    function IdxGen: ptruint; { provides unique indexes }
    procedure MoveToHalf;
    procedure RefitIndexes; { used after sort }
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(anItem: pointer); { appends data to list @ eol }
    procedure InsertAfter(anItem: pointer); { inserts data right after the cursor }
    function DeleteAfter: pointer; { deletes data right after the cursor }
    function IsBeforeFirst: boolean; { is cursor reset }
    function IsEmpty: boolean; { well duh! }
    function IsLast: boolean; { returns true when cursor is at end of list }
    procedure MoveBeforeFirst; { puts the cursor before the first actual item }
    function First: pointer; { puts the cursor at the first actual item, returns its data }
    function MoveNext: boolean; { moves the cursor forwards to the next item, returns false at EOL }
    function Next: pointer; { moves the cursor to the next item, returns its data or NIL at EOL }
    procedure MoveLast; { puts the cursor @ EOL }
    procedure Clear; { throwing it all away, user is responsible for freeing item memory first }
    function Examine: pointer; { returns data @ cursor }
    procedure Sort(aCompare: TbcCompareFunction); { perform an insertion-sort on the list }
    property Items[Index: ptrint]: pointer read get_Items write set_Items; default; { conveinience }
    property Count: ptrint read fCount;
  end; { *** TbcBaseSingleList *** }
  { TbcThreadSingleList is a threadsafe edition of the TbcBaseSingleList }
  TbcThreadSingleList = class
  private
    fLock: TGuardian;
    fList: TbcBaseSingleList;
    function get_Count: ptrint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(anItem: pointer); { appends data to end of list }
    procedure InsertAfter(anItem: pointer); { inserts data after current cursor-pos }
    function DeleteAfter: pointer; { deletes data after current cursor-pos }
    function IsBeforeFirst: boolean;
    function IsEmpty: boolean;
    function IsLast: boolean;
    procedure MoveBeforeFirst; { use with "while MoveNext do ..."}
    { puts the cursor at the first actual item, returns its data, good candidate for Lock/UnLock }
    function First: pointer; { use: "Itm:= list.First;" -> "while Itm <> nil do begin ... Itm:= list.Next; end;" }
    function MoveNext: boolean;
    { moves the cursor to the next item, returns its data or NIL at EOL, good candidate for Lock/UnLock }
    function Next: pointer; { use: "Itm:= list.First;" -> "while Itm <> nil do begin ... Itm:= list.Next; end;" }
    procedure Clear; { good candidate for Lock/UnLock }
    function Peek: pointer;
    function Lock: TbcBaseSingleList; { preferred for lengthy operations, like traversing the list }
    procedure UnLock;                 { preferred for lengthy operations, or sorting, clearing etc... }
    property Count: ptrint read get_Count;
  end; { TbcThreadSingleList }

implementation
uses LCLIntf, bc_advstring, bc_datetime;
{$i nodemanager.inc}

{ TbcThreadSingleList }
function TbcThreadSingleList.get_Count: ptrint;
begin
  if fLock.BeginWrite then try Result:= fList.Count; finally fLock.EndWrite; end;
end;

constructor TbcThreadSingleList.Create;
begin
  inherited Create;
  fLock:= TGuardian.Create;
  fList:= TbcBaseSingleList.Create;
end;

destructor TbcThreadSingleList.Destroy;
begin
  FreeAndNil(fList);
  FreeAndNil(fLock);
  inherited Destroy;
end;

procedure TbcThreadSingleList.Add(anItem: pointer);
begin
  if fLock.BeginWrite then try fList.Add(anItem); finally fLock.EndWrite; end;
end;

procedure TbcThreadSingleList.InsertAfter(anItem: pointer);
begin
  if fLock.BeginWrite then try fList.InsertAfter(anItem); finally fLock.EndWrite; end;
end;

function TbcThreadSingleList.DeleteAfter: pointer;
begin
  if fLock.BeginWrite then try Result:= fList.DeleteAfter; finally fLock.EndWrite; end;
end;

function TbcThreadSingleList.IsBeforeFirst: boolean;
begin
  if fLock.BeginWrite then try Result:= fList.IsBeforeFirst; finally fLock.EndWrite; end;
end;

function TbcThreadSingleList.IsEmpty: boolean;
begin
  if fLock.BeginWrite then try Result:= fList.IsEmpty; finally fLock.EndWrite; end;
end;

function TbcThreadSingleList.IsLast: boolean;
begin
  if fLock.BeginWrite then try Result:= fList.IsLast; finally fLock.EndWrite; end;
end;

procedure TbcThreadSingleList.MoveBeforeFirst;
begin
  if fLock.BeginWrite then try fList.MoveBeforeFirst; finally fLock.EndWrite; end;
end;

function TbcThreadSingleList.First: pointer;
begin
  if fLock.BeginWrite then try Result:= fList.First; finally fLock.EndWrite; end;
end;

function TbcThreadSingleList.MoveNext: boolean;
begin
  if fLock.BeginWrite then try Result:= fList.MoveNext; finally fLock.EndWrite; end;
end;

function TbcThreadSingleList.Next: pointer;
begin
  if fLock.BeginWrite then try Result:= fList.Next; finally fLock.EndWrite; end;
end;

procedure TbcThreadSingleList.Clear;
begin
  if fLock.BeginWrite then try fList.Clear; finally fLock.EndWrite; end;
end;

function TbcThreadSingleList.Peek: pointer;
begin
  if fLock.BeginWrite then try Result:= fList.Examine; finally fLock.EndWrite; end;
end;

function TbcThreadSingleList.Lock: TbcBaseSingleList;
begin
  fLock.Lock;
  Result:= fList;
end;

procedure TbcThreadSingleList.UnLock;
begin
  fLock.UnLock;
end;

{ TbcBaseSingleList }
function TbcBaseSingleList.get_Items(Index: ptrint): pointer;
begin
  if not bcIsWithinRange(0,Index,fCount,LOWBOUND) then raise Exception.Create('TbcBaseSingleList.get_Items: Error - Index out of bounds!');
  if Index = 0 then begin
    Result:= First;               { goto first one and return data }
    MoveNext;                     { move one step ahead, ready for next read }
    exit;                         { exit stage left }
  end;
  if fCursor^.sllnIdx = Index then begin
    Result:= (fCursor^.sllnData); { return data }
    MoveNext;                     { move one step ahead, ready for next read }
    exit;                         { exit stage left }
  end else if fCursor^.sllnNext^.sllnIdx = Index then begin
    MoveNext;                     { move one step ahead, to the right one }
    Result:= (fCursor^.sllnData); { return data }
    MoveNext;                     { move one step ahead, ready for next read }
    exit;                         { exit stage left }
  end;
  if (Index > (fCount div 2)) then MoveToHalf else MoveBeforeFirst;
  while MoveNext do if fCursor^.sllnIdx = Index then begin
    Result:= fCursor^.sllnData;
    MoveNext;
    break;
  end;
end; { TbcBaseSingleList.get_Items }

procedure TbcBaseSingleList.set_Items(Index: ptrint;aValue: pointer);
begin
exit; //TODO
  if not bcIsWithinRange(0,Index,fCount,LOWBOUND) then raise Exception.Create('TbcBaseSingleList.set_Items: Error - Index out of bounds!');
  if (Index >= (fCount div 2)) then fCursor:= fHalf else MoveBeforeFirst;
  while MoveNext do if fCursor^.sllnIdx = Index then begin
    fCursor^.sllnData:= aValue;
    break;
  end;
end; { TbcBaseSingleList.set_Items }

function TbcBaseSingleList.IdxGen: ptruint;
begin
  Result:= fGen;
  inc(fGen);
end; { TbcBaseSingleList.IdxGen }

procedure TbcBaseSingleList.MoveToHalf;
var
  H: ptrint;
begin
  H:= (fCount div 2);
  if fHalf^.sllnIdx = H then fCursor:= fHalf else begin
    if fCursor^.sllnIdx < H then while MoveNext do if fCursor^.sllnIdx = H then begin
      fHalf:= fCursor;
      break;
    end;
  end;
end; { TbcBaseSingleList.MoveToHalf }

procedure TbcBaseSingleList.RefitIndexes;
begin
  fGen:= 0;
  MoveBeforeFirst; { point to head }
  while MoveNext do begin
    fCursor^.sllnIdx:= IdxGen;
    if fCursor^.sllnIdx = (fCount div 2) then fHalf:= fCursor;
  end;
end; { TbcBaseSingleList.RefitIndexes }

constructor TbcBaseSingleList.Create;
begin
  inherited Create;
  fGen:= 0; { start of index generator }
  {allocate a head node}
  fHead:= snmAllocNode;
  fHead^.sllnNext:= nil;
  fHead^.sllnData:= nil;
  fHead^.sllnIdx:= MaxUIntValue; //bm
  {set the cursor to the head node}
  fCursor:= fHead;
  fHalf:= fHead;
end; { TbcBaseSingleList.Create }

destructor TbcBaseSingleList.Destroy;
begin
  Clear;
  snmFreeNode(fHead);
  inherited Destroy;
end; { TbcBaseSingleList.Destroy }

procedure TbcBaseSingleList.Add(anItem: pointer);
begin
  MoveLast;               { put cursor at eol }
  InsertAfter(anItem);    { add item after last item }
  MoveNext;               { move the cursor to last }
end; { TbcBaseSingleList.Add }

procedure TbcBaseSingleList.InsertAfter(anItem: pointer);
var NewNode: PsllNode;
begin
  {allocate a new node and insert after the cursor}
  NewNode:= snmAllocNode;
  NewNode^.sllnData:= anItem;
  NewNode^.sllnIdx:= IdxGen; //bm       index generator
  NewNode^.sllnNext:= fCursor^.sllnNext;
  fCursor^.sllnNext:= NewNode;
  inc(fCount);
end; { TbcBaseSingleList.InsertAfter }

function TbcBaseSingleList.DeleteAfter: pointer;
var OldNode: PsllNode;
begin
  OldNode:= fCursor^.sllnNext;
  if (OldNode = nil) then raise Exception.Create('TbcBaseSingleList.DeleteAfter: cannot delete - at end of list');
  Result:= OldNode^.sllnData;
  fCursor^.sllnNext:= OldNode^.sllnNext;
  snmFreeNode(OldNode);
  dec(fCount);
end; { TbcBaseSingleList.DeleteAfter }

function TbcBaseSingleList.IsBeforeFirst: boolean;
begin
  Result:= fCursor = fHead;
end; { TbcBaseSingleList.IsBeforeFirst }

function TbcBaseSingleList.IsEmpty: boolean;
begin
  Result:= fCount = 0;
end; { TbcBaseSingleList.IsEmpty }

function TbcBaseSingleList.IsLast: boolean;
begin
  Result:= fCursor^.sllnNext = nil;
end; { TbcBaseSingleList.IsLast }

procedure TbcBaseSingleList.MoveBeforeFirst;
begin
  { set the cursor to the head node }
  fCursor:= fHead;
end; { TbcBaseSingleList.MoveBeforeFirst }

function TbcBaseSingleList.First: pointer;
begin
  if not IsEmpty then begin
    fCursor:= fHead^.sllnNext;  { point cursor at first item with data }
    Result:= fCursor^.sllnData; { and return the data }
  end else Result:= nil; //?!? raise Exception.Create('TbcBaseSingleList.First: Error, List is empty!');
end;

function TbcBaseSingleList.MoveNext: boolean;
begin
  { advance the cursor to its own next pointer }
  if (fCursor^.sllnNext = nil) then Result:= false
  else begin
    fCursor:= fCursor^.sllnNext;
    Result:= true;
  end;
end; { TbcBaseSingleList.MoveNext }

function TbcBaseSingleList.Next: pointer;
begin
  if MoveNext then Result:= Examine
  else Result:= nil;
end; { TbcBaseSingleList.Next }

procedure TbcBaseSingleList.MoveLast;
begin
  { ends at last item, keeping an eye out for halfway point }
  while MoveNext do if fCursor^.sllnIdx = (fCount div 2) then fHalf:= fCursor;
end; { TbcBaseSingleList.MoveLast }

procedure TbcBaseSingleList.Clear;
var Temp: PsllNode;
begin
  Temp:= FHead^.sllnNext;                { start with the actual first item }
  while (Temp <> nil) do begin           { check for end of list }
    fHead^.sllnNext:= Temp^.sllnNext;    { point head.next to second item }
    snmFreeNode(Temp);                   { free what was the first item }
    Temp:= fHead^.sllnNext;              { point temp to the now first item }
  end;
  fCount:= 0;                            { indicate list is empty }
  fGen:= 0;                              { reset our index generator }
  fHalf:= fHead;
end; { TbcBaseSingleList.Clear }

function TbcBaseSingleList.Examine: pointer;
begin
  { return the data part of the cursor }
  Result:= fCursor^.sllnData;
end; { TbcBaseSingleList.Examine }

procedure TbcBaseSingleList.Sort(aCompare: TbcCompareFunction);
var
  Walker,WalkerParent: PsllNode;
  Temp,TempParent: PsllNode;
begin
  { if there are zero (or one) items the list is already sorted }
  if (Count <= 1) then exit;
  { perform an insertion sort from the second item onwards }
  WalkerParent:= fHead^.sllnNext;
  Walker:= WalkerParent^.sllnNext;
  while (Walker <> nil) do begin
    { find where the walker item should be in the sorted list to its
      left - we walk the sorted sublist making a note of the parent as
      we go so that we can insert properly. Note that the loop below
      will terminate in the worst case by the walker node itself - we
      won't run off the end of the list }
    TempParent:= fHead;
    Temp:= TempParent^.sllnNext;
    while (aCompare(Temp^.sllnData, Walker^.sllnData) < 0) do begin
      TempParent:= Temp;
      Temp:= TempParent^.sllnNext;
    end;
    { did we find the walker node? If so, it's in the right place so
      move the walker's parent on by one link }
    if (Temp = Walker) then WalkerParent:= Walker
    { otherwise, move the walker node into the correct place in the
      sorted sublist; leave the walker's parent where it is }
    else begin
      { disconnect the walker node }
      WalkerParent^.sllnNext:= Walker^.sllnNext;
      { connect the walker node in the correct place }
      Walker^.sllnNext:= Temp;
      TempParent^.sllnNext:= Walker;
    end;
    { set the walker node }
    Walker:= WalkerParent^.sllnNext;
  end;
  RefitIndexes;
end; { TbcBaseSingleList.Sort }



end.

