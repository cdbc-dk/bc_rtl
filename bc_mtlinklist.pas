{*******************************************************************************
*                                                                              *
*  Thread safe implementation of a Stack, a Queue,                             *
*  a single-linked list and a double-linked list                               *
*  Created:    05.05.1998 /bc Initial version                                  *
*  Updated:    03.07.2014 /bc Added thread-safety                              *
*  Refactored: 25.03.2020 /bc Code cleanup.                                    *
*              25.03.2020 /bc added guardian to tbcstack both global and fed   *
*              10.05.2022 /bc added owned guardian to tbcqueue                 *
*              28.11.2022 /bc relocated exception raising and added check for  *
*                             empty queue before lock, in tbcqueue.de_queue :o)*
*                                                                              *
*                                                                              *
*                                                                              *
*******************************************************************************}
unit bc_mtlinklist;
interface
uses
  Classes, SysUtils,
  syncobjs,
  bc_guardian;

{$IFOPT D+}
{$DEFINE InDebugMode}
{$ENDIF}

{$DEFINE UseNodeManager}

const
	UnitVersion = '4.28.11.2022';
  PageNodeCount = 30;

type
  TbcCompareFunction = function (anItem1, anItem2: pointer): ptrint;

type
  PsllNode = ^TsllNode;
  TsllNode = packed record
    sllnNext: PsllNode;
    sllnData: pointer;
  end;
  { *** TbcSingleList *** }
  TbcSingleList = class
  private
    fCount: ptrint;
    fCursor: PsllNode;
    fHead: PsllNode;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure InsertAfter(aItem : pointer);
    function DeleteAfter : pointer;
    function IsBeforeFirst : boolean;
    function IsLast : boolean;
    procedure MoveBeforeFirst;
    function MoveNext : boolean;
    procedure Clear;
    function Examine : pointer;
    procedure Sort(aCompare : TbcCompareFunction);
    property Count : ptrint read FCount;
  end; { *** TbcSingleList *** }

type
  PdllNode = ^TdllNode;
  TdllNode = packed record
    dllnNext: PdllNode;
    dllnPrev: PdllNode;
    dllnData: pointer;
  end;

  { *** TbcDoubleList *** }
  TbcDoubleList = class
  private
    fCount: ptrint;
    fCursor: PdllNode;
    fHead: PdllNode;
    fTail: PdllNode;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure InsertBefore(anItem: pointer);
    procedure InsertAfter(anItem: pointer);
    function Delete: pointer;
    function IsAfterLast: boolean;
    function IsBeforeFirst: boolean;
    procedure MoveAfterLast;
    procedure MoveBeforeFirst;
    function MoveNext: boolean;
    function MovePrevious: boolean;
    procedure Clear;
    function Examine: pointer;
    procedure Sort(aCompare: TbcCompareFunction);
    property Count: ptrint read FCount;
  end; { *** TbcDoubleList *** }

type
  { *** TbcStack *** }
  TbcStack = class
  private
    FCount: ptrint;
    FHead: PsllNode;
    function get_Version: string;
  protected
    fOwnLock: boolean;                                    {+ new state var}
    fLock: TGuardian;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create; overload;                         {+ overload}
    constructor Create(const aGuard: TGuardian); overload; {+ overload create with a CS fed in}
    destructor Destroy; override;
    procedure Clear;
    function Examine: pointer;
    function IsEmpty: boolean;
    function _Pop: pointer;
    procedure _Push(anItem: pointer);
    property Count: ptrint read FCount;
    property Version: string read get_Version;
  end; { *** TbcStack *** }

type
  { TbcQueue }
  TbcQueue = class
  private
    fCount: ptrint;
    fHead: PsllNode;
    fTail: PsllNode;
    function get_Version: string;
  protected
    fOnDataInQueue: TNotifyEvent;
    fLock: TGuardian;
    procedure Lock;
    procedure Unlock;
    procedure DoOnDataInQueue; { triggers the notify event, with Self as sender param }
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function De_Queue: pointer;
    procedure En_Queue(anItem: pointer);
    function Examine: pointer;
    function IsEmpty: boolean;
    property Count: ptrint read fCount;
    property OnDataInQueue: TNotifyEvent read fOnDataInQueue write fOnDataInQueue;
    property Version: string read get_Version;
  end;

type
  TbcObjectQueue = class(TbcQueue)
    procedure Put(anObject: TObject); overload;
    function Get: TObject; overload;
  end;

type
  { key / value record }
  PKeyValueRec = ^TKeyValueRec;
  TKeyValueRec = packed record
    Key: ptrint;
    Value: string[64];
  end;

  { TbcKeyValueList }
  TbcKeyValueList = class(TbcSingleList)
  protected
    fLock: TGuardian;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(aKey: ptrint;const aValue: string): PKeyValueRec;
    function FindKey(const aValue: string): ptrint;
    function FindValue(aKey: ptrint): string;
    procedure Delete(aKey: ptrint);
    procedure ClearRecords;
  end;

implementation

{===SingleNodeManager================================================}
type
  PsnmPage = ^TsnmPage;
  TsnmPage = packed record
    snmpNext  : PsnmPage;
    snmpNodes : array [0..pred(PageNodeCount)] of TsllNode;
  end;
{--------}
var
  snmFreeList : PsllNode;
  snmPageList : PsnmPage;
{--------}
procedure snmFreeNode(aNode : PsllNode);
begin
  {$IFDEF UseNodeManager}
  {add the node to the top of the free list}
  aNode^.sllnNext := snmFreeList;
  snmFreeList := aNode;
  {$ELSE}
  Dispose(aNode);
  {$ENDIF}
end;
{--------}
procedure snmAllocPage;
var
  NewPage : PsnmPage;
  i       : ptrint;
begin
  {get a new page}
  New(NewPage);
  {add it to the current list of pages}
  NewPage^.snmpNext := snmPageList;
  snmPageList := NewPage;
  {add all the nodes on the page to the free list}
  for i := 0 to pred(PageNodeCount) do
    snmFreeNode(@NewPage^.snmpNodes[i]);
end;
{--------}
function snmAllocNode : PsllNode;
begin
  {$IFDEF UseNodeManager}
  {if the free list is empty, allocate a new page of nodes}
  if (snmFreeList = nil) then
    snmAllocPage;
  {return the first node on the free list}
  Result := snmFreeList;
  snmFreeList := Result^.sllnNext;
  {$ELSE}
  New(Result);
  {$ENDIF}
  {$IFDEF InDebugMode}
  Result^.sllnNext := nil;
  Result^.sllnData := nil;
  {$ENDIF}
end;
{====================================================================}


{===DoubleNodeManager================================================}
type
  PdnmPage = ^TdnmPage;
  TdnmPage = packed record
    dnmpNext  : PdnmPage;
    dnmpNodes : array [0..pred(PageNodeCount)] of TdllNode;
  end;
{--------}
var
  dnmFreeList : PdllNode;
  dnmPageList : PdnmPage;
{--------}
procedure dnmFreeNode(aNode : PdllNode);
begin
  {$IFDEF UseNodeManager}
  {add the node to the top of the free list}
  aNode^.dllnNext := dnmFreeList;
  dnmFreeList := aNode;
  {$ELSE}
  Dispose(aNode);
  {$ENDIF}
end;
{--------}
procedure dnmAllocPage;
var
  NewPage : PdnmPage;
  i       : ptrint;
begin
  {get a new page}
  New(NewPage);
  {add it to the current list of pages}
  NewPage^.dnmpNext := dnmPageList;
  dnmPageList := NewPage;
  {add all the nodes on the page to the free list}
  for i := 0 to pred(PageNodeCount) do
    dnmFreeNode(@NewPage^.dnmpNodes[i]);
end;
{--------}
function dnmAllocNode : PdllNode;
begin
  {$IFDEF UseNodeManager}
  {if the free list is empty, allocate a new page of nodes}
  if (dnmFreeList = nil) then
    dnmAllocPage;
  {return the first node on the free list}
  Result := dnmFreeList;
  dnmFreeList := Result^.dllnNext;
  {$ELSE}
  New(Result);
  {$ENDIF}
  {$IFDEF InDebugMode}
  Result^.dllnNext := nil;
  Result^.dllnData := nil;
  {$ENDIF}
end;
{====================================================================}

{===TbcKeyValueList====================================================}
constructor TbcKeyValueList.Create;
begin
  inherited Create;
  fLock:= bc_guardian.TGuardian.Create; { setup our own guardian sentinel }
  MoveBeforeFirst; { reset cursor to start of list }
end;

destructor TbcKeyValueList.Destroy;
begin
  ClearRecords;
  FreeAndNil(fLock); { teardown our sentinel }
  inherited Destroy;
end;

function TbcKeyValueList.Add(aKey: ptrint; const aValue: string): PKeyValueRec;
begin
  fLock.Lock;
  try
    Result:= getmem(sizeof(TKeyValueRec));
    Result^.Key:= aKey;
    Result^.Value:= aValue;
    InsertAfter(Result);
  finally fLock.Unlock; end;
end;

function TbcKeyValueList.FindKey(const aValue: string): ptrint;
begin
  fLock.Lock;
  try
    Result:= -1;                                                     { not found }
    MoveBeforeFirst;                                    { start at the beginning }
    while MoveNext do begin                           { will stop at end of list }
      if PKeyValueRec(Examine)^.Value = aValue then begin
        Result:= PKeyValueRec(Examine)^.Key;              { found it, return key }
        break;                                                      { we're done }
      end;
    end;
  finally fLock.Unlock; end;
end;

function TbcKeyValueList.FindValue(aKey: ptrint): string;
begin
  fLock.Lock;
  try
  Result:= '';
  MoveBeforeFirst;
  while MoveNext do  begin
    if PKeyValueRec(Examine)^.Key = aKey then begin
      Result:= PKeyValueRec(Examine)^.Value;
      break;
    end;
  end;
  finally fLock.Unlock; end;
end;

procedure TbcKeyValueList.Delete(aKey: ptrint);
begin
  // maybe todo  walker & parent & deleteafter...
end;

procedure TbcKeyValueList.ClearRecords;
var
  Temp: PsllNode;
begin
  fLock.Lock;
  try
    Temp:= fHead^.sllnNext;                         { set temp = first in list }
    while (Temp <> nil) do begin
      fHead^.sllnNext:= Temp^.sllnNext;         { point head.next to temp.next }
      Freemem(Temp^.sllnData,sizeof(TKeyValueRec));      { dispose of our data }
      snmFreeNode(Temp);                          { dispose of our entire node }
      Temp:= FHead^.sllnNext;             { set temp anew to the first in list }
    end;
    FCount:= 0;                                            { list is now empty }
  finally fLock.Unlock; end;
end;

{====================================================================}


{===TbcSingleList====================================================}
constructor TbcSingleList.Create;
begin
  inherited Create;
  {allocate a head node}
  FHead := snmAllocNode;
  FHead^.sllnNext := nil;
  FHead^.sllnData := nil;
  {set the cursor to the head node}
  FCursor := FHead;
end;
{--------}
destructor TbcSingleList.Destroy;
begin
  Clear;
  snmFreeNode(FHead);
  inherited Destroy;
end;
{--------}
procedure TbcSingleList.Clear;
var
  Temp : PsllNode;
begin
  Temp := FHead^.sllnNext;
  while (Temp <> nil) do begin
    FHead^.sllnNext := Temp^.sllnNext;
    snmFreeNode(Temp);
    Temp := FHead^.sllnNext;
  end;
  FCount := 0;
end;
{--------}
function TbcSingleList.DeleteAfter : pointer;
var
  OldNode : PsllNode;
begin
  OldNode := FCursor^.sllnNext;
  if (OldNode = nil) then
    raise Exception.Create('TaaSingleList.DeleteAfter: cannot delete - at end of list');
  Result := OldNode^.sllnData;
  FCursor^.sllnNext := OldNode^.sllnNext;
  snmFreeNode(OldNode);
  dec(FCount);
end;
{--------}
function TbcSingleList.Examine : pointer;
begin
  {return the data part of the cursor}
  Result := FCursor^.sllnData;
end;
{--------}
procedure TbcSingleList.InsertAfter(aItem : pointer);
var
  NewNode : PsllNode;
begin
  {allocate a new node and insert after the cursor}
  NewNode := snmAllocNode;
  NewNode^.sllnData := aItem;
  NewNode^.sllnNext := FCursor^.sllnNext;
  FCursor^.sllnNext := NewNode;
  inc(FCount);
end;
{--------}
function TbcSingleList.IsBeforeFirst : boolean;
begin
  Result := FCursor = FHead;
end;
{--------}
function TbcSingleList.IsLast : boolean;
begin
  Result := FCursor^.sllnNext = nil;
end;
{--------}
procedure TbcSingleList.MoveBeforeFirst;
begin
  {set the cursor to the head node}
  fCursor := fHead;
end;
{--------}
function TbcSingleList.MoveNext : boolean;
begin
  {advance the cursor to its own next pointer}
  if (fCursor^.sllnNext = nil) then
    Result:= false
  else begin
    fCursor:= fCursor^.sllnNext;
    Result:= true;
  end;
end;
{--------}
procedure TbcSingleList.Sort(aCompare : TbcCompareFunction);
var
  Walker : PsllNode;
  Temp   : PsllNode;
  WalkerParent : PsllNode;
  TempParent   : PsllNode;
begin
  {if there are zero (or one) items the list is already sorted}
  if (Count <= 1) then
    Exit;
  {perform an insertion sort from the second item onwards}
  WalkerParent := FHead^.sllnNext;
  Walker := WalkerParent^.sllnNext;
  while (Walker <> nil) do begin
    {find where the walker item should be in the sorted list to its
     left - we walk the sorted sublist making a note of the parent as
     we go so that we can insert properly. Note that the loop below
     will terminate in the worst case by the walker node itself - we
     won't run off the end of the list}
    TempParent := FHead;
    Temp := TempParent^.sllnNext;
    while (aCompare(Temp^.sllnData, Walker^.sllnData) < 0) do begin
      TempParent := Temp;
      Temp := TempParent^.sllnNext;
    end;
    {did we find the walker node? If so, it's in the right place so
     move the walker's parent on by one link}
    if (Temp = Walker) then
      WalkerParent := Walker
    {otherwise, move the walker node into the correct place in the
     sorted sublist; leave the walker's parent where it is}
    else begin
      {disconnect the walker node}
      WalkerParent^.sllnNext := Walker^.sllnNext;
      {connect the walker node in the correct place}
      Walker^.sllnNext := Temp;
      TempParent^.sllnNext := Walker;
    end;
    {set the walker node}
    Walker := WalkerParent^.sllnNext;
  end;
end;
{====================================================================}


{===TbcDoubleList========================================================}
constructor TbcDoubleList.Create;
begin
  inherited Create;
  {allocate a head and a tail node}
  FHead:= dnmAllocNode;
  FTail:= dnmAllocNode;
  FHead^.dllnNext:= FTail;
  FHead^.dllnPrev:= nil;
  FHead^.dllnData:= nil;
  FTail^.dllnNext:= nil;
  FTail^.dllnPrev:= FHead;
  FTail^.dllnData:= nil;
  {set the cursor to the head node}
  FCursor:= FHead;
end;
{--------}
destructor TbcDoubleList.Destroy;
begin
  Clear;
  dnmFreeNode(FHead);
  dnmFreeNode(FTail);
  inherited Destroy;
end;
{--------}
procedure TbcDoubleList.Clear;
var Temp: PdllNode;
begin
  Temp:= FHead^.dllnNext;
  while (Temp <> FTail) do begin
    FHead^.dllnNext:= Temp^.dllnNext;
    dnmFreeNode(Temp);
    Temp:= FHead^.dllnNext;
  end;
  FCount:= 0;
end;
{--------}
function TbcDoubleList.Delete: pointer;
var OldNode: PdllNode;
begin
  OldNode:= FCursor;
  if (OldNode = FHead) or (OldNode = FTail) then
    raise Exception.Create('TaaDoubleList.Delete: cannot delete - at start/end of list');
  Result:= OldNode^.dllnData;
  OldNode^.dllnPrev^.dllnNext:= OldNode^.dllnNext;
  OldNode^.dllnNext^.dllnPrev:= OldNode^.dllnPrev;
  FCursor:= OldNode^.dllnNext;
  dnmFreeNode(OldNode);
  dec(FCount);
end;
{--------}
function TbcDoubleList.Examine: pointer;
begin
  {return the data part of the cursor}
  Result:= FCursor^.dllnData;
end;
{--------}
procedure TbcDoubleList.InsertAfter(anItem: pointer);
var NewNode: PdllNode;
begin
  if (FCursor = FTail) then
    FCursor:= FCursor^.dllnPrev;
  {allocate a new node and insert after the cursor}
  NewNode:= dnmAllocNode;
  NewNode^.dllnData:= anItem;
  NewNode^.dllnNext:= FCursor^.dllnNext;
  NewNode^.dllnPrev:= FCursor;
  FCursor^.dllnNext:= NewNode;
  NewNode^.dllnNext^.dllnPrev:= NewNode;
  inc(FCount);
end;
{--------}
procedure TbcDoubleList.InsertBefore(anItem: pointer);
var NewNode: PdllNode;
begin
  if (FCursor = FHead) then
    FCursor:= FCursor^.dllnNext;
  {allocate a new node and insert before the cursor}
  NewNode:= dnmAllocNode;
  NewNode^.dllnData:= anItem;
  NewNode^.dllnNext:= FCursor;
  NewNode^.dllnPrev:= FCursor^.dllnPrev;
  NewNode^.dllnPrev^.dllnNext:= NewNode;
  FCursor^.dllnPrev:= NewNode;
  inc(FCount);
end;
{--------}
function TbcDoubleList.IsAfterLast : boolean;
begin
  Result:= FCursor = FTail;
end;
{--------}
function TbcDoubleList.IsBeforeFirst : boolean;
begin
  Result:= FCursor = FHead;
end;
{--------}
procedure TbcDoubleList.MoveAfterLast;
begin
  {set the cursor to the tail node}
  FCursor:= FTail;
end;
{--------}
procedure TbcDoubleList.MoveBeforeFirst;
begin
  {set the cursor to the head node}
  FCursor:= FHead;
end;
{--------}
function TbcDoubleList.MoveNext : boolean;
begin
  {advance the cursor to its own next pointer}
  if (FCursor = FTail) then
    Result:= false
  else begin
    FCursor:= FCursor^.dllnNext;
    Result:= true;
  end;
end;
{--------}
function TbcDoubleList.MovePrevious: boolean;
begin
  {retard the cursor to its own previous pointer}
  if (FCursor = FHead) then
    Result:= false
  else begin
    FCursor:= FCursor^.dllnPrev;
    Result:= true;
  end;
end;
{--------}
procedure TbcDoubleList.Sort(aCompare: TbcCompareFunction);
var
  Walker: PdllNode;
  Temp: PdllNode;
  WalkerParent: PdllNode;
  TempParent: PdllNode;
begin
  {if there are zero (or one) items the list is already sorted}
  if (Count <= 1) then
    Exit;
  {perform an insertion sort from the second item onwards}
  WalkerParent:= FHead^.dllnNext;
  Walker:= WalkerParent^.dllnNext;
  while (Walker <> FTail) do begin
    {find where the walker item should be in the sorted list to its
     left - we walk the sorted sublist making a note of the parent as
     we go so that we can insert properly. Note that the loop below
     will terminate in the worst case by the walker node itself - we
     won't run off the end of the list}
    TempParent:= FHead;
    Temp:= TempParent^.dllnNext;
    while (aCompare(Temp^.dllnData, Walker^.dllnData) < 0) do begin
      TempParent:= Temp;
      Temp:= TempParent^.dllnNext;
    end;
    {did we find the walker node? If so, move the walker's parent on
     by one link}
    if (Temp = Walker) then begin
      WalkerParent:= Walker;
    end
    {otherwise, move the walker node into the correct place in the
     sorted sublist}
    else begin
      {disconnect the walker node}
      WalkerParent^.dllnNext:= Walker^.dllnNext;
      {connect the walker node in the correct place}
      Walker^.dllnNext:= Temp;
      TempParent^.dllnNext:= Walker;
    end;
    {set the walker node}
    Walker:= WalkerParent^.dllnNext;
  end;
  {now patch up all of the previous links}
  WalkerParent:= FHead;
  Walker:= WalkerParent^.dllnNext;
  while (WalkerParent <> FTail) do begin
    Walker^.dllnPrev:= WalkerParent;
    WalkerParent:= Walker;
    Walker:= WalkerParent^.dllnNext;
  end;
end;
{====================================================================}

{===TbcStack=========================================================}
function TbcStack.get_Version: string;
begin
  Result:= UnitVersion;
end;

procedure TbcStack.Lock;
begin
  fLock.Lock;
end;

procedure TbcStack.Unlock;
begin
  fLock.UnLock;
end;

constructor TbcStack.Create;
begin
  inherited Create;
  {allocate a head node}
  FHead:= snmAllocNode;
  FHead^.sllnNext:= nil;
  FHead^.sllnData:= nil;
  fLock:= bc_guardian.Guardian; { hook the global sentinel }
end;

constructor TbcStack.Create(const aGuard: TGuardian);
begin
  inherited Create;
  {allocate a head node}
  FHead:= snmAllocNode;
  FHead^.sllnNext:= nil;
  FHead^.sllnData:= nil;
  fLock:= aGuard;   { we get fed an initialized critical section }
end;

{--- MT OK ---}
destructor TbcStack.Destroy;
begin
  Clear;
  Lock;
  try snmFreeNode(FHead); finally Unlock; end;
  inherited Destroy;
end;
{--- MT OK ---}
procedure TbcStack.Clear;
var Temp: PsllNode;
begin
  Lock;
  try
    Temp:= FHead^.sllnNext;
    while (Temp <> nil) do begin
      FHead^.sllnNext:= Temp^.sllnNext;
      snmFreeNode(Temp);
      Temp:= FHead^.sllnNext;
    end;
    FCount := 0;
  finally Unlock; end;
end;
{--- MT OK ---}
function TbcStack.Examine: pointer;
begin
  Lock;
  try
    if (Count = 0) then raise Exception.Create('TaaStack.Examine: stack is empty');
    Result:= FHead^.sllnNext^.sllnData;
  finally Unlock; end;
end;
{--- MT OK ---}
function TbcStack.IsEmpty: boolean;
begin
  Lock;
  try
    Result:= Count = 0;
  finally Unlock; end;
end;
{--- MT OK ---}
function TbcStack._Pop: pointer;
var Temp: PsllNode;
begin
  Lock;
  try
    if (Count = 0) then raise Exception.Create('TaaStack.Pop: stack is empty');
    Temp:= FHead^.sllnNext;
    Result:= Temp^.sllnData;
    FHead^.sllnNext:= Temp^.sllnNext;
    snmFreeNode(Temp);
    dec(FCount);
  finally Unlock; end;
end;
{--- MT OK ---}
procedure TbcStack._Push(anItem: pointer);
var Temp: PsllNode;
begin
  Lock;
  try
    Temp:= snmAllocNode;
    Temp^.sllnData:= anItem;
    Temp^.sllnNext:= FHead^.sllnNext;
    FHead^.sllnNext:= Temp;
    inc(FCount);
  finally Unlock; end;
end;
{--- MT OK ---}
{====================================================================}

{===TbcQueue=========================================================}
procedure TbcQueue.DoOnDataInQueue; { threadsafe, but the callback isn't! }
begin
  if Assigned(fOnDataInQueue) then fOnDataInQueue(Self);
end;

{--- MT OK ---}
constructor TbcQueue.Create; { threadsafe }
begin
  inherited Create;
  {allocate a head node}
  fHead:= snmAllocNode;
  fHead^.sllnNext:= nil;
  fHead^.sllnData:= nil;
  {make the tail pointer point to the head node}
  fTail:= fHead;
  fLock:= bc_guardian.TGuardian.Create; { setup our own guardian sentinel }
end;

{--- MT OK ---}
destructor TbcQueue.Destroy; { threadsafe }
begin
  Clear;
  snmFreeNode(fHead);
  FreeAndNil(fLock); { teardown our sentinel }
  inherited Destroy;
end;

{--- MT OK ---}
procedure TbcQueue.Clear; { threadsafe }
var Temp: PsllNode;
begin
  Lock;
  try
    Temp:= fHead^.sllnNext;
    while (Temp <> nil) do begin
      fHead^.sllnNext:= Temp^.sllnNext;
      snmFreeNode(Temp);
      Temp:= fHead^.sllnNext;
    end;
    fCount:= 0;
    {the queue is now empty so make the tail pointer point to the head node}
    fTail:= fHead;
  finally Unlock; end;
end;

{--- MT OK ---}
function TbcQueue.Examine: pointer; { threadsafe }
begin
  Lock;
  try
    if (Count = 0) then raise Exception.Create('TbcQueue.Examine: Queue is empty');
    Result:= fHead^.sllnNext^.sllnData;
  finally Unlock; end;
end;

{--- MT OK ---}
function TbcQueue.IsEmpty: boolean; { threadsafe }
begin
  Lock;
  try
    Result:= Count = 0;
  finally Unlock; end;
end;

{--- MT OK ---}
function TbcQueue.De_Queue: pointer; { threadsafe } //bm
var Temp: PsllNode;
begin { trying to avoid exceptions in a lock }
  if (Count = 0) then raise Exception.Create('TbcQueue.De_Queue: Queue is empty');
  Lock;
  try
    Temp:= fHead^.sllnNext;
    Result:= Temp^.sllnData;
    fHead^.sllnNext:= Temp^.sllnNext;
    snmFreeNode(Temp);
    dec(fCount);
    {if we've managed to empty the queue, the tail pointer is now
     invalid, so reset it to point to the head node}
    if (Count = 0) then fTail:= fHead;
  finally Unlock; end;
end;

{--- MT OK ---}
procedure TbcQueue.En_Queue(anItem: pointer); { threadsafe }
var Temp: PsllNode;
begin
  Lock;
  try
    Temp:= snmAllocNode;
    Temp^.sllnData:= anItem;
    Temp^.sllnNext:= nil;
    {add the new node to the tail of the list and make sure the tail
     pointer points to the newly added node}
    fTail^.sllnNext:= Temp;
    fTail:= Temp;
    inc(fCount);
  finally Unlock; end;
  DoOnDataInQueue;
end;
{------------------------------------}
function TbcQueue.get_Version: string;
begin
  Result:= UnitVersion;
end;

{--- MT OK ---}
procedure TbcQueue.Lock;
begin
  fLock.Lock;
end;

{--- MT OK ---}
procedure TbcQueue.Unlock;
begin
  fLock.UnLock;
end;

{====================================================================}

procedure FinalizeUnit;
var
  STemp: PsnmPage;
  DTemp: PdnmPage;
begin
  {destroy all the single node pages}
  STemp:= snmPageList;
  while (STemp <> nil) do begin
    snmPageList:= STemp^.snmpNext;
    Dispose(STemp);
    STemp:= snmPageList;
  end;
  {destroy all the double node pages}
  DTemp:= dnmPageList;
  while (DTemp <> nil) do begin
    dnmPageList:= DTemp^.dnmpNext;
    Dispose(DTemp);
    DTemp:= dnmPageList;
  end;
end;

{ TbcObjectQueue }
// bc 04.07.2007
function TbcObjectQueue.Get: TObject;
begin
  Result:= TObject(De_Queue);
end;

procedure TbcObjectQueue.Put(anObject: TObject);
begin
  En_Queue(pointer(anObject));
end;

initialization
  snmFreeList := nil;
  snmPageList := nil;
  dnmFreeList := nil;
  dnmPageList := nil;

finalization
  FinalizeUnit;

end.
