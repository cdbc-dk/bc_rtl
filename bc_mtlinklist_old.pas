{******************************************************
*                                                     *
*  Thread safe implementation of a Stack and a Queue  *
*                                                     *
******************************************************}
unit bc_mtlinklist;

interface

uses
  Classes, SysUtils;

{$IFOPT D+}
{$DEFINE InDebugMode}
{$ENDIF}

{$DEFINE UseNodeManager}

const
	UnitVersion = '1.31.10.2014';
  PageNodeCount = 30;

type
  TaaCompareFunction = function (aItem1, aItem2 : pointer) : integer;

type
  PsllNode = ^TsllNode;
  TsllNode = packed record
    sllnNext: PsllNode;
    sllnData: pointer;
  end;

  TaaSingleList = class
  private
    FCount: integer;
    FCursor: PsllNode;
    FHead: PsllNode;
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
    procedure Sort(aCompare : TaaCompareFunction);
    property Count : integer read FCount;
  end; { TaaSingleList }

type
  PdllNode = ^TdllNode;
  TdllNode = packed record
    dllnNext: PdllNode;
    dllnPrev: PdllNode;
    dllnData: pointer;
  end;

  TaaDoubleList = class
  private
    FCount: integer;
    FCursor: PdllNode;
    FHead: PdllNode;
    FTail: PdllNode;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure InsertBefore(aItem: pointer);
    procedure InsertAfter(aItem: pointer);
    function Delete: pointer;
    function IsAfterLast: boolean;
    function IsBeforeFirst: boolean;
    procedure MoveAfterLast;
    procedure MoveBeforeFirst;
    function MoveNext: boolean;
    function MovePrevious: boolean;
    procedure Clear;
    function Examine: pointer;
    procedure Sort(aCompare: TaaCompareFunction);
    property Count: integer read FCount;
  end; { TaaDoubleList }


type

  { TaaStack }

  TaaStack = class
  private
    FCount: integer;
    FHead: PsllNode;
    function get_Version: string;
  protected
    fLock: TRtlCriticalSection;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Examine: pointer;
    function IsEmpty: boolean;
    function Pop: pointer;
    procedure Push(aItem: pointer);
    property Count: integer read FCount;
    property Version: string read get_Version;
  end; { TaaStack }

type
  { TaaQueue }
  TaaQueue = class
  private
    fCount: integer;
    fHead: PsllNode;
    fTail: PsllNode;
    function get_Version: string;
  protected
    fLock: TRtlCriticalSection;
    fOnDataInQueue: TNotifyEvent;
    procedure Lock;
    procedure Unlock;
    procedure DoOnDataInQueue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Dequeue: pointer;
    procedure Enqueue(aItem: pointer);
    function Examine: pointer;
    function IsEmpty: boolean;
    property Count: integer read FCount;
    property OnDataInQueue: TNotifyEvent read fOnDataInQueue write fOnDataInQueue;
    property Version: string read get_Version;
  end;

type
  TbcObjectQueue = class(TaaQueue)
    procedure En_Queue(anObject: TObject); overload;
    function De_Queue: TObject; overload;
    function Inspect: TObject;
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
  i       : integer;
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
  i       : integer;
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


{===TaaSingleList====================================================}
constructor TaaSingleList.Create;
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
destructor TaaSingleList.Destroy;
begin
  Clear;
  snmFreeNode(FHead);
  inherited Destroy;
end;
{--------}
procedure TaaSingleList.Clear;
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
function TaaSingleList.DeleteAfter : pointer;
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
function TaaSingleList.Examine : pointer;
begin
  {return the data part of the cursor}
  Result := FCursor^.sllnData;
end;
{--------}
procedure TaaSingleList.InsertAfter(aItem : pointer);
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
function TaaSingleList.IsBeforeFirst : boolean;
begin
  Result := FCursor = FHead;
end;
{--------}
function TaaSingleList.IsLast : boolean;
begin
  Result := FCursor^.sllnNext = nil;
end;
{--------}
procedure TaaSingleList.MoveBeforeFirst;
begin
  {set the cursor to the head node}
  FCursor := FHead;
end;
{--------}
function TaaSingleList.MoveNext : boolean;
begin
  {advance the cursor to its own next pointer}
  if (FCursor^.sllnNext = nil) then
    Result := false
  else begin
    FCursor := FCursor^.sllnNext;
    Result := true;
  end;
end;
{--------}
procedure TaaSingleList.Sort(aCompare : TaaCompareFunction);
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


{===TaaDoubleList========================================================}
constructor TaaDoubleList.Create;
begin
  inherited Create;
  {allocate a head and a tail node}
  FHead := dnmAllocNode;
  FTail := dnmAllocNode;
  FHead^.dllnNext := FTail;
  FHead^.dllnPrev := nil;
  FHead^.dllnData := nil;
  FTail^.dllnNext := nil;
  FTail^.dllnPrev := FHead;
  FTail^.dllnData := nil;
  {set the cursor to the head node}
  FCursor := FHead;
end;
{--------}
destructor TaaDoubleList.Destroy;
begin
  Clear;
  dnmFreeNode(FHead);
  dnmFreeNode(FTail);
  inherited Destroy;
end;
{--------}
procedure TaaDoubleList.Clear;
var
  Temp : PdllNode;
begin
  Temp := FHead^.dllnNext;
  while (Temp <> FTail) do begin
    FHead^.dllnNext := Temp^.dllnNext;
    dnmFreeNode(Temp);
    Temp := FHead^.dllnNext;
  end;
  FCount := 0;
end;
{--------}
function TaaDoubleList.Delete : pointer;
var
  OldNode : PdllNode;
begin
  OldNode := FCursor;
  if (OldNode = FHead) or (OldNode = FTail) then
    raise Exception.Create('TaaDoubleList.Delete: cannot delete - at start/end of list');
  Result := OldNode^.dllnData;
  OldNode^.dllnPrev^.dllnNext := OldNode^.dllnNext;
  OldNode^.dllnNext^.dllnPrev := OldNode^.dllnPrev;
  FCursor := OldNode^.dllnNext;
  dnmFreeNode(OldNode);
  dec(FCount);
end;
{--------}
function TaaDoubleList.Examine : pointer;
begin
  {return the data part of the cursor}
  Result := FCursor^.dllnData;
end;
{--------}
procedure TaaDoubleList.InsertAfter(aItem : pointer);
var
  NewNode : PdllNode;
begin
  if (FCursor = FTail) then
    FCursor := FCursor^.dllnPrev;
  {allocate a new node and insert after the cursor}
  NewNode := dnmAllocNode;
  NewNode^.dllnData := aItem;
  NewNode^.dllnNext := FCursor^.dllnNext;
  NewNode^.dllnPrev := FCursor;
  FCursor^.dllnNext := NewNode;
  NewNode^.dllnNext^.dllnPrev := NewNode;
  inc(FCount);
end;
{--------}
procedure TaaDoubleList.InsertBefore(aItem : pointer);
var
  NewNode : PdllNode;
begin
  if (FCursor = FHead) then
    FCursor := FCursor^.dllnNext;
  {allocate a new node and insert before the cursor}
  NewNode := dnmAllocNode;
  NewNode^.dllnData := aItem;
  NewNode^.dllnNext := FCursor;
  NewNode^.dllnPrev := FCursor^.dllnPrev;
  NewNode^.dllnPrev^.dllnNext := NewNode;
  FCursor^.dllnPrev := NewNode;
  inc(FCount);
end;
{--------}
function TaaDoubleList.IsAfterLast : boolean;
begin
  Result := FCursor = FTail;
end;
{--------}
function TaaDoubleList.IsBeforeFirst : boolean;
begin
  Result := FCursor = FHead;
end;
{--------}
procedure TaaDoubleList.MoveAfterLast;
begin
  {set the cursor to the tail node}
  FCursor := FTail;
end;
{--------}
procedure TaaDoubleList.MoveBeforeFirst;
begin
  {set the cursor to the head node}
  FCursor := FHead;
end;
{--------}
function TaaDoubleList.MoveNext : boolean;
begin
  {advance the cursor to its own next pointer}
  if (FCursor = FTail) then
    Result := false
  else begin
    FCursor := FCursor^.dllnNext;
    Result := true;
  end;
end;
{--------}
function TaaDoubleList.MovePrevious : boolean;
begin
  {retard the cursor to its own previous pointer}
  if (FCursor = FHead) then
    Result := false
  else begin
    FCursor := FCursor^.dllnPrev;
    Result := true;
  end;
end;
{--------}
procedure TaaDoubleList.Sort(aCompare : TaaCompareFunction);
var
  Walker : PdllNode;
  Temp   : PdllNode;
  WalkerParent : PdllNode;
  TempParent   : PdllNode;
begin
  {if there are zero (or one) items the list is already sorted}
  if (Count <= 1) then
    Exit;
  {perform an insertion sort from the second item onwards}
  WalkerParent := FHead^.dllnNext;
  Walker := WalkerParent^.dllnNext;
  while (Walker <> FTail) do begin
    {find where the walker item should be in the sorted list to its
     left - we walk the sorted sublist making a note of the parent as
     we go so that we can insert properly. Note that the loop below
     will terminate in the worst case by the walker node itself - we
     won't run off the end of the list}
    TempParent := FHead;
    Temp := TempParent^.dllnNext;
    while (aCompare(Temp^.dllnData, Walker^.dllnData) < 0) do begin
      TempParent := Temp;
      Temp := TempParent^.dllnNext;
    end;
    {did we find the walker node? If so, move the walker's parent on
     by one link}
    if (Temp = Walker) then begin
      WalkerParent := Walker;
    end
    {otherwise, move the walker node into the correct place in the
     sorted sublist}
    else begin
      {disconnect the walker node}
      WalkerParent^.dllnNext := Walker^.dllnNext;
      {connect the walker node in the correct place}
      Walker^.dllnNext := Temp;
      TempParent^.dllnNext := Walker;
    end;
    {set the walker node}
    Walker := WalkerParent^.dllnNext;
  end;
  {now patch up all of the previous links}
  WalkerParent := FHead;
  Walker := WalkerParent^.dllnNext;
  while (WalkerParent <> FTail) do begin
    Walker^.dllnPrev := WalkerParent;
    WalkerParent := Walker;
    Walker := WalkerParent^.dllnNext;
  end;
end;
{====================================================================}

function TaaStack.get_Version: string;
begin
  Result:= UnitVersion;
end;

procedure TaaStack.Lock;
begin
  EnterCriticalsection(fLock);
end;

procedure TaaStack.Unlock;
begin
  LeaveCriticalsection(fLock);
end;

{===TaaStack=========================================================}
constructor TaaStack.Create;
begin
  inherited Create;
  {allocate a head node}
  FHead := snmAllocNode;
  FHead^.sllnNext := nil;
  FHead^.sllnData := nil;
  InitCriticalSection(fLock); // initialize critical section
end;
{--- MT OK ---}
destructor TaaStack.Destroy;
begin
  Clear;
  Lock;
  try snmFreeNode(FHead); finally Unlock; end;
  DoneCriticalsection(fLock); // finalize critical section
  inherited Destroy;
end;
{--- MT OK ---}
procedure TaaStack.Clear;
var
  Temp : PsllNode;
begin
  Lock;
  try
    Temp := FHead^.sllnNext;
    while (Temp <> nil) do begin
      FHead^.sllnNext := Temp^.sllnNext;
      snmFreeNode(Temp);
      Temp := FHead^.sllnNext;
    end;
    FCount := 0;
  finally Unlock; end;
end;
{--- MT OK ---}
function TaaStack.Examine : pointer;
begin
  Lock;
  try
    if (Count = 0) then
      raise Exception.Create('TaaStack.Examine: stack is empty');
    Result := FHead^.sllnNext^.sllnData;
  finally Unlock; end;
end;
{--- MT OK ---}
function TaaStack.IsEmpty : boolean;
begin
  Lock;
  try
    Result := Count = 0;
  finally Unlock; end;
end;
{--- MT OK ---}
function TaaStack.Pop : pointer;
var
  Temp : PsllNode;
begin
  Lock;
  try
    if (Count = 0) then
      raise Exception.Create('TaaStack.Pop: stack is empty');
    Temp := FHead^.sllnNext;
    Result := Temp^.sllnData;
    FHead^.sllnNext := Temp^.sllnNext;
    snmFreeNode(Temp);
    dec(FCount);
  finally Unlock; end;
end;
{--- MT OK ---}
procedure TaaStack.Push(aItem : pointer);
var
  Temp : PsllNode;
begin
  Lock;
  try
    Temp := snmAllocNode;
    Temp^.sllnData := aItem;
    Temp^.sllnNext := FHead^.sllnNext;
    FHead^.sllnNext := Temp;
    inc(FCount);
  finally Unlock; end;
end;
{--- MT OK ---}
{====================================================================}
{ multithreading support }
procedure TaaQueue.Lock;
begin
  EnterCriticalsection(fLock);
end;

procedure TaaQueue.UnLock;
begin
  LeaveCriticalsection(fLock);
end;

procedure TaaQueue.DoOnDataInQueue;
begin
  if Assigned(fOnDataInQueue) then fOnDataInQueue(nil);
end;

{===TaaQueue=========================================================}
constructor TaaQueue.Create;
begin
  inherited Create;
  {allocate a head node}
  fHead:= snmAllocNode;
  fHead^.sllnNext:= nil;
  fHead^.sllnData:= nil;
  {make the tail pointer point to the head node}
  fTail:= fHead;
  InitCriticalSection(fLock); // initialize critical section
end;
{----MT OK----}
destructor TaaQueue.Destroy;
begin
  Clear;
  Lock;
  try snmFreeNode(fHead); finally Unlock; end;
  DoneCriticalsection(fLock); // finalize critical section
  inherited Destroy;
end;
{----MT OK----}
procedure TaaQueue.Clear;
var
  Temp: PsllNode;
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
    {the queue is now empty so make the tail pointer point to the head
     node}
    fTail:= fHead;
  finally Unlock; end;
end;
{----MT OK----}
function TaaQueue.Examine : pointer;
begin
  Lock;
  try
    if (Count = 0) then raise Exception.Create('TaaQueue.Examine: queue is empty');
    Result:= fHead^.sllnNext^.sllnData;
  finally Unlock; end;
end;
{----MT OK----}
function TaaQueue.IsEmpty : boolean;
begin
  Lock;
  try
    Result:= Count = 0;
  finally Unlock; end;
end;
{----MT OK----}
function TaaQueue.Dequeue : pointer;
var
  Temp : PsllNode;
begin
  Lock;
  try
    if (Count = 0) then raise Exception.Create('TaaQueue.Dequeue: queue is empty');
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
{----MT OK----}
procedure TaaQueue.Enqueue(aItem: pointer);
var
  Temp: PsllNode;
begin
  Lock;
  try
    Temp:= snmAllocNode;
    Temp^.sllnData:= aItem;
    Temp^.sllnNext:= nil;
    {add the new node to the tail of the list and make sure the tail
     pointer points to the newly added node}
    fTail^.sllnNext:= Temp;
    fTail:= Temp;
    inc(fCount);
  finally Unlock; end;
  DoOnDataInQueue;
end;

function TaaQueue.get_Version: string;
begin
  Result:= UnitVersion;
end;
{====================================================================}


procedure FinalizeUnit;
var
  STemp : PsnmPage;
  DTemp : PdnmPage;
begin
  {destroy all the single node pages}
  STemp := snmPageList;
  while (STemp <> nil) do begin
    snmPageList := STemp^.snmpNext;
    Dispose(STemp);
    STemp := snmPageList;
  end;
  {destroy all the double node pages}
  DTemp := dnmPageList;
  while (DTemp <> nil) do begin
    dnmPageList := DTemp^.dnmpNext;
    Dispose(DTemp);
    DTemp := dnmPageList;
  end;
end;

{ TbcObjectQueue }
// bc 13.03.2007
function TbcObjectQueue.De_Queue: TObject;
begin
  Result:= TObject(Dequeue);
end;

procedure TbcObjectQueue.En_Queue(anObject: TObject);
begin
  Enqueue(pointer(anObject));
end;

function TbcObjectQueue.Inspect : TObject;
begin
  Result:= TObject(Examine);
end;

initialization
  snmFreeList := nil;
  snmPageList := nil;
  dnmFreeList := nil;
  dnmPageList := nil;

finalization
  FinalizeUnit;

end.
