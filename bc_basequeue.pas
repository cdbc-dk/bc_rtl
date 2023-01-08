

{******************************************************************************$
$          Unit name : bc_basequeue.pas                                        $
$          Copyright : (C)2022 cdbc.dk                                         $
$          Programmer: Benny Christensen                                       $
$          Created   : 08.12.2022 /bc Anscester queue with pointers            $
$          Updated   : 08.12.2022 /bc created TbcThreadQueue                   $
$                      30.11.2022 /bc added "bcCloneImgRec" function           $
$                      01.12.2022 /bc                                          $
$                                                                              $
$                                                                              $
$ **************************************************************************** $
$  Purpose:                                                                    $
$  Implements an image queue and                                               $
$  image-objects for use with multi-                                           $
$  threading communication.                                                    $
$                                                                              $
$******************************************************************************}

unit bc_basequeue;
{$mode ObjFPC}{$H+}
{$define UseNodeManager}
{$define debug}
interface
uses
  Classes, SysUtils, bc_guardian;

const
  UnitVersion = '0.17.12.2022';
  PageNodeCount = 30;

type
  { single-list node }
  PsllNode = ^TsllNode;
  TsllNode = packed record
    sllnNext: PsllNode;
    sllnData: pointer;
  end;
  { double-list node }
  PdllNode = ^TdllNode;
  TdllNode = packed record
    dllnNext: PdllNode;
    dllnPrev: PdllNode;
    dllnData: pointer;
  end;
  TbcCompareFunction = function (anItem1, anItem2: pointer): ptrint;
  { TbcBaseQueue is what its name says, a basic queue WITHOUT thread-support }
  TbcBaseQueue = class
  private
    fCount: ptrint;
    fEventOnlyOnFirstData: boolean;
    fHandle: ptrint;
    fHead: PsllNode;
    fTail: PsllNode;
    function get_Version: string;
  protected
    fOnDataInQueue: TNotifyEvent;
    procedure DoOnDataInQueue; { triggers the notify event, with Self as sender param }
  public
    constructor Create; overload;
    constructor Create(aHandle: ptrint); overload;
    destructor Destroy; override;
    procedure Clear;
    function De_Queue: pointer;
    procedure En_Queue(anItem: pointer);
    function Examine: pointer;
    function IsEmpty: boolean;
    property Count: ptrint read fCount;
    property OnDataInQueue: TNotifyEvent read fOnDataInQueue write fOnDataInQueue;
    property EventOnlyOnFirstData: boolean read fEventOnlyOnFirstData write fEventOnlyOnFirstData;
    property CallerHandle: ptrint read fHandle write fHandle;
    property Version: string read get_Version;
  end;
  { TbcThreadQueue is a threadsafe edition of the basequeue }
  TbcThreadQueue = class
  private
    fLock: TGuardian;
    fQ: TbcBaseQueue;
    function get_Count: ptrint;
    function get_EventOnlyOnFirstData: boolean;
    function get_Handle: ptrint;
    function get_OnDataInQueue: TNotifyEvent;
    procedure set_EventOnlyOnFirstData(aValue: boolean);
    procedure set_Handle(aValue: ptrint);
    procedure set_OnDataInQueue(aValue: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function DeQueue: pointer;
    procedure EnQueue(anItem: pointer);
    function Peek: pointer;
    function IsEmpty: boolean;
    property Count: ptrint read get_Count;
    property OnDataInQueue: TNotifyEvent read get_OnDataInQueue write set_OnDataInQueue;
    property EventOnlyOnFirstData: boolean read get_EventOnlyOnFirstData write set_EventOnlyOnFirstData;
    property CallerHandle: ptrint read get_Handle write set_Handle;
  end; { TbcThreadQueue }
  { TbcStringQueue a threadsafe string-queue }
  TbcStringQueue = class
  private
    fQ: TbcThreadQueue;
    function get_Count: ptrint;
    function get_EventOnlyOnFirstData: boolean;
    function get_Handle: ptrint;
    function get_OnDataInQueue: TNotifyEvent;
    procedure set_EventOnlyOnFirstData(aValue: boolean);
    procedure set_Handle(aValue: ptrint);
    procedure set_OnDataInQueue(aValue: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function DeQueue: string;
    procedure EnQueue(anItem: string);
    function Peek: string;
    function IsEmpty: boolean;
    property Count: ptrint read get_Count;
    property OnDataInQueue: TNotifyEvent read get_OnDataInQueue write set_OnDataInQueue;
    property EventOnlyOnFirstData: boolean read get_EventOnlyOnFirstData write set_EventOnlyOnFirstData;
    property CallerHandle: ptrint read get_Handle write set_Handle;
  end;

implementation
uses LCLIntf, bc_advstring;
{$i nodemanager.inc}
(* cutaway
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
*)
{ TbcStringQueue }
function TbcStringQueue.get_Count: ptrint;
begin
  Result:= fQ.Count;
end;

function TbcStringQueue.get_EventOnlyOnFirstData: boolean;
begin
  Result:= fQ.EventOnlyOnFirstData;
end;

function TbcStringQueue.get_Handle: ptrint;
begin
  Result:= fQ.CallerHandle;
end;

function TbcStringQueue.get_OnDataInQueue: TNotifyEvent;
begin
  Result:= fQ.OnDataInQueue;
end;

procedure TbcStringQueue.set_EventOnlyOnFirstData(aValue: boolean);
begin
  fQ.EventOnlyOnFirstData:= aValue;
end;

procedure TbcStringQueue.set_Handle(aValue: ptrint);
begin
  fQ.CallerHandle:= aValue;
end;

procedure TbcStringQueue.set_OnDataInQueue(aValue: TNotifyEvent);
begin
  fQ.OnDataInQueue:= aValue;
end;

constructor TbcStringQueue.Create;
begin
  fQ:= TbcThreadQueue.Create;
end;

destructor TbcStringQueue.Destroy;
begin
  fQ.Free;
  inherited Destroy;
end;

procedure TbcStringQueue.Clear;
begin
  fQ.Clear;
end;

function TbcStringQueue.DeQueue: string;
var P: pchar;
begin
  P:= pchar(fQ.DeQueue);
  Result:= bcStrCopy(P);
  bcStrDispose(P);
end;

procedure TbcStringQueue.EnQueue(anItem: string);
begin
  fQ.EnQueue(bcStrNew(anItem));
end;

function TbcStringQueue.Peek: string;
begin
  Result:= pchar(fQ.Peek);
end;

function TbcStringQueue.IsEmpty: boolean;
begin
  Result:= fQ.IsEmpty;
end;

{ TbcThreadQueue }
function TbcThreadQueue.get_Count: ptrint;
begin
  if fLock.BeginWrite then try Result:= fQ.Count; finally fLock.EndWrite; end;
end;

function TbcThreadQueue.get_EventOnlyOnFirstData: boolean;
begin
  if fLock.BeginWrite then try Result:= fQ.EventOnlyOnFirstData; finally fLock.EndWrite; end;
end;

function TbcThreadQueue.get_Handle: ptrint;
begin
  if fLock.BeginWrite then try Result:= fQ.CallerHandle; finally fLock.EndWrite; end;
end;

function TbcThreadQueue.get_OnDataInQueue: TNotifyEvent;
begin
  if fLock.BeginWrite then try Result:= fQ.OnDataInQueue; finally fLock.EndWrite; end;
end;

procedure TbcThreadQueue.set_EventOnlyOnFirstData(aValue: boolean);
begin
  if fLock.BeginWrite then try fQ.EventOnlyOnFirstData:= aValue; finally fLock.EndWrite; end;
end;

procedure TbcThreadQueue.set_Handle(aValue: ptrint);
begin
  if fLock.BeginWrite then try fQ.CallerHandle:= aValue; finally fLock.EndWrite; end;
end;

procedure TbcThreadQueue.set_OnDataInQueue(aValue: TNotifyEvent);
begin
  if fLock.BeginWrite then try fQ.OnDataInQueue:= aValue; finally fLock.EndWrite; end;
end;

constructor TbcThreadQueue.Create;
begin
  inherited Create;
  fLock:= TGuardian.Create;
  fQ:= TbcBaseQueue.Create;
end;

destructor TbcThreadQueue.Destroy;
begin
  FreeAndNil(fQ);
  FreeAndNil(fLock);
  inherited Destroy;
end;

procedure TbcThreadQueue.Clear;
begin
  if fLock.BeginWrite then try fQ.Clear; finally fLock.EndWrite; end;
end;

function TbcThreadQueue.DeQueue: pointer;
begin
  if fLock.BeginWrite then try Result:= fQ.De_Queue; finally fLock.EndWrite; end;
end;

procedure TbcThreadQueue.EnQueue(anItem: pointer);
begin
  if fLock.BeginWrite then try fQ.En_Queue(anItem); finally fLock.EndWrite; end;
end;

function TbcThreadQueue.Peek: pointer;
begin
  if fLock.BeginWrite then try Result:= fQ.Examine; finally fLock.EndWrite; end;
end;

function TbcThreadQueue.IsEmpty: boolean;
begin
  if fLock.BeginWrite then try Result:= fQ.IsEmpty; finally fLock.EndWrite; end;
end;

{ TbcBaseQueue }
function TbcBaseQueue.get_Version: string;
begin
  Result:= UnitVersion;
end; { TbcBaseQueue.get_Version }

procedure TbcBaseQueue.DoOnDataInQueue;
begin
  case fEventOnlyOnFirstData of
    false: begin
             if Assigned(fOnDataInQueue) then fOnDataInQueue(Self);
             if fHandle <> -1 then PostMessage(fHandle,4711,1,fCount); //LM_DATAINQUEUE   //bm
           end;
    true:  if fCount = 1 then begin
             if Assigned(fOnDataInQueue) then fOnDataInQueue(Self);
             if fHandle <> -1 then PostMessage(fHandle,4711,1,fCount);
           end;
  end;
end; { TbcBaseQueue.DoOnDataInQueue }

constructor TbcBaseQueue.Create;
begin
  inherited Create;
  {allocate a head node}
  fHead:= snmAllocNode;
  fHead^.sllnNext:= nil;
  fHead^.sllnData:= nil;
  {make the tail pointer point to the head node}
  fTail:= fHead;
  fHandle:= -1;
  fEventOnlyOnFirstData:= false;
end; { TbcBaseQueue.Create }

constructor TbcBaseQueue.Create(aHandle: ptrint);
begin
  Create; { call colleague }
  fHandle:= aHandle;
end; { TbcBaseQueue.Create }

destructor TbcBaseQueue.Destroy;
begin
  Clear;
  snmFreeNode(fHead);
  inherited Destroy;
end; { TbcBaseQueue.Destroy }

procedure TbcBaseQueue.Clear;
var Temp: PsllNode;
begin
  Temp:= fHead^.sllnNext;
  while (Temp <> nil) do begin
    fHead^.sllnNext:= Temp^.sllnNext;
    snmFreeNode(Temp);
    Temp:= fHead^.sllnNext;
  end;
  fCount:= 0;
  {the queue is now empty so make the tail pointer point to the head node}
  fTail:= fHead;
end; { TbcBaseQueue.Clear }

function TbcBaseQueue.De_Queue: pointer;
var Temp: PsllNode;
begin
  if (Count = 0) then raise Exception.Create('TbcBaseQueue.De_Queue: Queue is empty');
  Temp:= fHead^.sllnNext;
  Result:= Temp^.sllnData;
  fHead^.sllnNext:= Temp^.sllnNext;
  snmFreeNode(Temp);
  dec(fCount);
  {if we've managed to empty the queue, the tail pointer is now invalid,
   so reset it to point to the head node}
  if (Count = 0) then fTail:= fHead;
end; { TbcBaseQueue.De_Queue }

procedure TbcBaseQueue.En_Queue(anItem: pointer);
var Temp: PsllNode;
begin
  Temp:= snmAllocNode;
  Temp^.sllnData:= anItem;
  Temp^.sllnNext:= nil;
  {add the new node to the tail of the list and make sure the tail
   pointer points to the newly added node}
  fTail^.sllnNext:= Temp;
  fTail:= Temp;
  inc(fCount);
  DoOnDataInQueue; { look at property "EventOnlyOnFirstData" }
end; { TbcBaseQueue.En_Queue }

function TbcBaseQueue.Examine: pointer;
begin
  if (Count = 0) then raise Exception.Create('TbcBaseQueue.Examine: Queue is empty');
  Result:= fHead^.sllnNext^.sllnData;
end; { TbcBaseQueue.Examine }

function TbcBaseQueue.IsEmpty: boolean;
begin
  Result:= Count = 0;
end; { TbcBaseQueue.IsEmpty }


end.

