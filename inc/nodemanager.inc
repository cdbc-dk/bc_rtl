
(* include file for lists and stacks and queues *)

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
  {$ifdef UseNodeManager}
  {add the node to the top of the free list}
  aNode^.sllnNext := snmFreeList;
  snmFreeList := aNode;
  {$else}
  Dispose(aNode);
  {$endif}
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
  {$ifdef UseNodeManager}
  {if the free list is empty, allocate a new page of nodes}
  if (snmFreeList = nil) then
    snmAllocPage;
  {return the first node on the free list}
  Result := snmFreeList;
  snmFreeList := Result^.sllnNext;
  {$else}
  New(Result);
  {$endif}
  {$ifdef debug}
  Result^.sllnNext := nil;
  Result^.sllnData := nil;
  {$endif}
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
  {$ifdef UseNodeManager}
  {add the node to the top of the free list}
  aNode^.dllnNext := dnmFreeList;
  dnmFreeList := aNode;
  {$else}
  Dispose(aNode);
  {$endif}
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
  {$ifdef UseNodeManager}
  {if the free list is empty, allocate a new page of nodes}
  if (dnmFreeList = nil) then
    dnmAllocPage;
  {return the first node on the free list}
  Result := dnmFreeList;
  dnmFreeList := Result^.dllnNext;
  {$else}
  New(Result);
  {$endif}
  {$ifdef debug}
  Result^.dllnNext := nil;
  Result^.dllnData := nil;
  {$endif}
end;
{====================================================================}

(* end of nodemanager include file *)


