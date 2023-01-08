

{******************************************************************************$
$          Unit name : bc_imgqueue.pas                                         $
$          Copyright : (C)2022 cdbc.dk                                         $
$          Programmer: Benny Christensen                                       $
$          Created   : 26.11.2022 /bc Idea and vision                          $
$          Updated   : 26.11.2022 /bc created ImgRec stuff & TImgQueue         $
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

unit bc_imgqueue;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, bc_basequeue, bc_baselist;

type
  { TImgRec holds 2 streams, 1 thumbnail and 1 fullsize image + a name & a type }
  PImgRec = ^TImgRec;
  TImgRec = record
    irHeight: ptrint;     { height of full size image }
    irID_dd: ptrint;      { id of the owner dataset }
    irID_ph: ptrint;      { id of the thumbnail rec in dataset }
    irID_pi: ptrint;      { id if the image rec in dataset }
    irImage: TStream;     { a full size image }
    irName: pchar;        { original file name }
    irThumbnail: TStream; { a 200x150 thumbnail image }
    irType: pchar;        { the type of image, .jpg, .png or .bmp etc... }
    irWidth: ptrint;      { width of full size image }
  end;
  { TbcImgQueue holds our results when they're ready, threadsafe }
  TbcImgQueue = class
  private
    fQ: TbcThreadQueue;
    function get_Count: ptrint;
    function get_EventOnlyOnFirstData: boolean;
    function get_hOwner: ptrint;
    function get_OnDataInQueue: TNotifyEvent;
    procedure set_EventOnlyOnFirstData(aValue: boolean);
    procedure set_hOwner(aValue: ptrint);
    procedure set_OnDataInQueue(aValue: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function DeQueue: PImgRec;
    procedure EnQueue(anItem: PImgRec);
    function Peek: PImgRec;
    function IsEmpty: boolean;
    property Count: ptrint read get_Count;
    property OnDataInQueue: TNotifyEvent read get_OnDataInQueue write set_OnDataInQueue;
    property EventOnlyOnFirstData: boolean read get_EventOnlyOnFirstData write set_EventOnlyOnFirstData;
    property HandleOwner: ptrint read get_hOwner write set_hOwner;
  end; { TbcImgQueue }
  { TbcImgList holds a list of imagerecords, NOT threadsafe! }
  TbcImgList = class
  private
    fFreeItemsOnExchange: boolean;
    fList: TbcBaseSingleList;
    function get_Count: ptrint;
    function get_Items(Index: ptrint): PImgRec;
    procedure set_Items(Index: ptrint; AValue: PImgRec);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(anItem: PImgRec); { appends data to list @ eol }
    procedure Clear; { throwing it all away, freeing item memory as we go along }
    function First: PImgRec; { puts the cursor at the first actual item, returns its data * pairs with next }
    function IsEmpty: boolean; { well duh! }
    procedure MoveBeforeFirst; { puts the cursor before the first actual item }
    procedure MoveLast; { puts the cursor @ EOL }
    function MoveNext: boolean; { moves the cursor forwards to the next item, returns false at EOL }
    function Next: PImgRec; { moves the cursor to the next item, returns its data or NIL at EOL * pairs with first }
    function Peek: PImgRec; { returns data @ cursor }
    property Count: ptrint read get_Count;
    property FreeItemsOnExchange: boolean read fFreeItemsOnExchange write fFreeItemsOnExchange;
    property Items[Index: ptrint]: PImgRec read get_Items write set_Items; default; { conveinience }
  end; { TbcImgList }

(*
CreatePH = 'CREATE TABLE dd_photos(id_ph integer primary key, id_dd integer, date_ph integer, namestr_ph varchar(128), thumb_ph blob, width_ph integer, height_ph integer, type_ph varchar(16), reserved_ph varchar(128));';

CreatePI = 'CREATE TABLE dd_pictures(id_pi integer primary key, id_ph integer, picture_pi blob, width_pi integer, height_pi integer, type_pi varchar(16), reserved_pi varchar(128));';
*)






{ bcCreateImgRec is the constructor for our image-records }
function bcCreateImgRec(const aName,aType: string;anId_dd: ptrint = -1): PImgRec; { functional programming }
{ bcCloneImgRec makes a copy/clone of the in-param }
function bcCloneImgRec(anImgRec: PImgRec): PImgRec;
{ bcDestroyImgRec is the destructor for our image-records }
procedure bcDestroyImgRec(anImgRec: PImgRec);

implementation
uses bc_advstring;

function bcCreateImgRec(const aName, aType: string;anId_dd: ptrint = -1): PImgRec;
begin
  Result:= GetMem(sizeof(TImgRec));
  with Result^ do begin
    irName:= bcStrNew(aName);
    irType:= bcStrNew(aType);
    irImage:= TMemoryStream.Create;
    irThumbnail:= TMemoryStream.Create;
    irHeight:= 0;
    irWidth:= 0;
    irID_dd:= anId_dd;
    irID_ph:= -1;
    irID_pi:= -1;
  end;
end; { bcCreateImgRec }

function bcCloneImgRec(anImgRec: PImgRec): PImgRec;
begin
  Result:= bcCreateImgRec(string(anImgRec^.irName),string(anImgRec^.irType),anImgRec^.irID_dd);
  anImgRec^.irImage.Position:= 0;
  Result^.irImage.CopyFrom(anImgRec^.irImage,anImgRec^.irImage.Size);
  Result^.irImage.Position:= 0;
  anImgRec^.irImage.Position:= 0;
  anImgRec^.irThumbnail.Position:= 0;
  Result^.irThumbnail.CopyFrom(anImgRec^.irThumbnail,anImgRec^.irThumbnail.Size);
  Result^.irThumbnail.Position:= 0;
  anImgRec^.irThumbnail.Position:= 0;
  Result^.irHeight:= anImgRec^.irHeight;
  Result^.irWidth:= anImgRec^.irWidth;
  Result^.irID_ph:= anImgRec^.irID_ph;
  Result^.irID_pi:= anImgRec^.irID_pi;
end; { bcCloneImgRec }

procedure bcDestroyImgRec(anImgRec: PImgRec);
begin
  with anImgRec^ do begin
    bcStrDispose(irName);
    bcStrDispose(irType);
    irImage.Free;
    irThumbnail.Free;
    irHeight:= 0;
    irWidth:= 0;
    irID_dd:= -1;
    irID_ph:= -1;
    irID_pi:= -1;
  end;
  FreeMemAndNil(anImgRec);
end; { DestroyImgRec }

{ TbcImgList }
function TbcImgList.get_Count: ptrint;
begin
  Result:= fList.Count;
end;

function TbcImgList.get_Items(Index: ptrint): PImgRec;
begin
  Result:= PImgRec(fList[Index]);
end;

procedure TbcImgList.set_Items(Index: ptrint; AValue: PImgRec);
var D: PImgRec;
begin
  if fFreeItemsOnExchange then begin
    D:= PImgRec(fList[Index]);
    bcDestroyImgRec(D);
  end;
  fList[Index]:= aValue;
end;

constructor TbcImgList.Create;
begin
  inherited Create;
  fList:= TbcBaseSingleList.Create;
end;

destructor TbcImgList.Destroy;
begin
  Clear;
  fList.Free;                                                 { free the list }
  inherited Destroy;
end;

procedure TbcImgList.Add(anItem: PImgRec);
begin
  fList.Add(anItem);
end;

procedure TbcImgList.Clear;
begin
  MoveBeforeFirst;                                            { from the start }
  while MoveNext do bcDestroyImgRec(PImgRec(fList.Examine));  { free items }
  fList.Clear;                                                { clear the list }
end;

function TbcImgList.Peek: PImgRec;
begin
  Result:= PImgRec(fList.Examine);
end;

function TbcImgList.First: PImgRec;
begin
  Result:= PImgRec(fList.First);
end;

function TbcImgList.IsEmpty: boolean;
begin
  Result:= fList.IsEmpty;
end;

procedure TbcImgList.MoveBeforeFirst;
begin
  fList.MoveBeforeFirst;
end;

procedure TbcImgList.MoveLast;
begin
  fList.MoveLast;
end;

function TbcImgList.MoveNext: boolean;
begin
  Result:= fList.MoveNext;
end;

function TbcImgList.Next: PImgRec;
begin
  Result:= PImgRec(fList.Next);
end;

{ TbcImgQueue }
function TbcImgQueue.get_Count: ptrint;
begin
  Result:= fQ.Count; { delegation }
end;

function TbcImgQueue.get_EventOnlyOnFirstData: boolean;
begin
  Result:= fQ.EventOnlyOnFirstData;
end;

function TbcImgQueue.get_hOwner: ptrint;
begin
  Result:= fQ.CallerHandle;
end;

function TbcImgQueue.get_OnDataInQueue: TNotifyEvent;
begin
  Result:= fQ.OnDataInQueue;
end;

procedure TbcImgQueue.set_EventOnlyOnFirstData(aValue: boolean);
begin
  fQ.EventOnlyOnFirstData:= aValue;
end;

procedure TbcImgQueue.set_hOwner(aValue: ptrint);
begin
  fQ.CallerHandle:= aValue;
end;

procedure TbcImgQueue.set_OnDataInQueue(aValue: TNotifyEvent);
begin
  fQ.OnDataInQueue:= aValue;
end;

constructor TbcImgQueue.Create;
begin
  fQ:= TbcThreadQueue.Create;
end;

destructor TbcImgQueue.Destroy;
begin
  Clear;
  fQ.Free;
  inherited Destroy;
end;

procedure TbcImgQueue.Clear;
begin
  if fQ.Count > 0 then while not fQ.IsEmpty do bcDestroyImgRec(PImgRec(fQ.DeQueue));
  fQ.Clear;
end;

function TbcImgQueue.DeQueue: PImgRec;
begin
  Result:= PImgRec(fQ.DeQueue);
end;

procedure TbcImgQueue.EnQueue(anItem: PImgRec);
begin
  fQ.EnQueue(pointer(anItem));
end;

function TbcImgQueue.Peek: PImgRec;
begin
  Result:= PImgRec(fQ.Peek);
end;

function TbcImgQueue.IsEmpty: boolean;
begin
  Result:= fQ.IsEmpty;
end;


end.

