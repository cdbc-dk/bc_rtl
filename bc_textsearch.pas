
{***************************************************************************
*        Unit name : bc_textsearch.pas                                     *
*        Copyright : (C)cdbc.dk 2022                                       *
*        Programmer: Benny Christensen                                     *
*        Created   : 18.10.2022 /bc helper for dealing with text searching.*
*        Updated   : 18.10.2022 /bc added                                  *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
****************************************************************************
*        Purpose:                                                          *
*        Classes for text searches etc.                                    *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*        TODO:                                                             *
****************************************************************************
*        License:                                                          *
*        "Beer License" - If you meet me one day, you'll buy me a beer :-) *
*        I'm NOT liable for anything! Use at your own risk!!!              *
***************************************************************************}

unit bc_textsearch;
{$mode objfpc}{$H+}
{.$define debug}
interface
uses
  Classes, SysUtils,bc_bomintf;
const
  MATCHESCOUNTRESIZER = 100; { Memory used = MATCHESCOUNTRESIZER * sizeof(TTextSearchRec) }
type
  SizeIntArray = array of SizeInt; { our own alias, = array of SizeInt; otherwise defined in StrUtils }
  { search record for our findings... }
  PTextSearchRec = ^TTextSearchRec;
  TTextSearchRec = record
    srKey: string;                          { the search key we're looking for }
    srId: ptruint;                             { correlation to bom.id_dd 1..1 }
    srCount: ptruint;                                   { how many did we find }
    srPos: SizeIntArray;                              { where are they located }
    srCursor: ptruint;     { internal tracker, for when traversing the results }
    srLength: ptruint;                              { length of the search-key }
    srName: string;            { what's the name of the record, we found it in }
  end;
  PTextSearchArray = ^TTextSearchArray;
  TTextSearchArray = array of TTextSearchRec;
  { TFreeTextSearch a class for encapsulating free text searching }
  TFreeTextSearch = class(TPersistent)
  private
    fDataset: IBom; { only visible in this class, derivatives must declare their own! }
    function get_Count: SizeInt;
    function get_Items(Index: SizeInt): PTextSearchRec;
    procedure set_Items(Index: SizeInt; aValue: PTextSearchRec);
  protected
    fCaseSensitive: boolean;
    fCursor,fMatchesCount: SizeInt;
    fLastRec: boolean;
    fMatchAll: boolean;
    fMatchesAllocatedLimit: SizeInt; { Currently allocated space for matches. }
    fSearchArray: TTextSearchArray;
    procedure Init;
    procedure ResizeAllocatedMatches; { Resizes the allocated space for matches index }
    procedure AddMatch(const aSearchRec: TTextSearchRec); { Add a SearchRec to the result-set }
  public
    constructor Create(aDataset: TObject); // IBom...constructor Create(aDataset: Ibom); // IBom...
    destructor Destroy; override;
    procedure ResetCursor; virtual;
    procedure Clear; virtual;
    function GetNextSearchRec: PTextSearchRec; virtual;
    procedure OnEnumerateItem(aSender: TObject;anItem: TObject;UserData: pointer;var aCancel: boolean); virtual;
    function SearchDataset(const aPattern: string;aCaseSensitive: boolean;aMatchAll: boolean = true): SizeInt; virtual;
    property Items[Index: SizeInt]: PTextSearchRec read get_Items write set_Items; default;
    property Count: SizeInt read get_Count;
    property IsLastRec: boolean read fLastRec;
  end;
{ bcGetNextSearchPosition, provided a searchrec, it will return the next pos in text,
  first time ~ first one, returns 0 on exhausted positions ~ spin around }
function bcGetNextSearchPosition(aSearchRec: PTextSearchRec): SizeInt;

implementation
uses Dialogs,bc_bomimpl,bc_advstring,LazUTF8;
var gId: SizeInt;

function bcGetNextSearchPosition(aSearchRec: PTextSearchRec): SizeInt;
begin
  if aSearchRec <> nil then begin
    Result:= aSearchRec^.srPos[aSearchRec^.srCursor];
    inc(aSearchRec^.srCursor);
    if aSearchRec^.srCursor >= aSearchRec^.srCount then aSearchRec^.srCursor:= 0;
  end;
end;

{ TFreeTextSearch }
procedure TFreeTextSearch.ResetCursor;
begin
  fCursor:= 0;
  fLastRec:= false;
end;

procedure TFreeTextSearch.ResizeAllocatedMatches;
begin
  fMatchesAllocatedLimit:= fMatchesCount + MATCHESCOUNTRESIZER;
  SetLength(fSearchArray,fMatchesAllocatedLimit);
end;

function TFreeTextSearch.get_Count: SizeInt;
begin
  Result:= fMatchesCount;
end;

procedure TFreeTextSearch.AddMatch(const aSearchRec: TTextSearchRec);
begin
  if fMatchesCount = fMatchesAllocatedLimit then begin
    ResizeAllocatedMatches;
  end;
  fSearchArray[fMatchesCount]:= aSearchRec;
  inc(fMatchesCount);
end;

function TFreeTextSearch.get_Items(Index: SizeInt): PTextSearchRec;
begin
  Result:= @fSearchArray[Index];
end;

procedure TFreeTextSearch.set_Items(Index: SizeInt; aValue: PTextSearchRec);
begin
  { do absolutely nothing! }
end;

procedure TFreeTextSearch.Init;
begin
  SetLength(fSearchArray,0);
  fCursor:= 0;
  fLastRec:= false;
  fMatchesCount:= 0;
  fMatchesAllocatedLimit:= 0;
  fMatchAll:= true;
end;

constructor TFreeTextSearch.Create(aDataset: TObject);
begin
  inherited Create;
  if not aDataset.GetInterface(SGUIDIBom,fDataset) then raise exception.CreateFmt('Error! %s does not implement an IBom interface.',[aDataset.ClassName]);
  Init;
end;

destructor TFreeTextSearch.Destroy;
begin
  Clear;
  fDataset:= nil;
  inherited Destroy;
end;

procedure TFreeTextSearch.Clear;
var
  SRec: PTextSearchRec;
begin
  ResetCursor;
  SRec:= GetNextSearchRec;
  while SRec <> nil do begin
    SetLength(SRec^.srPos,0);
    SRec^.srKey:= '';
    SRec^.srName:= '';
    SRec:= GetNextSearchRec;
  end;
  SetLength(fSearchArray,0);
  fMatchesAllocatedLimit:= 0;
  fMatchesCount:= 0;
  fCursor:= 0;
  fLastRec:= false;
end;

function TFreeTextSearch.GetNextSearchRec: PTextSearchRec;
begin
  if fCursor >= fMatchesCount then Result:= nil
  else begin
    Result:= @fSearchArray[fCursor];
    inc(fCursor);
    if fCursor = fMatchesCount then fLastRec:= true;
  end;
end;

procedure TFreeTextSearch.OnEnumerateItem(aSender: TObject; anItem: TObject;
  UserData: pointer; var aCancel: boolean);
var
  I: TBomItem;
  Pt: string;
  SRec: TTextSearchRec;
begin
  I:= TBomItem(anItem);
//  if anItem is TBomItem then I:= TBomItem(anItem) else exit; // if IBom(aSender).ItemClass is TBomItem then I:= TBomItem(anItem);
  Pt:= pchar(UserData);
  try
    if bcFindMatches(Pt,(I.Description+' '+I.Date.AsString),fCaseSensitive,SRec.srPos,fMatchAll) then begin
      SRec.srId:= I.Id;
      SRec.srKey:= Pt;
      SRec.srCount:= Length(SRec.srPos);
      SRec.srLength:= UTF8Length(Pt);
      SRec.srName:= I.Date.AsString;
      SRec.srCursor:= 0; { init for internal use }
      AddMatch(SRec);
      aCancel:= not fMatchAll; { break after first occurrence?!? }
    end;
  except aCancel:= true; end;
end;

function TFreeTextSearch.SearchDataset(const aPattern: string;aCaseSensitive: boolean;aMatchAll: boolean): SizeInt;
begin
  { initialize and clear for go! }
  if ((aPattern = '') or (fDataset.ItemCount = 0)) then exit; { nothing to do }
  fCaseSensitive:= aCaseSensitive;
  fMatchAll:= aMatchAll;
  Clear;
  { we're sending our search-pattern along in UserData }
  fDataset.Enumerate(@aPattern[1],@OnEnumerateItem); { you've got to love pointers :o) }
  { we've now filled our result-set, release excess memory }
  SetLength(fSearchArray,fMatchesCount);
  Result:= fMatchesCount;
end;

initialization
//  __Example:= nil;

finalization 
//  FreeAndNil(__Example);
  
end.

