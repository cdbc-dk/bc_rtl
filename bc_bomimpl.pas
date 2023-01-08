unit bc_bomimpl;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, bc_bomintf, bc_datetime;
type
  { TBomItem ~ is an object with basic properties, meant for use with IBom/TBom }
  TBomItem = class
  private
    fAmount: double;
    fDate: TIsoDate;
    fDesc: string;
    fId: ptruint;
    fName: string;
    fTag: ptrint;
    fUserdata: pointer;
  public
    constructor Create;
    destructor Destroy; override;
    property Amount: double read fAmount write fAmount;
    property Date: TIsoDate read fDate;
    property Description: string read fDesc write fDesc;
    property Id: ptruint read fId write fId;
    property Name: string read fName write fName;
    property Tag: ptrint read fTag write fTag;
    property Userdata: pointer read fUserdata write fUserdata;
  end; { TBomItem }
  TBomItemClass = class of TBomItem;
  { TBom a very basic object that implements ibom, works with TBomItemClass }
  TBom = class(TObject,IBom)
  protected
    fFilename: string;
    fItemClass: TBomItemClass;
    fItems: TFPList;
    fModified: boolean;
    fOwnsObjects: boolean;
    fTag: ptrint;
    fUserdata: pointer;
    function get_ItemCount: ptruint; virtual; //ibom
    function get_ItemClass: TClass; virtual;  //ibom
    function get_Modified: boolean; virtual; //ibom
    { used here and in descendant classes when they choose not to implement a method }
    procedure RaiseErrorNotImplemented(aSender: TObject;aMethodName: string);
    property Filename: string read fFilename write fFilename; // to be elevated to public in descendant
  public
    constructor Create(anItemClass: TBomItemClass;OwnsObjects: boolean = true); virtual;
    destructor Destroy; override;
    procedure Clear; virtual; //ibom
    function AddItem: TObject; virtual; //ibom, returns TBomItemClass
    function GetItemObjFromID(const anId: ptruint): TObject; virtual; //ibom
    function IndexOfObj(anItem: TObject): ptrint; virtual; //ibom
    function ReadData: boolean; virtual; //ibom
    function ReadDataWithBlob(const Asc: boolean): boolean; virtual; //ibom
    function UpdateData: boolean; virtual; //ibom
    procedure Enumerate(UserData: pointer;aCallback: TbomEnumerateEvent); virtual; //ibom
    property ItemCount: ptruint read get_ItemCount; //ibom
    property ItemClass: TClass read get_ItemClass; //ibom
    property Modified: boolean read get_Modified; //ibom
    property Tag: ptrint read fTag write fTag; //not in ibom
    property Userdata: pointer read fUserdata write fUserdata; //not in ibom
  end; { TBom }
{ function for comparing items by date }
function BomDateCompare(Item1, Item2: Pointer): Integer;

implementation
var
  bcFormatSettings: TFormatSettings; //?!?

{ TBom }
function TBom.get_Modified: boolean;
begin
  Result:= fModified;
end;

function TBom.get_ItemCount: ptruint;
begin
  Result:= fItems.Count;
end;

function TBom.get_ItemClass: TClass;
begin
  Result:= fItemClass;
end;

procedure TBom.RaiseErrorNotImplemented(aSender: TObject; aMethodName: string);
begin
  raise ENotImplemented.CreateFmt('Sorry, %s.%s is not implemented yet.',[aSender.ClassName,aMethodName]);
end;

constructor TBom.Create(anItemClass: TBomItemClass; OwnsObjects: boolean);
begin
  inherited Create;
  fItemClass:= anItemClass;
  fOwnsObjects:= OwnsObjects;
  fItems:= TFPList.Create;
  fTag:= 0;
  fUserdata:= nil;
  fFilename:= '';
  fModified:= false;
end;

destructor TBom.Destroy;
begin
  Clear;
  fItems.Free;
  fItemClass:= nil;
  inherited Destroy;
end;

procedure TBom.Clear;
var I: ptrint;
begin
  if fOwnsObjects then for I:= fItems.Count-1 downto 0 do TBomItem(fItems[I]).Free;
  fItems.Clear;
end;

function TBom.AddItem: TObject;
begin
  Result:= fItemClass.Create;
  TBomItem(Result).Id:= fItems.Add(Result); { get our new id straight away }
  fModified:= true;
end;

function TBom.GetItemObjFromID(const anId: ptruint): TObject;
var I: ptrint;
begin
  Result:= nil;
  for I:= 0 to fItems.Count-1 do if TBomItem(fItems[I]).Id = anId then begin
    Result:= TBomItem(fItems[I]);
    break;
  end;
end;

function TBom.IndexOfObj(anItem: TObject): ptrint;
begin
  Result:= fItems.IndexOf(pointer(anItem));
end;

function TBom.ReadData: boolean;
//var Sl: TStringList;
begin
  Result:= false;
  RaiseErrorNotImplemented(Self,'ReadData');
(*
  Sl:= TStringList.Create;
  try
    bcLoadFromFileUTF8(Sl,fFilename);
    ParseInput(Sl.Text);
    fItems.Sort(@BomDateCompare);
    Result:= true;
    fModified:= false;
  finally Sl.Free; end;
*)
end;

function TBom.ReadDataWithBlob(const Asc: boolean): boolean;
begin
  Result:= false;
  RaiseErrorNotImplemented(Self,'ReadDataWithBlob');
end;

function TBom.UpdateData: boolean;
begin
  Result:= false;
  RaiseErrorNotImplemented(Self,'UpdateData');
(*
  try
    RenameFile(fFilename,ChangeFileExt(fFilename,'.bak')); { retain the closest backup }
  except end;
  Sl:= TStringList.Create;
  try
    ....
    bcSaveToFileUTF8(Sl,fFilename);           { save the file with an utf8 bom }
    Result:= true;
    fModified:= false;
  finally Sl.Free; end;
*)
end;

procedure TBom.Enumerate(UserData: pointer; aCallback: TbomEnumerateEvent);
var
  Cancel: boolean;
  I: ptrint;
begin
  if not assigned(aCallback) then exit;
  Cancel:= false;
  for I:= 0 to fItems.Count-1 do begin
    aCallback(Self,TObject(fItems[I]),UserData,Cancel); { works! } //bm
//    aCallback(Self,TBomItem(fItems[I]),UserData,Cancel); { type-specific, works too }
    if Cancel then break;
  end;
end;

{ TBomItem }
constructor TBomItem.Create;
begin
  inherited Create;
  fDate:= TIsoDate.Create(now);
end;

destructor TBomItem.Destroy;
begin
  fDate.Free;
  inherited Destroy;
end;

function BomDateCompare(Item1, Item2: Pointer): Integer;
begin
  if TBomItem(Item1).Date.AsInteger < TBomItem(Item2).Date.AsInteger then Result:= -1
  else if TBomItem(Item1).Date.AsInteger = TBomItem(Item2).Date.AsInteger then Result:= 0
  else if TBomItem(Item1).Date.AsInteger > TBomItem(Item2).Date.AsInteger then Result:= 1;
end;

initialization
  bcFormatSettings:= DefaultFormatSettings;
  bcFormatSettings.ThousandSeparator:= '.';
  bcFormatSettings.DecimalSeparator:= ',';
  bcFormatSettings.CurrencyString:= '';
end.

