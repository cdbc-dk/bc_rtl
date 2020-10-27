unit bc_objectringbuffer;
{=========================================
| container TObjectRingBuffer 13.03.2020 |
=========================================}
interface

uses
  Classes, SysUtils;

const
  UnitVersion = '1.31.03.2020';
type
  { TObjectRingBuffer }
  TObjectRingBuffer = class(TObject)
  private
    fStart: cardinal;
    fCount: cardinal;
    fElements: array of TObject;
    fMaxElements: cardinal;
  public
    constructor Create(MaxElements: cardinal);
    destructor Destroy; override;
    procedure Add(anElement: TObject);
    function GetFirst: TObject;
    function GetLast: TObject;
    function GetCount: cardinal;
    procedure Reset;
  end; { TObjectRingBuffer }

implementation

{ TObjectRingBuffer }
constructor TObjectRingBuffer.Create(MaxElements: cardinal);
begin
  inherited Create;
  fStart := 0;
  fCount := 0;
  fMaxElements := MaxElements;
  SetLength(fElements, fMaxElements);
end;

destructor TObjectRingBuffer.Destroy;
var Obj: TObject
begin
  // going out of business, free objects !!!
  for Obj in fElements do Obj.Free;
  SetLength(fElements, 0);
  inherited Destroy;
end;

procedure TObjectRingBuffer.Add(anElement: TObject);
var
  Idx: integer;
begin
  Idx:= (fStart + fCount) mod fMaxElements;
  fElements[Idx]:= anElement;
  if fCount < fMaxElements then Inc(fCount)
  else fStart:= (fStart + 1) mod fMaxElements;
end;

function TObjectRingBuffer.GetFirst: TObject;
begin
  Result:= fElements[fStart];
end;

function TObjectRingBuffer.GetLast: TObject;
begin
  Result:= fElements[(fStart + fCount - 1) mod fMaxElements];
end;

function TObjectRingBuffer.GetCount: cardinal;
begin
  Result:= fCount;
end;

procedure TObjectRingBuffer.Reset;
begin
  fStart:= 0;
  fCount:= 0;
end;

end. 
