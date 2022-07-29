{%MainUnit bc_types.pas}
{ implementation: (c)20.04.2020 /bc
  updated:           12.05.2020 /bc added two class functions }

uses bc_strings; { provides StringWorkshop, requires UnitVersion >= '3.20.04.2020' /bc }
type
  PPtruint = ^ptruint;        { pointer magic }
  PVersionRec = ^TVersionRec; { pointer magic }
  TVersionRec = packed record
    vrBuild_Number,
    vrMicro_Number,
    vrMinor_Number,
    vrMajor_Number: word; { 2 bytes each = 8 bytes -> size of ptruint on 64 bit }
  end;

{ *** TVersion *** }
function TVersion.get_AsPtrUint: ptruint;
begin
  Result:= fVersionNumber;
end;

function TVersion.get_AsString: string;
begin
  Result:= DecodeToString(fVersionNumber);
end;

function TVersion.EncodeFromString(const aVersionString: string): ptruint;
var
  S: string;
  VRec: TVersionRec;
begin
  Result:= 0; { it's unsigned, hence the zero }
  { first off, pick the string apart, we know the format :-) }
  S:= StringWorkshop.GetFieldToken(1,aVersionString,'.');
  VRec.vrMajor_Number:= StrToInt(S);
  S:= StringWorkshop.GetFieldToken(2,aVersionString,'.');
  VRec.vrMinor_Number:= StrToInt(S);
  S:= StringWorkshop.GetFieldToken(3,aVersionString,'.');
  VRec.vrMicro_Number:= StrToInt(S);
  S:= StringWorkshop.GetFieldToken(4,aVersionString,'.');
  VRec.vrBuild_Number:= StrToInt(S);
  { now calculate the result }
  Result:= PPtruint(@VRec)^; { pointer magic }
end;

function TVersion.DecodeToString(const aVersionNumber: ptruint): string;
var
  VRec: TVersionRec;
begin
  Result:= '';                          { initialize result }
  VRec:= PVersionRec(@aVersionNumber)^; { pointer magic, tversionrec is 8 bytes & ptruint is also 8 bytes }
  { now encode the result string, single digits will be zero-padded }
  Result:= StringWorkshop.IntToStrPad0(VRec.vrMajor_Number)+'.'+
           StringWorkshop.IntToStrPad0(VRec.vrMinor_Number)+'.'+
           StringWorkshop.IntToStrPad0(VRec.vrMicro_Number)+'.'+
           StringWorkshop.IntToStrPad0(VRec.vrBuild_Number);
end;

constructor TVersion.Create(const aVersionNumber: ptruint);
begin
  inherited Create;
  fVersionNumber:= aVersionNumber;
end;

constructor TVersion.Create(const aUnitVersion: string);
begin
  inherited Create;
  fVersionNumber:= EncodeFromString(aUnitVersion);
end;

destructor TVersion.Destroy;
begin
  inherited Destroy;
end;

class function TVersion.VersionNumberToString(const aVersionNumber: ptruint): string;
begin
  with TVersion.Create(aVersionNumber) do begin
    Result:= AsString;
    Free;
  end;
end;

class function TVersion.VersionStringToPtrUint(const aVersionString: string): ptruint;
begin
  with TVersion.Create(aVersionString) do begin
    Result:= AsPtrUint;
    Free;
  end;
end;

{ included by bc_types.pas }
