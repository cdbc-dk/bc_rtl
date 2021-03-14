{%MainUnit bc_datetime.pas}
{ implementation: (c)cdbc 01.03.2021 /bc }

{ TIsoTime }
function TIsoTime.get_AsInteger: ptrint;
begin
  Result:= fIntTime; // 10.08.2006 new implementation, now stored as seconds per day
end;

{ adds or subtracts hours according to timezone, accepts negative values }
function TIsoTime.get_Timezone: integer;
begin
  Result:= fTimezone;
end;

function TIsoTime.Get_Hours: word;
begin
  // 10.08.2006 new implementation, stored as seconds per day
  // formula: fIntTime = ((H*3600) + (M*60) + S)
  //          H = fIntTime div 3600; L = fIntTime mod 3600
  //          M = L div 60
  //          S = L mod 60
  Result:= fIntTime div 3600;
end;

function TIsoTime.Get_Minutes: word;
var L: ptrint;
begin
  // 10.08.2006 new implementation, stored as seconds per day
  // formula: fIntTime = ((H*3600) + (M*60) + S)
  //          H = fIntTime div 3600; L = fIntTime mod 3600
  //          M = L div 60
  //          S = L mod 60
  L:= fIntTime mod 3600;
  Result:= L div 60;
end;

function TIsoTime.Get_Seconds: word;
var L: ptrint;
begin
  // 10.08.2006 new implementation, stored as seconds per day
  // formula: fIntTime = ((H*3600) + (M*60) + S)
  //          H = fIntTime div 3600; L = fIntTime mod 3600
  //          M = L div 60
  //          S = L mod 60
  L:= fIntTime mod 3600;
  Result:= L mod 60;
end;

function TIsoTime.Get_Time: TDateTime;
begin
  Result:= fTime;
end;

function TIsoTime.get_AsString: string;
begin
  Result:= sysutils.TimeToStr(fTime);
end;

procedure TIsoTime.set_AsInteger(const aValue: ptrint);
var
  H,M,S,L: word;
begin
  // 10.08.2006 new implementation, stored as seconds per day
  // formula: fIntTime = ((H*3600) + (M*60) + S)
  //          H = fIntTime div 3600; L = fIntTime mod 3600
  //          M = L div 60
  //          S = L mod 60
  if aValue <> fIntTime then begin
    fIntTime:= aValue;
    H:= fIntTime div 3600; L:= fIntTime mod 3600;
    M:= L div 60;
    S:= L mod 60;
    H:= H + fTimezone; { 28.02.2021 /bc }
    if H = 24 then H:= 0; { 09.03.2021 /bc }
    fTime:= EncodeTime(H,M,S,0);
  end;
end;

procedure TIsoTime.set_AsTime(const aValue: TDateTime);
var
  H,M,S,Dummy: word;
begin
  if aValue <> fTime then begin
    H:= 0; M:= 0; S:= 0; Dummy:= 0;
    DecodeTime(aValue,H,M,S,Dummy);
    H:= H + fTimezone; { 28.02.2021 /bc }
    if H = 24 then H:= 0; { 09.03.2021 /bc }
    fIntTime:= ((H*3600)+(M*60)+S);
    fTime:= EncodeTime(H,M,S,0);
//    fTime:= aValue
  end;
end;

{ added timezone var, defaults to 1 }
constructor TIsoTime.Create(aHours, aMinutes, aSeconds: word;aTimezone: integer = 1);
begin
  inherited Create;
  fTimezone:= aTimezone; { 28.02.2021 /bc }
  aHours:= aHours + fTimezone; { 28.02.2021 /bc }
  if aHours = 24 then aHours:= 0; { 09.03.2021 /bc }
  fIntTime:= ((aHours*3600)+(aMinutes*60)+aSeconds);
  fTime:= EncodeTime(aHours,aMinutes,aSeconds,0);
end;

constructor TIsoTime.Create(aTime: TDateTime;aTimezone: integer = 1);
begin
  inherited Create;
  fTimezone:= aTimezone; { 28.02.2021 /bc }
  set_AsTime(aTime);
end;

constructor TIsoTime.Create(aTime: ptrint;aTimezone: integer = 1);
begin
  inherited Create;
  fTimezone:= aTimezone; { 28.02.2021 /bc }
  set_AsInteger(aTime);
end;

destructor TIsoTime.Destroy;
begin
  inherited Destroy;
end;
procedure TIsoTime.set_AsString(const Value: string);
begin
  set_AsTime(sysutils.StrToTime(Value));
end;

procedure TIsoTime.set_Timezone(aValue: integer);
begin
  if fTimezone <> aValue then fTimezone:= aValue;
end;

{ included by bc_datetime.pas }
