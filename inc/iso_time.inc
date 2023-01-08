{%MainUnit bc_datetime.pas}
{ implementation: (c)cdbc 01.03.2021 /bc }

{ TIsoTime }
function TIsoTime.get_AsInteger: ptrint;
begin
  Result:= fIntTime; // 14.08.2022 new implementation, stored as milliseconds per day
end;

function TIsoTime.get_AsStringISO: string; { hh:mm:ss.mss }
var H,M,S,MSec: word;
begin
  DecodeTime(fTime,H,M,S,MSec);
  result:= bcToStr(H)+':'+
           bcToStr(M)+':'+
           bcToStr(S)+'.'+
           bcToStr(MSec);
end;

function TIsoTime.get_AsStringLong: string; { hh.mm.ss }
var H,M,S,MSec: word;
begin
  DecodeTime(fTime,H,M,S,MSec);
  result:= bcToStr(H) + '.' + bcToStr(M) + '.' + bcToStr(S);
end;

function TIsoTime.get_AsStringShort: string; { hh.mm }
var H,M,S,MSec: word;
begin
  DecodeTime(fTime,H,M,S,MSec);
  result:= bcToStr(H) + '.' + bcToStr(M);
end;

{ adds one hour according to summertime dates ~ }
function TIsoTime.get_DaylightSavingTime: integer;
begin
  Result:= DateIsInDST(fTime);
end;

{ adds or subtracts hours according to timezone, accepts negative values }
function TIsoTime.get_Timezone: integer;
begin
  Result:= fTimezone;
end;

function TIsoTime.get_Hours: word;
begin
  // 14.08.2022 new implementation, stored as milliseconds per day
  // formula: fIntTime = ((H*3600000) + (M*60000) + (S*1000) + MSec
  //          H = fIntTime div 3600000; L = fIntTime mod 3600000
  //          M = L div 60000; L = L mod 60000
  //          S = L div 1000
  //          MSec = L mod 1000
  Result:= fIntTime div 3600000;  //bm
end;

function TIsoTime.get_Minutes: word;
var L: ptrint;
begin
  // 10.08.2006 new implementation, stored as seconds per day
  // formula: fIntTime = ((H*3600000) + (M*60000) + (S*1000) + MSec
  //          H = fIntTime div 3600000; L = fIntTime mod 3600000
  //          M = L div 60000; L = L mod 60000
  //          S = L div 1000
  //          MSec = L mod 1000
  L:= fIntTime mod 3600000;                  { l = leftover from hour division }
  Result:= L div 60000;
end;

function TIsoTime.get_Seconds: word;
var L: ptrint;
begin
  // 10.08.2006 new implementation, stored as seconds per day
  // formula: fIntTime = ((H*3600000) + (M*60000) + (S*1000) + MSec
  //          H = fIntTime div 3600000; L = fIntTime mod 3600000
  //          M = L div 60000; L = L mod 60000
  //          S = L div 1000
  //          MSec = L mod 1000
  L:= fIntTime mod 3600000;                  { l = leftover from hour division }
  L:= L mod 60000;                        { l = leftover from minutes division }
  Result:= L div 1000;
end;

function TIsoTime.get_MilliSeconds: word;  //bm
var L: ptrint;
begin
  L:= fIntTime mod 3600000;                  { l = leftover from hour division }
  L:= L mod 60000;                        { l = leftover from minutes division }
  Result:= L mod 1000;               { result = leftover from seconds division }
end;

function TIsoTime.get_Time: TDateTime;
begin
  Result:= fTime;
end;

function TIsoTime.get_AsString: string; { hh.mm.ss }
begin
  Result:= get_AsStringLong;
end;

procedure TIsoTime.set_AsInteger(const aValue: ptrint); //bm
var
  H,M,S,MSec: word;
  L: cardinal; // leftover can be a pretty high number :-)
begin
  // 14.08.2022 new implementation, stored as milliseconds per day
  // formula: fIntTime = ((H*3600000) + (M*60000) + (S*1000) + MSec
  //          H = fIntTime div 3600000; L = fIntTime mod 3600000
  //          M = L div 60000; L = L mod 60000
  //          S = L div 1000
  //          MSec = L mod 1000
  if aValue <> fIntTime then begin
    fIntTime:= aValue;
    H:= fIntTime div 3600000; L:= fIntTime mod 3600000;
    M:= L div 60000; L:= L mod 60000;
    S:= L div 1000;
    MSec:= L mod 1000;
    H:= H + fTimezone; { denmark is utc + 1 28.02.2021 /bc }
    H:= H + DateIsInDST(Now); { will add 1, if it's daylight saving time 15.08.2022 /bc }
    case H of
      24: H:= 0;
      25: H:= 1
    end;
    fTime:= EncodeTime(H,M,S,MSec);
  end;
end;

procedure TIsoTime.set_AsStringISO(aValue: string); { hh:mm:ss.000 }
begin
  SetTimeRaw(bcStrToTime(aValue,STRISO,':'));
end;

procedure TIsoTime.set_AsStringLong(aValue: string); { hh.mm.ss }
begin
  SetTimeRaw(bcStrToTime(aValue,STRLONG,'.'));
end;

procedure TIsoTime.set_AsStringShort(aValue: string); { hh.mm }
begin
  SetTimeRaw(bcStrToTime(aValue,STRSHORT,'.'));
end;

procedure TIsoTime.set_AsTime(const aValue: TDateTime);
var
  H,M,S,MSec: word;
begin
  if aValue <> fTime then begin
    H:= 0; M:= 0; S:= 0; MSec:= 0;
    DecodeTime(aValue,H,M,S,MSec);
    H:= H + fTimezone; { 28.02.2021 /bc }
    H:= H + DateIsInDST(aValue); { will add 1, if it's daylight saving time 15.08.2022 /bc }
    case H of
      24: H:= 0;
      25: H:= 1
    end;
    fIntTime:= ((H*3600000)+(M*60000)+(S*1000)+MSec); { changed to include msecs //bm }
    fTime:= EncodeTime(H,M,S,MSec);
  end;
end;

procedure TIsoTime.SetTimeRaw(const aValue: TDateTime);
var
  H,M,S,MSec: word;
begin
  if aValue <> fTime then begin
    fTime:= aValue;
    H:= 0; M:= 0; S:= 0; MSec:= 0;
    DecodeTime(aValue,H,M,S,MSec);
    fIntTime:= ((H*3600000)+(M*60000)+(S*1000)+MSec); { changed to include msecs //bm }
  end;
end;

{ added timezone var, defaults to 1, added daylight saving time support }
constructor TIsoTime.Create(aHours, aMinutes, aSeconds, aMSecs: word;aTimezone: integer = 1);
begin
  inherited Create;
  fTimezone:= aTimezone; { 28.02.2021 /bc }
  aHours:= aHours + fTimezone; { 28.02.2021 /bc }
  aHours:= aHours + DateIsInDST(Now); { will add 1, if it's daylight saving time 15.08.2022 /bc }
  case aHours of
    24: aHours:= 0;
    25: aHours:= 1
  end;
  fIntTime:= ((aHours*3600000)+(aMinutes*60000)+(aSeconds*1000)+aMSecs);  //bm
  fTime:= EncodeTime(aHours,aMinutes,aSeconds,aMSecs);
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
  SetTimeRaw(bcStrToTime(Value,STRLONG,'.'));
end;

procedure TIsoTime.set_Timezone(aValue: integer);
begin
  if fTimezone <> aValue then fTimezone:= aValue;
end;

{ included by bc_datetime.pas }
