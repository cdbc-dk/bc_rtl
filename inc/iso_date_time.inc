{%MainUnit bc_datetime.pas}
{ implementation: (c)cdbc 01.03.2021 /bc }

{ *** TIsoDateTime *** }
function TIsoDateTime.get_Date: IIsoDate;
begin
  Result:= fDate;
end;

function TIsoDateTime.get_Time: IIsoTime;
begin
  Result:= fTime;
end;

function TIsoDateTime.get_AsString: string;
begin
  Result:= fDate.AsString+' - '+fTime.AsString;
end;

{ returns datetime as iso formatted string }
{ ISO 8601 represents date and time by starting with the year,
  followed by the month, the day, the hour, the minutes, seconds and milliseconds.
  ex: YYYY-MM-DD HH:MM:SS.SSS
  ex: 2020-07-10 15:00:00.000, represents the 10th of July 2020 at 3 p.m.
  (in local time as there is no time zone offset specified — more on that below).
}
function TIsoDateTime.get_AsISOString: string;
begin
  Result:= bcToStr(fDate.Year)+'-'+
           bcToStr(fDate.Month)+'-'+
           bcToStr(fDate.Day)+' '+
           bcToStr(fTime.Hours)+':'+
           bcToStr(fTime.Minutes)+':'+
           bcToStr(fTime.Seconds)+'.'+
           bcToStr(fTime.MilliSeconds);
end;

constructor TIsoDateTime.Create;
begin
  inherited Create;
  fDate:= TIsoDate.Create(now);
  fTime:= TIsoTime.Create(now);
end;

constructor TIsoDateTime.Create(const aDateTime: TDateTime);
begin
  inherited Create;
  fDate:= TIsoDate.Create(aDateTime);
  fTime:= TIsoTime.Create(aDateTime);
end;

destructor TIsoDateTime.Destroy;
begin
  fDate:= nil;
  fTime:= nil;
  inherited Destroy;
end;

procedure TIsoDateTime.SetDateTime(const aDateTime: TDateTime);
begin
  fDate.AsDate:= aDateTime;
  fTime.AsTime:= aDateTime;
end;

{ included by bc_datetime.pas }
