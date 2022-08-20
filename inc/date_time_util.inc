{%MainUnit bc_datetime.pas}
{ implementation: (c)cdbc 01.03.2021 /bc }

function bcDateToStr(const aDate: TDateTime): string;
begin // LongDateFormat: 'dd" "mmmm" "yyyy';
//  DateTimeToString(Result,FormatSettings.LongDateFormat,aDate,FormatSettings);
  DateTimeToString(Result,'dd"."mm"."yyyy',aDate,FormatSettings);
end;

{ * takes a date as integer param and converts it to a string representation * }
function bcIntDateToStr(const aDate: ptrint): string; { 06.05.2020 /bc }
var I: IIsoDate;
begin
  I:= TIsoDate.Create(aDate);
  Result:= I.AsString;
  I:= nil;
end;

function bcStrToDate(const aDateStr: string): TDateTime;
begin
//  function StrToDate(const S: AnsiString; separator : char): TDateTime;
  Result:= StrToDate(aDateStr,'.');
end;

function bcDateToWeekno(const aDate: TDateTime): ptrint;
var I: IIsoDate;
begin
  I:= TIsoDate.Create(aDate);
  Result:= I.ISOWeekNumber;
  I:= nil;
end;

{ * takes a datestr and works out the weeknumber it represents * }
function bcDateStrToWeekno(const aDateStr: string): ptrint;
var I: IIsoDate;
begin
  I:= TIsoDate.Create(aDateStr);
  Result:= I.ISOWeekNumber;
  I:= nil;
end;

{ * takes a datetime param and converts it to a ptrint representation * }
function bcDateToInt(const aDate: TDateTime): ptrint; { 06.05.2020 /bc }
var I: IIsoDate;
begin
  I:= TIsoDate.Create(aDate);
  Result:= I.AsInteger;
  I:= nil;
end;

{ takes a datetime param and converts it to a ptruint representation, milliseconds }
function bcTimeToIntRaw(aTime: TDateTime): ptruint;
var
  H, M, S, MSec: word;
begin
  H:= 0; M:= 0; S:= 0; MSec:= 0;
  DecodeTime(aTime,H,M,S,MSec);
  Result:= ((H*3600000)+(M*60000)+(S*1000)+MSec);
end;

{ * takes time as an integer param and converts it to a string representation * }
function bcIntTimeToStr(const aTime: ptrint): string;
var I: IIsoTime;
begin
  I:= TIsoTime.Create(aTime);
  Result:= I.AsString;
  I:= nil;
end;

function bcTimeToStr(const aTime: TDateTime): string; { 01.05.2020 /bc }
var I: IIsoTime;
begin
  I:= TIsoTime.Create(aTime);
  Result:= I.AsString;
  I:= nil;
end;

function bcDateTimeToStr(const aDateTime: TDateTime): string;
var I: IIsoDateTime;
begin
  I:= TIsoDateTime.Create(aDateTime);
  Result:= I.AsString;
  I:= nil;
end;

{ used for daylight saving time }
function LastSundayInMarch(aYear: word): TDateTime;
var Dt: TIsoDate;
begin
  Dt:= TIsoDate.Create(24,03,aYear);
  try
    while Dt.DayNumber <> 7 do Dt.IncrementDay(1);
    Result:= Dt.AsDate;
  finally Dt.Free; end;
end;

{ used for daylight saving time }
function LastSundayInOctober(aYear: word): TDateTime;
var I: IIsoDate;
begin
  I:= TIsoDate.Create(24,10,aYear);
  while I.DayNumber <> 7 do I.IncrementDay(1);
  Result:= I.AsDate;
  I:= nil;
end;

{ DateIsInDST returns 1 for daylight saving time, 0 for normal time 15.08.2022 /bc }
function DateIsInDST(const aDateTime: TDateTime): integer;     { one could use }
begin                                                   { DateTimeInRange from }
  Result:= 0;                                                  { DateUtils.inc }
  if (aDateTime >= LastSundayInMarch(YearOf(aDateTime))) and
     (aDateTime <= LastSundayInOctober(YearOf(aDateTime))) then Result:= 1;
end;

{ utility function, pads single digits with prefacing '0', if lower than 10 }
function bcToStr(anInt: ptruint): string;
begin
  if anInt < 10 then Result:= '0'+anInt.ToString    { tostring needs help with }
  else Result:= anInt.ToString;                     { single digit numbers :-P }
end;

{ included by bc_datetime.pas } 
