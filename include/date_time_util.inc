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

{ included by bc_datetime.pas }
