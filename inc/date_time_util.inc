{%MainUnit bc_datetime.pas}
{ implementation: (c)cdbc 01.03.2021 /bc }

function bcDateToStr(const aDate: TDateTime): string;
var
  S,Res: ansistring;
begin // LongDateFormat: 'dd" "mmmm" "yyyy';
//  DateTimeToString(Result,FormatSettings.LongDateFormat,aDate,FormatSettings);
  S:= 'dd"."mm"."yyyy';
  DateTimeToString(Res,S,aDate,FormatSettings); { this compiles with or without longstrings on $H+ }
  Result:= Res;
end;

{ * takes a date as integer param and converts it to a string representation * }
function bcIntDateToStr(const aDate: ptrint): string; { 06.05.2020 /bc }
var I: IIsoDate;
begin
  I:= TIsoDate.Create(aDate);
  Result:= I.AsString;
  I:= nil;
end;

{ bcDetectDateFormat tries to ascertain the date format ~ dd.mm.yyyy, mm/dd/yyyy or yyyy-mm-dd,
  returns a word identifier 0 = dd.mm.yyyy .. 2 = yyyy-mm-dd ~ DATE_DA, DATE_US or DATE_ISO }
function bcDetectDateFormat(const aStr: string): word;
var
  I: integer;
begin
  for I:= 1 to Length(aStr) do case aStr[I] of
    '.': begin Result:= DATE_DA; break; end; //0
    '/': begin Result:= DATE_US; break; end;  //1
    '-': begin Result:= DATE_ISO; break; end;  //2
  end;
end;

function bcStrToDate(const aDateStr: string;aFormat: word = DATE_DA): TDateTime;
begin
//  function StrToDate(const S: AnsiString; separator : char): TDateTime;
  case aFormat of
    DATE_DA : Result:= StrToDate(aDateStr,'.');
    DATE_STR: Result:= StrToDate(aDateStr,'-');
  end;
end; { bcStrToDate #1 }

function bcStrToDate(const aDateStr: string; aFormat, aType: word): TDateTime;
var
  P,PE: pchar;
  L,Fc,Y,M,D: word;
  Buf: string;
  C: char;
begin
  if aDateStr = '' then exit;
  case aFormat of
    DATE_DA : C:= '.';
    DATE_US : C:= '/';
    DATE_ISO: C:= '-';
    DATE_STR: C:= '-'; { extra for numbnuts }
  end;
  L:= Length(aDateStr);
  P:= pchar(aDateStr); PE:= P + L; Buf:= ''; Fc:= 0;
  while P <= PE do begin
    if ((P^ = C) or (P = PE)) then begin
      case Fc of
        0: begin
             case aFormat of
               DATE_DA,DATE_STR : D:= Buf.ToInteger;
               DATE_US : M:= Buf.ToInteger;
               DATE_ISO: Y:= Buf.ToInteger;
             end;
             Buf:= '';
             inc(Fc); // 1
           end; { Fc = 0 }
        1: begin
             case aFormat of
               DATE_DA,DATE_STR : M:= Buf.ToInteger;
               DATE_US : D:= Buf.ToInteger;
               DATE_ISO: M:= Buf.ToInteger;
             end;
             Buf:= '';
             inc(Fc); // 2
           end; { Fc = 1 }
        2: begin
             case aFormat of
               DATE_DA,DATE_STR : Y:= Buf.ToInteger;
               DATE_US : Y:= Buf.ToInteger;
               DATE_ISO: D:= Buf.ToInteger;
             end;
             Buf:= '';
             inc(Fc); // 3
           end; { Fc = 2 }
      end; { case fc of }
    end; { P^=C }
    if P^ in ['0'..'9'] then Buf+= P^;
    inc(P);
  end; { while }
  { cater for DATE_STR ~ NumbNuts using '-' in DATE_DA }
  if aFormat = DATE_ISO then begin { iso or idiots at play }
    if D > 31 then begin { idiots at play }
      L:= Y;
      Y:= D;  { switch'a'roo }
      D:= L;
    end;
  end;
  { cater for short dates }
  if aType = STRSHORT then if Y < ((YearOf(now)-2000)+5) then Y+= 2000 else Y+= 1900; { lookahead = 5 years }
  Result:= EncodeDate(Y,M,D);
end; { bcStrToDate #2 }

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

function bcStrToTime(const aStr: string; aFormat: word; aSeparator: char): TDateTime;
var
  H,M,S,MSec: word;
  Tm,Tmp: string;
  FullIso: boolean;
  C: char;
begin
  if aStr = '' then exit;
  FullIso:= false; { check if date is part of iso-string }
  for C in aStr do if C in ['-','/',' '] then begin
    FullIso:= true;
    break;
  end;
  case aFormat of
    0: begin { StrShort ~ 22.21 }
         H:= bcGetFieldToken(1,aStr,aSeparator).ToInteger;
         M:= bcGetFieldToken(2,aStr,aSeparator).ToInteger;
         S:= 0; MSec:= 0;
       end;
    1: begin { StrLong ~ 22.21.20 }
         Tm:= copy(trim(aStr),1,8);
         H:= bcGetFieldToken(1,Tm,aSeparator).ToInteger;
         M:= bcGetFieldToken(2,Tm,aSeparator).ToInteger;
         if Length(aStr) > 5 then S:= bcGetFieldToken(3,Tm,aSeparator).ToInteger
         else S:= 0;
         if Length(aStr) = 12 then MSec:= bcGetFieldToken(2,aStr,',').ToInteger
         else MSec:= 0;
       end;
    2: begin { StrIso ~ 11:12:13.140}
         case FullIso of
           false: begin { just time part ~ 18:19:20.365 }
                    Tm:= bcGetFieldToken(1,aStr,'.'); { get time up till .msec }
                    MSec:= bcGetFieldToken(2,aStr,'.').ToInteger;
                    H:= bcGetFieldToken(1,Tm,aSeparator).ToInteger;
                    M:= bcGetFieldToken(2,Tm,aSeparator).ToInteger;
                    S:= bcGetFieldToken(3,Tm,aSeparator).ToInteger;
                  end;
           true:  begin { full iso string ~ 1970-01-01 18:19:20.365 }
                    Tm:= bcGetFieldToken(2,aStr,' ');             { get time part }
                    MSec:= bcGetFieldToken(2,Tm,'.').ToInteger; { get msecs }
                    Tmp:= bcGetFieldToken(1,Tm,'.');  { get rest of time lest msec }
                    H:= bcGetFieldToken(1,Tmp,aSeparator).ToInteger;
                    M:= bcGetFieldToken(2,Tmp,aSeparator).ToInteger;
                    S:= bcGetFieldToken(3,Tmp,aSeparator).ToInteger;
                  end;
         end;
       end;
  end;
  Result:= EncodeTime(H,M,S,MSec);
end;

function bcDateTimeToStr(const aDateTime: TDateTime): string;
var I: IIsoDateTime;
begin
  I:= TIsoDateTime.Create(aDateTime);
  Result:= I.AsString;
  I:= nil;
end; { bcDateTimeToStr }

function bcDateTimeToISOStr(const aDateTime: TDateTime): string;
begin
  with TIsoDateTime.Create(aDateTime) do begin
    Result:= AsISOString;
  end;
end; { bcDateTimeToISOStr }

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
     (aDateTime < LastSundayInOctober(YearOf(aDateTime))) then Result:= 1;
end;

{ utility function, pads single digits with prefacing '0', if lower than 10 }
function bcToStr(anInt: ptruint): string;
begin
  if anInt < 10 then Result:= '0'+anInt.ToString    { tostring needs help with }
  else Result:= anInt.ToString;                     { single digit numbers :-P }
end;

{ bcIsWithinRange checks if a value is between two boundries ~ ie. within range, both boundries inclusive }
function bcIsWithinRange(aLowBoundry, anItem, aHighBoundry: ptrint;Inclusion: word = BOTHBOUND): boolean;
begin
  case Inclusion of
    NOBOUND: Result:= ((anItem > aLowBoundry) and (anItem < aHighBoundry));
    LOWBOUND: Result:= ((anItem >= aLowBoundry) and (anItem < aHighBoundry));
    HIGHBOUND: Result:= ((anItem > aLowBoundry) and (anItem <= aHighBoundry));
    BOTHBOUND: Result:= ((anItem >= aLowBoundry) and (anItem <= aHighBoundry));
  end;
end;

{ included by bc_datetime.pas } 
