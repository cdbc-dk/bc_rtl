{%MainUnit bc_datetime.pas}
{ implementation: (c)cdbc 01.03.2021 /bc }

{
First, we need to review what the ISO week number is. According to the ISO
(International Standards Organization) in document ISO 8601, an ISO week
starts on a Monday (which is counted as day 1 of the week), and week 1
for a given year is the week that contains the first Thursday in the year.

Calculating the date of ISO week 1 for a given year
If you play around with the numbers, you'll see how to calculate the date
of the Monday for week 1. The first Thursday of the year will be either
the 1st, 2nd, all the way up to the 7th of January.

If it were the 1st, week 1 will start on the 29-Dec of the previous year
(yes, this is correct: the ISO week 1 for a given year may have dates from the previous year);
if the 2nd, week 1 will start on 30-Dec of the previous year;
if the 3rd, 31-Dec; if the 4th, 1-Jan; if the 5th, 2-Jan; if the 6th, 3-Jan;
and finally if the first Thursday were the 7th, week 1 would start on 4-Jan.

However, calculating the date of the first Thursday is hard. Well,
not hard, but complicated. A better way is to see that the ISO week
is so defined that the 4-Jan of every year is in week 1. In other words,
that the first week must contain four or more days from the year
(if 1-Jan were a Thursday, 4-Jan would be the Sunday, and hence would form week 1).
So we calculate 4-Jan and work out which day of the week it is.
If it's Thursday, week 1 starts three days earlier,
if Friday, four days earlier, if Saturday, five days earlier,
if Sunday, six days earlier. If it's a Monday, we found the week 1 start date straight away;
if Tuesday, week 1 starts one day earlier, if Wednesday, two days earlier.
}
// get the date for monday in week 1
function TIsoDate.GetIsoWeekOne(const Year: word): TDateTime;
var
  aDayNumber: word;
begin // design by contract!
  if (Year < 1899) or (Year > 2099) then
    raise Exception.Create('Error - TIsoDate.GetIsoWeekOne:'+#13#10+
                           '  Year is out of range! Must be 1899 < Year < 2099.');
  Result:= SysUtils.EncodeDate(Year,1,4);
  // get the ISO day number for this date 1 = Monday, 7 = Sunday
  aDayNumber:= (SysUtils.DayOfWeek(Result)-1);
  // 0 = Sunday, 6 = Saturday ~ convert to ISO representation
  if aDayNumber = 0 then aDayNumber:= 7;
  // return the date of the Monday that is less than or equal to this date
  case aDayNumber of
    1: ;// monday, found it, do nothing, we've got the date already
    2: Result:= SysUtils.EncodeDate(Year,1,3); // tuesday, week 1 starts 1 day earlier
    3: Result:= SysUtils.EncodeDate(Year,1,2); // wednesday, week 1 starts 2 days earlier
    4: Result:= SysUtils.EncodeDate(Year,1,1); // thursday, week 1 starts 3 days earlier
    5: Result:= SysUtils.EncodeDate(Year-1,12,31); // friday, week one starts four days earlier, ie. last year
    6: Result:= SysUtils.EncodeDate(Year-1,12,30); // saturday, week one starts five days earlier, ie. last year
    7: Result:= SysUtils.EncodeDate(Year-1,12,29); // sunday, week one starts six days earlier, ie. last year
  end; // case
end; { GetIsoWeekOne }

function TIsoDate.GetDayNumber(aDateTime: TDateTime): word;
begin
  Result:= (SysUtils.DayOfWeek(aDateTime)-1);
  if Result = 0 then Result:= 7; // convert sunday from 0 to 7, ISO-spec
end;

{------------------ date calculations ----- 13.02.2007 / bc -------------------}
  { d = no scale, m = scale 256, y = scale 65536 }

  (* const
       Year_Modifier  = $10000; { ~ 65536 }
       Month_Modifier = $100;   { ~ 256 } *)

  { formula -> ptrint = ((Y * 65536) + (M * 256) + D) }
  { formula -> Y = ptrint div 65536; Rem = ptrint mod 65536; }
  {            M = Rem div 256; D = Rem mod 256; }
{------------------ date calculations ----- 13.02.2007 / bc -------------------}

function TIsoDate.get_AsInteger: ptrint;
begin
  Result:= ((fYear * Year_Modifier) + (fMonth * Month_Modifier) + fDay);
end;

function TIsoDate.get_AsString: string;
begin
//  Result:= datetostr(fDate); { changed 05.01.2014 /bc }
  DateTimeToString(Result,'dd"."mm"."yyyy',fDate,FormatSettings);
end;

function TIsoDate.Get_Date: TDateTime;
begin
  Result:= fDate;
end;

function TIsoDate.Get_Day: word;
begin
  Result:= word(fDay);
end;

function TIsoDate.get_DayAsString: string;
begin
  Result:= fDay.ToString;
end;

function TIsoDate.Get_DayNumber: word;
begin
  Result:= fDayNumber;
end;

function TIsoDate.Get_IsoWeekNumber: word;
begin
  Result:= fIsoWeekNumber;
end;

function TIsoDate.get_WeekNumberAsString: string;
begin
  Result:= fIsoWeekNumber.ToString;
end;

function TIsoDate.Get_IsoYear: word;
begin
  Result:= fIsoYear;
end;

function TIsoDate.Get_Month: word;
begin
  Result:= word(fMonth);
end;

function TIsoDate.get_MonthAsString: string;
begin
  Result:= fMonth.ToString;
end;

function TIsoDate.Get_Year: word;
begin
  Result:= word(fYear);
end;

function TIsoDate.get_YearAsString: string;
begin
  Result:= fYear.ToString;
end;

function TIsoDate.GetDayName: string;
begin
  Result:= DayNames[fDayNumber];
end;

{There are a couple of things to point out right away, I think.
First is that 29-Dec, 30-Dec, and 31-Dec of a given year could actually
be in the first week of the succeeding year, and second is that 1-Jan, 2-Jan,
3-Jan of a given year could be in the last week of the previous year.

Apart from those exceptional 6 days, it's pretty easy to calculate
the week number for a given day: calculate the Monday of week 1 in the same year,
subtract it from the date you're given to get the number of days in between,
divide this by 7 (discarding the remainder) and add 1. The result is the week number.

Let's illustrate with a concrete example. This year (2003),
week 1 started on 30-Dec-2002 (the first Thursday of 2003 was 2-Jan).
Say we were trying to calculate the week number for Mon 3-Feb.
Subtracting 30-Dec-2002 from 3-Feb-2003 gives 35 days. Divide this by 7 gives 5.
Add 1 to give 6. 3-Feb is thus in week 6. By looking at a diary or a calendar,
you can verify that this is correct. Another test: Sun 2-Feb. The difference in days is 34.
Divide by 7 (discarding the remainder) gives 4. Add 1 to give us the answer
that Sun 2-Feb is in week 5. The previous test will show us that this is true:
the Sunday prior to a Monday is in a previous week.

Calculating the ISO week for a hard date
Having solved the problem for 359 (or 360) days of the year,
we should now solve it for the 6 problematic days.
(In other words a 98% success rate for an algorithm isn't good enough <g>.)
Let's look at the case of 1-Jan in depth. We need to see if it's counted
as being in the previous year, so we calculate when week 1 of this year starts.
If week 1 starts after 1-Jan then obviously 1-Jan is going to appear in the last week
of the previous year. So we calculate the start date for week 1 of the previous year,
subtract it from 1-Jan, divide by 7 and add 1. Obviously this algorithm
will also work for 2-Jan and 3-Jan.

Next up, let's think about 31-Dec. This may appear in the first week of the following year.
So calculate the start of week 1 of the following year. If 31-Dec is less than this,
it will be in the last week of its year, and we'll use the standard algorithm
to calculate it. If 31-Dec is greater than or equal to than the start of week 1
of the following year, it's obviously in that week.
Notice that we initially need to calculate week 1 of the following year for 31-Dec,
not week 1 of the current year. This is different from all the other dates.
(Of course, the same argument and algorithm will apply for 29-Dec and 30-Dec.)
Implementing the ISO week calculation}
function TIsoDate.GetIsoWeek(aDateTime: TDateTime): word;
var
  Week1,Temp: TDateTime;
  aIsoYear,Dd,Mm: word;
begin
  // init
  aIsoYear:= 0; Dd:= 0; Mm:= 0;
  // first get a hold of the year we're dealing with
  SysUtils.DecodeDate(aDateTime,aIsoYear,Mm,Dd);
  // then get a temporary date to work with, test for special dates
  Temp:= SysUtils.EncodeDate(aIsoYear,12,29);
  if aDateTime >= Temp then begin
    Week1:= GetIsoWeekOne(aIsoYear+1);
    if aDateTime < Week1 then Week1:= GetIsoWeekOne(aIsoYear)
    else inc(aIsoYear);
  end else begin
    Week1:= GetIsoWeekOne(aIsoYear);
    if aDateTime < Week1 then begin
      dec(aIsoYear);
      Week1:= GetIsoWeekOne(aIsoYear);
    end;
  end;
  fIsoYear:= aIsoYear;
  // get the number days between day1 in week1 and supplied date, divide by 7 and add 1
  Dd:= round(aDateTime) - round(Week1);
  fIsoWeekNumber:= (Dd div 7) + 1;
  Result:= fIsoWeekNumber; // !!! beware of Year !!!
end; { GetIsoWeek }

function TIsoDate.GetMonthName: string;
begin
  Result:= MonthNames[fMonth];
end;

{------------------ date calculations ----- 13.02.2007 / bc -------------------}
  { d = no scale, m = scale 256, y = scale 65536 }

  (* const
       Year_Modifier  = $10000; { ~ 65536 }
       Month_Modifier = $100;   { ~ 256 } *)

  { formula -> ptrint = ((Y * 65536) + (M * 256) + D) }
  { formula -> Y = ptrint div 65536; Rem = ptrint mod 65536; }
  {            M = Rem div 256; D = Rem mod 256; }
{------------------ date calculations ----- 13.02.2007 / bc -------------------}

procedure TIsoDate.set_AsInteger(const AValue: ptrint);
var
  Rem: ptrint; // remainder
begin
  fYear:= 0; fMonth:= 0; fDay:= 0;             { 1. initialize values first }
  fYear:= AValue div Year_Modifier;            { 2. then extract year-part }
  Rem:= AValue mod Year_Modifier;              { 3. then save what remains }
  fMonth:= Rem div Month_Modifier;             { 4. then extract month-part }
  fDay:= Rem mod Month_Modifier;               { 5. then extract day-part }
  fDate:= EncodeDate(word(fYear),
                     word(fMonth),
                     word(fDay));              { 6. then encode dateobject }
  fDayNumber:= GetDayNumber(fDate);            { 7. then calculate daynumber }
  fIsoWeekOneStartDate:= GetIsoWeekOne(fYear); { 8. then calculate 1st isoweek }
  GetIsoWeek(fDate);                           { 9. last calculate actual weeknumber }
end;

procedure TIsoDate.set_AsString(const aValue: string);
var D: TDateTime;
begin
  D:= StrToDate(aValue,'.');
  Set_Date(D);
end;

procedure TIsoDate.set_Date(const Value: TDateTime);
begin
  if Value <> fDate then begin
    fDate:= Value;
    DecodeDate(fDate,fYear,fMonth,fDay);
    fDayNumber:= GetDayNumber(fDate);
    fIsoWeekOneStartDate:= GetIsoWeekOne(fYear);
    GetIsoWeek(fDate);
  end;
end;

constructor TIsoDate.Create(aDay,aMonth,aYear: word);
begin
  inherited Create;
  fDay:= aDay;
  fMonth:= aMonth;
  fYear:= aYear;
  fDate:= EncodeDate(aYear,aMonth,aDay);
  fDayNumber:= GetDayNumber(fDate);
  fIsoWeekOneStartDate:= GetIsoWeekOne(aYear);
  GetIsoWeek(fDate);
end;

constructor TIsoDate.Create(aDate: TDateTime);
begin
  inherited Create;
  fDate:= aDate;
  DecodeDate(aDate,fYear,fMonth,fDay);
  fDayNumber:= GetDayNumber(fDate);
  fIsoWeekOneStartDate:= GetIsoWeekOne(fYear);
  GetIsoWeek(fDate);
end;

constructor TIsoDate.Create(aDate: ptrint);
begin
  inherited Create;
  set_AsInteger(aDate);
end;

constructor TIsoDate.Create(aDateStr: string);
begin
  inherited Create;
  set_AsString(aDateStr);
end;

destructor TIsoDate.Destroy;
begin
  inherited Destroy;
end;

{ helper functions }
function Word2Byte(const W:word): byte;
begin
  Result:= 0;
  try
    if W < 127 then Result:= byte(W);
  except raise Exception.Create('Word2Byte: Error, value out of byte range!'); end;
end;

function TIsoDate.CompareTo(const aDate: IIsoDate;Descending: boolean): ptrint;
begin
  if fDate < aDate.AsDate then begin
    if Descending then Result:= 1 else Result:= -1;
  end
  else if fDate = aDate.AsDate then Result:= 0
  else if Descending then Result:= -1 else Result:= 1;
end;

{ included by bc_datetime.pas }
