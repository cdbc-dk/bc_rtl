
{*************************************************************************$
$          Unit name : bc_datetime.pas                                    $
$          Copyright : cdbc.dk(C) 2006 - 2021                             $
$          Programmer: Benny Christensen                                  $
$          Created   : 2006.05.03 /bc                                     $
$          Updated   : 2014.01.06 /bc                                     $
$                    : 2020.03.26 /bc                                     $
$                    : 2020.03.30 /bc                                     $
$                    : 2020.04.28 /bc                                     $
$                    : 2020.05.01 /bc implemented utility functions       $
$                    : 2021.02.12 /bc implemented day,month,year-asstring $
$                    : 2021.02.28 /bc implemented timezone                $
$                    : 2021.03.01 /bc refactored to include files         $
$                    : 2021.03.09 /bc correction for midnight/timezone    $
$                    : 2022.11.12 /bc implemented bcstrtotime             $
$                    : 2022.11.18 /bc implemented bcDetectDateFormat      $
$                                                                         $
$************************************************************************ $
$          Purpose   :                                                    $
$          One date interface / class, which                              $
$          implements ISO weeknumber                                      $
$          calculations, as well as some                                  $
$          service functionality.                                         $
$          One time interface / class.                                    $
$          One IISoDateTime interface / class                             $
$          TODO: implement week-stuff                                     $
$*************************************************************************$
$          Licence   :                                                    $
$          "Beer License" - If you meet me one day, you buy me a beer :-) $
$          I'm NOT liable for anything! Use at your own risk!!!           $
$*************************************************************************}

unit bc_datetime;
interface
uses
  SysUtils,
  DateUtils;

const
  { version control }
  UnitVersion = '11.13.11.2022';
  { Danish day and month names }
  DayNames: array[1..7]of string = ('Mandag',
                                    'Tirsdag',
                                    'Onsdag',
                                    'Torsdag',
                                    'Fredag',
                                    'L�rdag',
                                    'S�ndag');
  MonthNames: array[1..12]of string = ('Januar',
                                       'Februar',
                                       'Marts',
                                       'April',
                                       'Maj',
                                       'Juni',
                                       'Juli',
                                       'August',
                                       'September',
                                       'Oktober',
                                       'November',
                                       'December');

  Year_Modifier  = $10000; { ~ 65536 }
  Month_Modifier = $100;   { ~ 256 }
  { time formats, used in bcstrtotime }
  STRSHORT = 0; { StrShort }
  STRLONG  = 1; { StrLong }
  STRISO   = 2; { StrIso }
  { date formats, used in bcdetectdateformat }
  DATE_DA  = 0; { danish dd.mm.yyyy }
  DATE_US  = 1; { us std. mm/dd/yyyy }
  DATE_ISO = 2; { iso spec. yyyy-mm-dd }
  DATE_STR = 3; { danish dd-mm-yyyy }
  { range specifiers, used in bciswithinrange }
  NOBOUND   = 0; { both boundries NOT included }
  LOWBOUND  = 1; { low boundry included }
  HIGHBOUND = 2; { high boundry included }
  BOTHBOUND = 3; { both boundries included }
  
type
  { Iso date interface, including ISOweeknumber/ISOYear calculations }
  IIsoDate = interface ['{B06A7CFC-326C-4454-8BA0-4F6AADDE4ADF}']
    function get_AsDate: TDateTime;
    function get_Day: word;
    function get_DayNumber: word;
    function get_IsoWeekNumber: word;
    function get_IsoYear: word;
    function get_Month: word;
    function get_Year: word;
    function get_AsInteger: ptrint;
    function get_AsString: string;
    function get_AsStringShort: string;
    function get_AsStringISO: string;
    function GetDayNumber(aDateTime: TDateTime): word;
    function GetDayName: string;
    function GetMonthName: string;
    function CompareTo(const aDate:IIsoDate;Descending: boolean): ptrint;
    procedure set_AsInteger(const aValue: ptrint);
    procedure set_AsString(const aValue: string);
    procedure set_AsStringShort(aValue: string);
    procedure set_AsStringISO(aValue: string);
    procedure set_AsDate(const Value: TDateTime);
    procedure IncrementDay(aNumberOfDays: word); { 15.08.2022 /bc }
    property Day: word read get_Day;
    property Month: word read get_Month;
    property Year: word read get_Year;
    property DayNumber: word read get_DayNumber;
    property DayName: string read getDayName;
    property MonthName: string read getMonthName;
    property ISOYear: word read get_IsoYear;
    property ISOWeekNumber: word read get_IsoWeekNumber;
    property AsDate: TDateTime read get_AsDate write set_AsDate;
    property AsInteger: ptrint read get_AsInteger write set_AsInteger;
    property AsString: string read get_AsString write set_AsString;
    property AsStringShort: string read get_AsStringShort write set_AsStringShort;
    property AsStringISO: string read get_AsStringISO  write set_AsStringISO;
  end;
  { iso time interface }
  IIsoTime = interface ['{A151913D-B90F-4CEE-BF6A-4FE870945E6E}']
    function get_AsInteger: ptrint;
    function get_AsStringISO: string;
    function get_AsStringLong: string;
    function get_AsStringShort: string; { hh:mm }
    function get_Hours: word;
    function get_Minutes: word;
    function get_Seconds: word;
    function get_MilliSeconds: word;
    function get_Time: TDateTime;
    function get_AsString: string;
    procedure set_AsInteger(const aValue: ptrint);
    procedure set_AsStringISO(aValue: string);
    procedure set_AsStringLong(aValue: string);
    procedure set_AsStringShort(aValue: string); { hh:mm }
    procedure set_AsTime(const aValue: TDateTime);
    procedure SetTimeRaw(const aValue: TDateTime);
    procedure set_AsString(const Value: string);
    property Hours: word read get_Hours;
    property Minutes: word read get_Minutes;
    property Seconds: word read get_Seconds;
    property MilliSeconds: word read get_MilliSeconds;
    property AsTime: TDateTime read get_Time write set_AsTime;
    property AsInteger: ptrint read get_AsInteger write set_AsInteger;
    { AsString defaults to AsStringLong returns time of format hh.mm.ss }
    property AsString: string read get_AsString write set_AsString;
    { AsStringShort returns time of format hh.mm }
    property AsStringShort: string read get_AsStringShort write set_AsStringShort;
    { AsStringLong returns time of format hh.mm.ss }
    property AsStringLong: string read get_AsStringLong write set_AsStringLong;
    { AsStringISO returns time of format hh:mm:ss.mss }
    property AsStringISO: string read get_AsStringISO write set_AsStringISO;
  end;
  { iso datetime interface }
  IIsoDateTime = interface ['{1A57E513-A9E0-413B-ABBE-830426AC3261}']
    function get_AsStringShort: string;
    function get_Date: IIsoDate;
    function get_Time: IIsoTime;
    function get_AsString: string;                     { �� 28.04.2020 /bc }
    function get_AsISOString: string;
    function GetDateTime: TDateTime;
    procedure SetDateTime(const aDateTime: TDateTime); { �� 28.04.2020 /bc }
    procedure set_AsStringShort(aValue: string);
    property Time: IIsoTime read get_Time;
    property Date: IIsoDate read get_Date;
    property AsString: string read get_AsString;       { �� 28.04.2020 /bc }
    property AsStringShort: string read get_AsStringShort write set_AsStringShort;
    property AsISOString: string read get_AsISOString {write set_AsISOString}; { bm 14.08.2022 /bc }
    property AsDateTime: TDateTime read GetDateTime write SetDateTime;
  end; { iisodatetime }
  { TIsoTime }
  TIsoTime = class(TInterfacedObject,IIsoTime)
  private  { 14.08.2022 new implementation, now stored as milliseconds per day }
    fIntTime: ptrint;
    fTimezone: integer;
    fTime: TDateTime;
    function get_AsInteger: ptrint;
    function get_AsStringISO: string;
    function get_AsStringLong: string;
    function get_AsStringShort: string; { hh:mm }
    function get_DaylightSavingTime: integer;  //bm
    function get_MilliSeconds: word;
    function get_Timezone: integer;
    function get_Hours: word;
    function get_Minutes: word;
    function get_Seconds: word;
    function get_Time: TDateTime;
    function get_AsString: string;
    procedure set_AsInteger(const aValue: ptrint);
    procedure set_AsStringISO(aValue: string);
    procedure set_AsStringLong(aValue: string);
    procedure set_AsStringShort(aValue: string); { hh:mm }
    procedure set_AsTime(const aValue: TDateTime);
    procedure SetTimeRaw(const aValue: TDateTime);
    procedure set_AsString(const Value: string);
    procedure set_Timezone(aValue: integer);
  public
    constructor Create(aHours,aMinutes,aSeconds,aMSecs: word;aTimezone: integer = 1); overload;
    constructor Create(aTime: TDateTime;aTimezone: integer = 1); overload;
    constructor Create(aTime: ptrint;aTimezone: integer = 1); overload;
    destructor Destroy; override;
    property Hours: word read get_Hours;
    property Minutes: word read get_Minutes;
    property Seconds: word read get_Seconds;
    property MilliSeconds: word read get_MilliSeconds;
    property AsTime: TDateTime read get_Time write set_AsTime;
    property AsInteger: ptrint read get_AsInteger write set_AsInteger;
    { AsString defaults to AsStringLong returns time of format hh.mm.ss }
    property AsString: string read get_AsString write set_AsString;
    { AsStringShort returns time of format hh.mm }
    property AsStringShort: string read get_AsStringShort write set_AsStringShort;
    { AsStringLong returns time of format hh.mm.ss }
    property AsStringLong: string read get_AsStringLong write set_AsStringLong;
    { AsStringISO returns time of format hh:mm:ss.mss }
    property AsStringISO: string read get_AsStringISO write set_AsStringISO;
    property Timezone: integer read get_Timezone write set_Timezone;
    property DaylightSavingTime: integer read get_DaylightSavingTime;
  end;
  { TIsoDate }
  TIsoDate = class(TInterfacedObject,IIsoDate)
  private
    fDay,fMonth,fYear,fDayNumber,fIsoYear,fIsoWeekNumber: word;
    fIsoWeekOneStartDate,fDate: TDateTime;
    function get_AsDate: TDateTime;
    function get_AsStringISO: string;
    function get_AsStringShort: string;
    function get_Day: word;
    function get_DayAsString: string;
    function Get_DayNumber: word;
    function Get_IsoWeekNumber: word;
    function get_WeekNumberAsString: string;
    function Get_IsoYear: word;
    function Get_Month: word;
    function get_MonthAsString: string;
    function Get_Year: word;
    function get_YearAsString: string;
    function get_YearAsStringShort: string;
    function get_AsInteger: ptrint;
    function get_AsString: string;
    function GetDayNumber(aDateTime: TDateTime): word;
    function GetDayName: string;
    function GetIsoWeekOne(const Year: word): TDateTime;
    function GetIsoWeek(aDateTime: TDateTime): word;
    function GetMonthName: string;
    function CompareTo(const aDate:IIsoDate;Descending: boolean): ptrint;    
    procedure set_AsInteger(const aValue: ptrint);
    procedure set_AsString(const aValue: string);
    procedure set_AsDate(const Value: TDateTime);
    procedure set_AsStringISO(aValue: string);
    procedure set_AsStringShort(aValue: string);
  public
    constructor Create(aDay,aMonth,aYear: word); overload;
    constructor Create(aDate: TDateTime); overload;
    constructor Create(aDate: ptrint); overload;
    constructor Create(aDateStr: string); overload; { 05.01.2014 /bc }
    destructor Destroy; override;
    procedure IncrementDay(aNumberOfDays: word); { 15.08.2022 /bc }
    property Day: word read get_Day;
    property DayAsString: string read get_DayAsString;
    property Month: word read get_Month;
    property MonthAsString: string read get_MonthAsString;
    property Year: word read get_Year;
    property YearAsString: string read get_YearAsString;
    property DayNumber: word read Get_DayNumber; { of week, 1..7, according to Iso-spec }
    property DayName: string read GetDayName; { returns the danish name of the weekday }
    property MonthName: string read GetMonthName;
    property ISOYear: word read Get_IsoYear;
    property ISOWeekNumber: word read Get_IsoWeekNumber;
    property WeekNumberAsString: string read get_WeekNumberAsString;
    property AsDate: TDateTime read get_AsDate write set_AsDate;
    property AsInteger: ptrint read get_AsInteger write set_AsInteger;
    { returns the date as a string, format = 'dd.mm.yyyy' }
    property AsString: string read get_AsString write set_AsString;
    { returns the date as a shortstring, format = 'dd.mm.yy' }
    property AsStringShort: string read get_AsStringShort write set_AsStringShort;
    property AsStringISO: string read get_AsStringISO  write set_AsStringISO;
  end;
{ TIsoDateTime }
  TIsoDateTime = class(TInterfacedObject,IIsoDateTime)
  private
    fDate: IIsoDate;
    fTime: IIsoTime;
    function get_AsStringShort: string;
    procedure set_AsStringShort(aValue: string);
  protected
    function get_Date: IIsoDate;
    function get_Time: IIsoTime;
    function get_AsString: string;                     { �� 28.04.2020 /bc }
    function get_AsISOString: string;
  public
    constructor Create; overload; { defaults to "now" }
    constructor Create(const aDateTime: TDateTime); overload;
    destructor Destroy; override;
    function GetDateTime: TDateTime;
    procedure SetDateTime(const aDateTime: TDateTime); { �� 28.04.2020 /bc }
    property Date: IIsoDate read get_Date;
    property Time: IIsoTime read get_Time;
    property AsString: string read get_AsString;       { �� 28.04.2020 /bc }
    property AsStringShort: string read get_AsStringShort write set_AsStringShort;
    property AsISOString: string read get_AsISOString {write set_AsISOString}; { bm 14.08.2022 /bc }
    property AsDateTime: TDateTime read GetDateTime write SetDateTime;
  end;

{ utility functions }
function bcDateToStr(const aDate: TDateTime): string;
function bcIntDateToStr(const aDate: ptrint): string; { 06.05.2020 /bc }
{ bcDetectDateFormat tries to ascertain the date format ~ dd.mm.yyyy, mm/dd/yyyy or yyyy-mm-dd,
  returns a word identifier 0 = dd.mm.yyyy .. 2 = yyyy-mm-dd ~ DATE_DA, DATE_US or DATE_ISO }
function bcDetectDateFormat(const aStr: string): word;
{ bcStrToDate #1 takes 2 params, the string to convert and the format,
  defaults to DATE_DA ~ dd.mm.yyyy }
function bcStrToDate(const aDateStr: string;aFormat: word = DATE_DA): TDateTime; overload;
{ bcStrToDate #2 takes 3 params, the string to convert, the  format ~ DATE_DA,
  DATE_US & DATE_ISO and the type ~ STRSHORT, STRLONG & STRISO, returns a TDateTime }
function bcStrToDate(const aDateStr: string;aFormat,aType: word): TDateTime; overload;
function bcDateToWeekno(const aDate: TDateTime): ptrint;
function bcDateStrToWeekno(const aDateStr: string): ptrint;
function bcDateToInt(const aDate: TDateTime): ptrint; { 01.05.2020 /bc }
function bcTimeToIntRaw(aTime: TDateTime): ptruint; { no timezone & no dst }
function bcIntTimeToStr(const aTime: ptrint): string;
function bcTimeToStr(const aTime: TDateTime): string; { 01.05.2020 /bc }
{ bcStrToTime converts a string into time, depending on format, separator is just conveinience }
function bcStrToTime(const aStr: string;aFormat: word;aSeparator: char): TDateTime;
function bcDateTimeToStr(const aDateTime: TDateTime): string; { 18.02.2021 /bc }
{ bcDateTimeToISOStr converts a datetime to a string complying with ISO specification }
function bcDateTimeToISOStr(const aDateTime: TDateTime): string;
function LastSundayInMarch(aYear: word): TDateTime; { used for dst calculations 15.08.2022 /bc }
function LastSundayInOctober(aYear: word): TDateTime; { used for dst calculations 15.08.2022 /bc }
{ DateIsInDST returns 1 for daylight saving time, 0 for normal time 15.08.2022 /bc }
function DateIsInDST(const aDateTime: TDateTime): integer;
{ ToString doesn't pad single digits, this does ~ 1 -> 01 }
function bcToStr(anInt: ptruint): string; { pad single digits 14.08.2022 /bc }
{ bcIsWithinRange checks if a value is between two boundries ~ ie. within range,
  inclusion param determines if no,low,high or both boundries are included,
  inclusion constants are: "NOBOUND", "LOWBOUND", "HIGHBOUND" & "BOTHBOUND" }
function bcIsWithinRange(aLowBoundry,anItem,aHighBoundry: ptrint;Inclusion: word = BOTHBOUND): boolean;

implementation
uses bc_advstring;
{ refactored 01.03.2021 /bc }
{$i iso_date.inc}       { moved the implementation part to an include-file }
{$i iso_time.inc}       { moved the implementation part to an include-file }
{$i iso_date_time.inc}  { moved the implementation part to an include-file }
{$i date_time_util.inc} { moved the implementation part to an include-file }

end.
 
