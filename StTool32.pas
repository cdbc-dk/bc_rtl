
                   {**************************************$
                   $  Unit name : StTool32.pas            $
                   $  Copyright : Creative Development(C) $
                   $  Programmer: Benny Christensen       $
                   $  Created   : May 1999  /bc           $
                   $  Updated   : 2001-09-26/bc           $
                   $ ************************************ $
                   $  Purpose   :                         $
                   $  Toolbox for working with strings,   $
                   $  conversions and textfiles in        $
                   $  Object Pascal.                      $
                   $**************************************}

unit StTool32;
interface
uses SysUtils,Classes,Windows,Global,Person,Crypt;
{ functions }
function Min(I1,I2: integer): integer;
function Max(I1,I2: integer): integer;
function StrUpCase(const S: string): string;
function StrLowCase(const S: string): string;
function CharUpCase(Ch: char): char;
function CharLowCase(Ch: char): char;
function Getmesline32(Lineno: longint; Filename: string) : string;
function GetField(No: integer; S:string): string;
function GetFieldToken(No: integer; S:string; Separator: char): string;
function GetFirstWord(var S : string) : string;
function DeleteFirstWord(S : string) : string;
function DeleteFirstField(S : string) : string;
function GetLastWord(S : string) : string;
function GetLastWordToken(S:string;Token:char): string;
function GetLastWordSh(S : shortstring): shortstring;
function DeleteLastWord(S : string) : string;
function DeleteLastWordToken(S:string;Token:char): string;
function SwitchChar(O,N: Char; S: string): string;
function ValidateCpr(const Cpr: string): boolean;
function MakeStringNumeric(S: string): string;
function RemoveCh0(S: shortstring):shortstring;
function RemoveChSh(Ch: char;S: shortstring):shortstring;
function RemoveCh(Ch: char;S: string):string;
function RemoveStr(const SubStr,S: string):string;
function RemoveCtrlChars(const S: string): string;
function FoundInString(SubStr,S: string): boolean;
function PosApart(Token: char;CharsBetween: integer;const S: string): boolean;
function PosApartMaxLen(Token: char;CharsBetween: integer;const S: string;MaxLen: integer): boolean;
function SubstituteStr(OldS,NewS,S: string):string;
function CodeLine2CodeString(ACodeLine: TCodeLine): string;
function CodeString2CodeLine(ACodeString: string): TCodeLine;
function CompareDates(Date1,Date2: TDate): integer;
function DeleteFirstFieldToken(S:string;Token:char): string;
function GetFirstFieldToken(var S:string;Token:char): string;
function GetCityFromZipCode(Zip:string): string; { depends upon c:\eme\ud\conv\postno }
function GetZipCodeFromCity(City:string): string;
function LastSpaceInStr(S:string): integer;
function LastTokenInStr(Token:char;S:string): integer;
function GetFirstStrUpCase(var S:string): string;
function GetFirstUpCaseChar(const S:string): Integer;
function GetFirstDigit(const S:string): Integer;
function GetFirstDigitStr(const S:string): string;
function GetFirstFloatStr(const S:string): string;
function EmEncrypt(Line: string): string;
function EmDecrypt(Line: string): string;
function CalTimeApart(Time1,Time2: string): integer;
function StringIsNumeric(const S:string): boolean;
function SubStrIndexInList(const SubStr: string;Index: integer;const List: TStrings): integer;
function GetLastChar(const S: string): char;
function FileCopy(Source, Dest: string): boolean;
function Month2NumStr(const S:string): TSt2;
function Str2AbsTime(S: TSt5): integer;   { takes format "10.50" }
function dB2Hex(dB:integer): TSt2;{ takes an integer and converts it to hexstr }
function Hex2Num(N:TSt2): byte;
function TrueDate2EmarDate(const S: string): string;
function LabDate2LongDate(const S: string): string; { format ddmmyy -> dd.mm.yyyy }
function CalDate2LongDate(const S: string): string; { format yymmdd -> dd.mm.yyyy }
function DosCp2WinCpF(const S: string): string;
function WinCp2DosCpF(const S: string): string;
function CreateNewname(const Filename,OldPath,NewPath: string): string;
function StrIsDate(const S: string): boolean;
function StrIsDateLong(const S: string): boolean;
function IsValidFilename(const S:string): boolean;
function PosofStr(List: TStrings;const S: string): integer;
function PosofStrInFirstField(List: TStrings;const S: string;Separator: char;Entrypoint: integer): integer;
function GetBirthYearFromCpr(const aCprno: string): string; { ########## => #### }
function DecodePlcDate(iDate: integer;aDate: PBcDateTime): boolean;
function GetDosAdress(Filename, AddrInit: String; List: TStrings): Boolean;
function AdressToYderNo(List: TStrings): TSt6;
function IsStrMatch(const S1,S2: string): double;
{ procedures }
procedure SwitchCharP(O,N: char;var S: TSt10);
procedure DosCp2WinCpPChar(P: pchar;Size: cardinal); { !!!ACHTUNG!!! PCHAR }
procedure DosCp2WinCp(var S: string);
procedure SetVersals(var S:string);
procedure DeleteFirstChar(var S:string);
procedure DeleteLastChar(var S:string);
procedure CheckForEndBackslash(var S: string);
procedure SetProperLongStringLength(var S: String; L: integer);
procedure IntSort(var A: array of Integer); { sorts any integerarray }
procedure StrSort(var A: array of string;MaxLen: integer);
procedure SortStringList(Strings: TStrings;MaxLen: integer);
procedure PutString(SubStr:Tst80;var Dest:Tst80;Index,FieldLen:integer);
procedure GotoXY(X,Y: smallint);
procedure WriteXY(S: string; X,Y: smallint);

implementation
//uses FileTree;


var
  HOut: THandle;                    { Needed to implement the GotoXY-procedure }

function Min(I1,I2: integer): integer;
begin
  if I1 < I2 then Result:= I1 else Result:= I2;
end;

function Max(I1,I2: integer): integer;
begin
  if I1 > I2 then Result:= I1 else Result:= I2;
end;

function LeadSpace(Len: integer;const S: string): string;
Var
  B,L,D: integer;
begin
  L:= length(S); D:= Len-L;
  setlength(Result,Len);
  for B:= 1 to Len do Result[B]:= ' ';
  for B:= L downto 1 do Result[B+D]:= S[B];
end; { LeadSpace }

function StrUpCase(const S: string): string;
{------------------------------------------}
var
  Ch: Char;
  L: Integer;
  Source,Dest: PChar;
begin { StrUpCase }
  L:= length(S);
  SetLength(Result,L);
  Source:= pointer(S);
  Dest:= pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32) else
    if Ch = Ae then Ch:= Aee else if Ch = Oe then Ch:= Oee else
    if Ch = Aa then Ch:= Aaa;
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;  { StrUpCase }

function StrLowCase(const S: string): string;
{-------------------------------------------}
var
  Ch: char;
  L: integer;
  Source, Dest: pchar;
begin { StrLowCase }
  L:= length(S);
  SetLength(Result,L);
  Source:= pointer(S);
  Dest:= pointer(Result);
  while L <> 0 do
  begin
    Ch:= Source^;                 { dos }
    if (Ch >= 'A') and (Ch <= 'Z') then Inc(Ch, 32) else
    if Ch = Aee then Ch:= Ae else if Ch = Oee then Ch:= Oe else
    if Ch = Aaa then Ch:= Aa;     { win32 }
    if Ch = 'Æ' then Ch:= 'æ' else if Ch = 'Ø' then Ch:= 'ø' else
    if Ch = 'Å' then Ch:= 'å';
    Dest^:= Ch;
    inc(Source);
    inc(Dest);
    dec(L);
  end;
end;  { StrLowCase }

function CharUpCase(Ch: char): char;
{----------------------------------------}
begin { CharUpCase }
  if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32) else
  if Ch = Ae then Ch:= Aee else if Ch = Oe then Ch:= Oee else
  if Ch = Aa then Ch:= Aaa;
  Result:= Ch;
end;  { CharUpCase }

function CharLowCase(Ch: char): char;
{-----------------------------------------}
begin { CharLowCase }
  if (Ch >= 'A') and (Ch <= 'Z') then Inc(Ch, 32) else
  if Ch = Aee then Ch:= Ae else if Ch = Oee then Ch:= Oe else
  if Ch = Aaa then Ch:= Aa;
  Result:= Ch;
end;  { CharLowCase }

function Str2Int(St: string): longint;
var
  I: longint;
  Err: integer;
begin
  if St<>'' then begin
    val(St,I,Err);
    if Err=0 then Str2Int:= I else Str2Int:= -1;
  end else Str2Int:= 0;
end; { str2int }

function GetMesLine32(Lineno: longint; Filename: string): string; { win32 }
var
  Counter,I: longint;
  Tmpstr,Res: string;
  Preffile: textfile;
begin
  Tmpstr:= ''; Res:='';
  assignfile(Preffile,Filename);
  {$I-} reset(Preffile); {$I+}
  if IOresult=0 then begin
    while not Eof(Preffile) do begin
      readln(Preffile,Tmpstr);
      if (copy(Tmpstr,1,1)<>'*') and (copy(Tmpstr,1,1)<>'=') and (Tmpstr<>'') then
        for i:= 1 to length(Tmpstr) do begin
          if (I=1) and (copy(Tmpstr,I,1)='0') then delete(Tmpstr,I,1);
          if copy(Tmpstr,I,1)='=' then begin
            counter:= str2int(copy(Tmpstr,1,I-1));
            if Counter = Lineno then Res:= copy(Tmpstr,I+1,length(Tmpstr)-I);
          end;
        end;
    end;
  end else Res:= 'Options file for this application does not exist...';
  closefile(Preffile);
  GetMesLine32:= Res;
end; { getmesline32 }

function GetField(No: integer; S:string): string;
var
  Tmp,Res: string;
  I: integer;
begin
  I:= 0; Tmp:=''; Res:= '';
  if length(S)>0 then begin
    S:=S+':';
    while (I<>No) and (length(S)>0) do begin
      while (copy(S,1,1)<>':') and (length(S)>0) do begin
        Tmp:= Tmp+copy(S,1,1);
        delete(S,1,1);
      end;
      if copy(S,1,1) = ':' then begin
        delete(S,1,1);
        inc(I);
      end;
      if I = No then Res:= Tmp else Tmp:='';
    end;
  end else Res:= S;
  GetField:= Res;
end; { getfield }

function GetFieldToken(No: integer; S:string; Separator: char): string;
var
  Tmp,Res: string;
  I: integer;
begin
  I:= 0; Tmp:='';                                             { initialization }
  if length(S)>0 then begin                                 { save clockcycles }
    S:=S+Separator;          { append separator to cater for last field / word }
    while (I<>No) and (length(S)>0) do begin
      while (copy(S,1,1)<>Separator) and (length(S)>0) do begin
        Tmp:= Tmp+copy(S,1,1);         { build temporary string as we go along }
        delete(S,1,1);              { this is why S is not a 'const' parameter }
      end;
      if copy(S,1,1) = Separator then begin       { safety check for separator }
        delete(S,1,1);{ if present delete it, we don't want the separator only }
        inc(I);        { the text in between. after that increment word number }
      end;
      if I = No then Res:= Tmp else Tmp:='';     { is it what we want?, if not }
    end;                                                            { continue }
  end else Res:= S;               { if fed an empty string return it untouched }
  GetFieldToken:= Res;                                { return our findings... }
end; { getfieldtoken }

function DeleteFirstWord(S : string) : string;
{--------------------------------------------}
begin
  if S<>'' then
    begin
      if (pos(' ',S)=0) then S:='' else
        delete(S,1,(pos(' ',S)));
      if copy(S,1,1)=' ' then delete(S,1,1);
      DeleteFirstWord := S;
    end;
end; { deletefirstword }

function DeleteFirstField(S : string) : string;
{--------------------------------------------}
begin
  if S<>'' then
    begin
      if (pos(':',S)=0) then S:='' else
        delete(S,1,(pos(':',S)));
      if copy(S,1,1)=':' then delete(S,1,1);
      DeleteFirstField := S;
    end;
end; { deletefirstfield }

function GetFirstWord(var S : string) : string;
{--------------------------------------------}
Var
  I : integer;
  S1: string;
begin
  I := 1; S1:= '';
  while (copy(S,I,1)<>' ') and (I <= length(S)) do
    begin
      S1:= S1+copy(S,I,1);
      Inc(I);
    end;
  Dec(I);
  setlength(S1,I);
  S:= DeleteFirstWord(S);
  GetFirstWord := S1;
end; { getfirstword }

function GetLastWord(S: string): string;
{--------------------------------------------}
Var
  I : integer;
  S1: string;
begin
  I := length(S); S1:= '';
  while (copy(S,I,1)<>' ') and (I>0)  do
    begin
      insert(copy(S,I,1),S1,1);
      Dec(I);
    end;
  setlength(S1,length(S)-I);
  GetLastWord := S1;
end; { getlastword }

function GetLastWordToken(S:string;Token:char): string;
{-----------------------------------------------------}
Var
  I : integer;
  S1: string;
begin
  I := length(S); S1:= '';
  while (copy(S,I,1)<>Token) and (I>0)  do
    begin
      insert(copy(S,I,1),S1,1);
      Dec(I);
    end;
  setlength(S1,length(S)-I);
  GetLastWordToken := S1;
end; { getlastwordtoken }

function GetLastWordSh(S : shortstring): shortstring;
{---------------------------------------------------}
Var
  I : integer;
  S1: shortstring;
begin
  I := length(S); S1:= '';
  while (copy(S,I,1)<>' ') and (I>0)  do
    begin
      insert(copy(S,I,1),S1,1);
      Dec(I);
    end;
  setlength(S1,length(S)-I);
  GetLastWordSh := S1;
end; { getlastwordsh }

function DeleteLastWord(S : string) : string;
{-------------------------------------------}
Var
  I : integer;
begin
  if s<>'' then begin
    I := length(S);
    while (copy(S,I,1)<>' ') and (I>0)  do
      begin
        delete(S,I,1);
        Dec(I);
      end;
    Dec(I);
    setlength(S,I);
  end;
  Result:= S;
end; { deletelastword }

function DeleteLastWordToken(S:string;Token:char): string;
{--------------------------------------------------------}
Var
  I : integer;
begin
  if s<>'' then begin
    I := length(S);
    while (copy(S,I,1)<>Token) and (I>0)  do
      begin
        delete(S,I,1);
        Dec(I);
      end;
    Dec(I);
    setlength(S,I);
  end;
  Result:= S;
end; { deletelastwordToken }

function SwitchChar(O,N: Char; S: string): string;
{------------------------------------------------}
var
  I: integer;
begin { switchchar }
  if S<>'' then for I:= 1 to length(S) do
    if S[I] = O then S[I]:= N;
  SwitchChar:= S;
end;  { switchchar }

function ValidateCpr(const Cpr: string): boolean; { checks true if numeric cpr }
{-----------------------------------------------}
var
  I: integer;
begin
  if length(Cpr) > 5 then begin
    Result:= true;
    for I:= 1 to length(Cpr) do begin
      if ((ord(Cpr[I])<45) or (ord(Cpr[I])>57)) then Result:= false;
    end;
    if Result then begin
      if (strtoint(copy(Cpr,1,2))>31) or (copy(Cpr,1,2)='00') then Result:= false;
      if (strtoint(copy(Cpr,3,2))>12) or (copy(Cpr,3,2)='00') then Result:= false;
    end;
  end else Result:= false;
end; { validatecpr }

function MakeStringNumeric(S: string): string;
{--------------------------------------------}
var
  I: integer;
begin { stringisnumeric }
  Result:='';
  if S<>'' then for I:= 1 to length(S) do
    if ((ord(S[I])<48) or (ord(S[I])>57)) then delete(S,I,1);
  Result:= S;
end;  { stringisnumeric }

function RemoveCh0(S: shortstring):shortstring;
{---------------------------------------------}
begin { remove#0 }
  if pos(#0,S)<>0 then while pos(#0,S)<>0 do delete(S,pos(#0,S),1);
  Result:= S;
end;  { remove#0 }

function RemoveChSh(Ch: char;S: shortstring):shortstring;
{-------------------------------------------------------}
begin { removechsh }
  if pos(Ch,S)<>0 then while pos(Ch,S)<>0 do delete(S,pos(Ch,S),1);
  Result:= S;
end;  { removechsh }

function RemoveCh(Ch: char;S: string):string;
{-------------------------------------------}
begin { removech }
  if pos(Ch,S)<>0 then while pos(Ch,S)<>0 do delete(S,pos(Ch,S),1);
  Result:= S;
end;  { removech }

function RemoveStr(const SubStr,S: string):string;
{------------------------------------------------}
var Len: integer;
begin
  Len:= length(SubStr); Result:= S;
  if pos(SubStr,Result)<>0 then
    while pos(SubStr,Result)<>0 do delete(Result,pos(SubStr,Result),Len);
end;  { removestr }

function RemoveCtrlChars(const S: string): string;
var I: integer;
begin
  Result:= ''; 
  if length(S) > 0 then begin
    for I:= 1 to length(S) do begin
      case ord(S[I]) of
        $0A,$0D : Result:= Result+S[I];
        $20..$80: Result:= Result+S[I];
      end;  
//      if ((ord(S[I]) < $0A) and (ord(S[I]) > $0D) and (ord(S[I]) < $20)) then delete(Result,I,1); // ie. ' '
    end;
  end;
end;

function FoundInString(SubStr,S: string): boolean;
{------------------------------------------------}
begin
  Result:= pos(SubStr,S)<>0;
end;  { foundinstring }

function PosApart(Token: char;CharsBetween: integer;const S: string): boolean;
{----------------------------------------------------------------------------}
var
  I,Trailer: integer;
begin
  Result:= false; inc(CharsBetween);
  if S<>'' then for I:= 1 to length(S) do begin
    Trailer:= I-CharsBetween;
    if Trailer>=1 then Result:= ((S[I]=Token) and (S[Trailer]=Token));
    if Result then break;
  end;
end;  { posapart }

function PosApartMaxLen(Token: char;CharsBetween: integer;const S: string;MaxLen: integer): boolean;
{--------------------------------------------------------------------------------------------------}
var
  I,Trailer: integer;
begin
  Result:= false; inc(CharsBetween);
  if length(S) < MaxLen then MaxLen:= length(S);
  for I:= 1 to MaxLen do begin
    Trailer:= I-CharsBetween;
    if Trailer>=1 then Result:= ((S[I]=Token) and (S[Trailer]=Token));
    if Result then break;
  end;
end;  { posapartmaxlen }

function SubstituteStr(OldS,NewS,S: string):string;
{-------------------------------------------------}
begin
  if pos(OldS,S)<>0 then
    while pos(OldS,S)<>0 do begin
      insert(NewS,S,pos(OldS,S));
      delete(S,pos(OldS,S),length(OldS));
    end;
  Result:= S;
end;  { substitutestr }

function CodeLine2CodeString(ACodeLine: TCodeLine): string;
{---------------------------------------------------------}
begin
  if ACodeLine=nil then raise Exception.Create('Cannot access a nil-pointer.')
  else begin
    { sygcodeformat= 99:03:01:1:1:0000:*DUnknown:: }
    Result:= ACodeLine.Year+':'+ACodeLine.Month+':'+ACodeLine.Day+':'+
             ACodeLine.No+':'+ACodeLine.Time+':'+ACodeLine.Code+':'+
             ACodeLine.Note+'::';
  end;
end; { CodeLine2CodeString }

function CodeString2CodeLine(ACodeString: string): TCodeLine;
{-----------------------------------------------------------}
begin
  if ACodeString='' then raise Exception.Create('Cannot convert an empty string.')
  else begin
    { sygcodeformat= 99:03:01:1:1:0000:*DUnknown:: }
    Result:= TCodeLine.Create;
    Result.Year:= GetField(1,ACodeString);
    Result.Month:= GetField(2,ACodeString);
    Result.Day:= GetField(3,ACodeString);
    Result.No:= GetField(4,ACodeString);
    Result.Time:= GetField(5,ACodeString);
    Result.Code:= GetField(6,ACodeString);
    Result.Note:= GetField(7,ACodeString);
  end;
end; { CodeString2CodeLine }

function CompareDates(Date1,Date2: TDate): integer;
{-------------------------------------------------}
begin      // returns -1 if date1 is newest, 0 if equal or 1 if date2 is newest.
  Result:= 0;                // when 2 digit year, 00-85 = 2000+, 85-99 = 1900+.
  if ((Date1.Year='0') or (Date1.Year='00') or (Date1.Year='000')) then Date1.Year:='2000' else
  if length(Date1.Year) < 3 then begin
    if strtoint(Date1.Year)<86 then Date1.Year:= inttostr(strtoint(Date1.Year)+2000) else
    if strtoint(Date1.Year)>85 then Date1.Year:= inttostr(strtoint(Date1.Year)+1900);
  end;
  if ((Date2.Year='0') or (Date2.Year='00') or (Date2.Year='000')) then Date2.Year:='2000' else
  if length(Date2.Year) < 3 then begin
    if strtoint(Date2.Year)<86 then Date2.Year:= inttostr(strtoint(Date2.Year)+2000) else
    if strtoint(Date2.Year)>85 then Date2.Year:= inttostr(strtoint(Date2.Year)+1900);
  end;
  if strtoint(Date1.Year) > strtoint(Date2.Year) then Result:= -1 else
  if strtoint(Date2.Year) > strtoint(Date1.Year) then Result:= 1 else
  if strtoint(Date1.Year) = strtoint(Date2.Year) then begin
    if strtoint(Date1.Month) > strtoint(Date2.Month) then Result:= -1 else
    if strtoint(Date2.Month) > strtoint(Date1.Month) then Result:= 1 else
    if strtoint(Date1.Month) = strtoint(Date2.Month) then begin
      if strtoint(Date1.Day) > strtoint(Date2.Day) then Result:= -1 else
      if strtoint(Date2.Day) > strtoint(Date1.Day) then Result:= 1 else
      if strtoint(Date1.Day) = strtoint(Date2.Day) then Result:= 0;
    end;
  end;
end; { CompareDates }

function DeleteFirstFieldToken(S:string;Token:char): string;
{----------------------------------------------------------}
begin
  if S<>'' then begin
    if (pos(Token,S)=0) then S:='' else
      delete(S,1,(pos(Token,S)));
    if copy(S,1,1)=Token then delete(S,1,1);
  end;
  Result:= S;
end; { deletefirstfieldtoken }

function GetFirstFieldToken(var S:string;Token:char): string;
{-----------------------------------------------------------}
Var
  I : integer;
  S1: string;
begin
  I := 1; S1:= '';
  while (copy(S,I,1)<>Token) and (I <= length(S)) do
    begin
      S1:= S1+copy(S,I,1);
      Inc(I);
    end;
  Dec(I);
  setlength(S1,I);
  S:= DeleteFirstFieldToken(S,Token);
  Result:= S1;
end; { getfirstfieldtoken }

function GetCityFromZipCode(Zip:string): string;
{----------------------------------------------}
var
  List: TStrings;
  Line: string;
  I: integer;
begin
  List:= TStringList.Create;
  Result:= '';
  try
    List.LoadFromFile('c:\eme\ud\conv\postno.txt');
    try
      for I:= 0 to List.Count-1 do begin
        Line:= List[I];
        if FoundInString(Zip,Line) then begin
          Result:= copy(Line,pos(' ',Line)+1,length(Line));
          break;
        end;  
      end;
    finally
      List.Free;
    end;
  except on E:EInOutError do
    begin
      List.Free;
      Messagebox(0,pchar(E.Message),'ZipCityRetrieval failure',MB_ICONERROR or MB_OK);
    end;
  end;
end; { GetCityFromZipCode }

function GetZipCodeFromCity(City:string): string;
{----------------------------------------------}
var
  List: TStrings;
  Line: string;
  I: integer;
begin
  List:= TStringList.Create;
  Result:= '';
  try
    List.LoadFromFile('c:\eme\ud\conv\postno.txt');
    try
      for I:= 0 to List.Count-1 do begin
        Line:= List[I];
        if FoundInString(City,Line) then begin
          Result:= copy(Line,1,4);
          break;
        end;
      end;
    finally
      List.Free;
    end;
  except on E:EInOutError do
    begin
      List.Free;
      Messagebox(0,pchar(E.Message),'ZipCityRetrieval failure',MB_ICONERROR or MB_OK);
    end;
  end;
end; { GetZipCodeFromCity }

function LastSpaceInStr(S:string): integer;
{-----------------------------------------}
var
  I: integer;
begin
  I:= -1;
  if S<>'' then begin
    I:= length(S);
    while ((S[I]<>' ') and (I>0)) do dec(I);
    if I=0 then I:= -1;
  end;
  Result:= I;
end; { LastSpaceInStr }

function LastTokenInStr(Token:char;S:string): integer;
{----------------------------------------------------}
var
  I: integer;
begin
  I:= -1;
  if S<>'' then begin
    I:= length(S);
    while ((S[I]<>Token) and (I>0)) do dec(I);
    if I=0 then I:= -1;
  end;
  Result:= I;
end; { LastTokenInStr }

function GetFirstStrUpCase(var S:string): string;
{-----------------------------------------------}
var
  I: integer;
begin
  Result:=''; I:=1;
  if S<>'' then begin
    while ((S[I] in ['A'..'Z',Aee,Oee,Aaa,' ','-','.']) and (I<length(S))) do inc(I);
    if I<>1 then begin
      dec(I);
      Result:= copy(S,1,I);
      delete(S,1,I);
      Result:= trim(Result);
    end;
  end;
end; { GetFirstStrUpCase }

function GetFirstUpCaseChar(const S:string): Integer;
{---------------------------------------------------}
var
  I: integer;
begin
  Result:=-1; I:=1;
  if S<>'' then begin
    while ((not (S[I] in ['A'..'Z',Aee,Oee,Aaa])) and (I<length(S))) do inc(I);
    if I<>1 then begin
      Result:= I;
    end;
  end;
end; { GetFirstUpCaseChar }

function GetFirstDigit(const S:string): Integer;
{----------------------------------------------}
var
  I: integer;
begin
  Result:=-1; I:=1;
  if S<>'' then begin
    while ((not (S[I] in ['0'..'9'])) and (I<length(S))) do inc(I);
    if I > 0 then begin
      Result:= I;
    end;
  end;
end; { GetFirstDigit }

function GetFirstDigitStr(const S:string): string;
{------------------------------------------------}
var
  I,Start: integer;
begin
  Result:=''; I:= GetFirstDigit(S);
  if I <> -1 then begin                             { string contains digit(s) }
    Start:= I;
    while ((S[I] in ['0'..'9']) and (I<length(S))) do inc(I);
    if I > 1 then begin
      Result:= copy(S,Start,I-Start);
      Result:= trim(Result);
    end;
  end;
end; { GetFirstDigitStr }

function GetFirstFloatStr(const S:string): string;
{------------------------------------------------}
var
  I,Start: integer;
begin
  Result:=''; I:= GetFirstDigit(S);
  if I <> -1 then begin                             { string contains digit(s) }
    Start:= I;
    while ((S[I] in ['0'..'9',',','.','-']) and (I<length(S))) do inc(I);
    if I > Start then begin
      Result:= copy(S,Start,(I-Start)+1);
      Result:= trim(Result);
    end;
  end;
end; { GetFirstFloatStr }

function EmEncrypt(Line: string): string;
{---------------------------------------}
begin
  if Line <> '' then BcEncrypt(Line);
  Result:= Line;
end; { EmEncrypt }

function EmDecrypt(Line: string): string;
{---------------------------------------}
begin
  if Line <> '' then BcDecrypt(Line);
  Result:= Line;
end; { EmDecrypt }

function CalTimeApart(Time1,Time2: string): integer;
{--------------------------------------------------}
var
  I1,I2,H,M: integer;
begin
  Time1:= RemoveStr('.',Time1); Time1:= RemoveStr(':',Time1); Time1:= RemoveStr('-',Time1); Time1:= RemoveStr(',',Time1);
  Time2:= RemoveStr('.',Time2); Time2:= RemoveStr(':',Time2); Time2:= RemoveStr('-',Time2); Time2:= RemoveStr(',',Time2);
  H:= strtoint(copy(Time1,1,2)); M:= strtoint(copy(Time1,3,2)); I1:= (H*60)+M;
  H:= strtoint(copy(Time2,1,2)); M:= strtoint(copy(Time2,3,2)); I2:= (H*60)+M;
  Result:= I2-I1;
end; { caltimeapart }

function StringIsNumeric(const S:string): boolean;
{------------------------------------------------}
var
  I: integer;
begin
  Result:=true;
  if S<>'' then begin
    for I:= 1 to length(S) do if ((ord(S[I])<48) or (ord(S[I])>57)) then
      if not (S[I] in [',','.','-']) then Result:= false;
  end else Result:= false;
end; { StringIsNumeric }

function SubStrIndexInList(const SubStr: string;Index: integer;const List: TStrings): integer;
{---------------------------------------------------------------------------------------------}
var
  I: integer;
begin
  Result:= -1;
  if ((List <> nil) and
     (SubStr <> '') and
     (Index > 0)) then
    for I:= 0 to List.Count-1 do begin
      if copy(List[I],Index,length(SubStr)) = SubStr then begin
        Result:= I;
        break;
      end;
    end;
end; { SubStrIndexInList }

function GetLastChar(const S: string): char;
begin
  Result:= #0;
  if S <> '' then Result:= S[length(S)];
end; { GetLastChar }

function FileCopy(Source, Dest: string): boolean;
{-----------------------------------------------}
var
  FI,FO: TFileStream;
  P: pchar;
  I,Err: integer;
begin
  FO:= nil; P:= nil;                                                {initialize}
  FI:= TFileStream.Create(Source,fmOpenRead);                    {open instream}
  I:= FI.Size;                                                 {get buffer size}
  try
    GetMem(P,I);                                    {allocate memory for buffer}
    Err:= FI.Read(P^,I);                           {read size bytes into buffer}
    FO:= TFileStream.Create(Dest,fmCreate);                   {create outstream}
    Result:= (Err = FO.Write(P^,I));             {write size bytes to outstream}
  finally
    FI.Free;                                                     {free instream}
    FO.Free;                                                    {free outstream}
    FreeMem(P);                                              {deallocate buffer}
  end;
end; { FileCopy }

function Month2NumStr(const S:string): TSt2;
{------------------------------------------}
var I: byte;
begin
  for I:= 0 to 11 do if FoundInString(S,MonthSet[I]) then begin
    Result:= inttostr(I+1);
    if length(Result) < 2 then Result:= '0'+Result;
  end;
end; { Month2NumStr }

function Str2AbsTime(S: TSt5): integer;
{-------------------------------------}
var { S format must be either "##.##" or "##:##" or "##-##" or "##,##"}
  I: integer;
begin
  Result:= -1;
  if S <> '' then begin
    I:= length(S);
    while I > 0 do begin
      if S[I] = 'o' then S[I]:= '0';
      if S[I] = 'O' then S[I]:= '0';
      if S[I] = 'l' then S[I]:= '1';
      if S[I] = 'L' then S[I]:= '1';
      dec(I);
    end;
    Result:= 60 * strtoint(copy(S,1,2));
    Result:= Result + strtoint(copy(S,4,2));
  end;
end; { Str2AbsTime }

function dB2Hex(dB:integer): TSt2;
{--------------------------------}
const
  Hex: array[0..15] of char = '0123456789ABCDEF';
begin { dB2Hex }
  Result:= '';
//  dB:= 120 - dB;               { +121 dB -> $FF, +120 dB -> $00, -10 dB -> $82 }
  if dB = -1 then dB:= 255;
  Result:= Hex[dB div 16] + Hex[dB mod 16];
end;  { dB2Hex }

function dB2HexMirror(dB:integer): TSt2;
{-------------------------------------}
const
  Hex: array[0..15] of char = '0123456789ABCDEF';
begin { dB2Hex }
  Result:= '';
  dB:= 120 - dB;               { +121 dB -> $FF, +120 dB -> $00, -10 dB -> $82 }
  if dB = -1 then dB:= 255;
  Result:= Hex[dB div 16] + Hex[dB mod 16];
//  dB2Hex:= Hex[dB div 16] + Hex[dB mod 16]; { old tp55 convention }
end;  { dB2Hex }

function Hex2Num(N:TSt2): byte;
{-----------------------------}
var
  I1,I2: byte;
begin { Hex2Num }
  I1:= ord(N[1]); I2:= Ord(N[2]);
  if I1 >= 65 then dec(I1,7); if I2 >= 65 then dec(I2,7);
  dec(I1,48); dec(I2,48);
  inc(I2,16*I1);
  Result:= I2;
//  Hex2Num:= I; { old tp55 convention }
end;  { Hex2Num }

function TrueDate2EmarDate(const S: string): string;
begin
  Result:= '';
  if length(S) = 8 then begin { ex.: 20010510 }
    Result:= copy(S,7,2)+'.'+copy(S,5,2)+'.'+copy(S,3,2);
  end;
  if length(S) = 6 then begin { ex.: 010510 }
    Result:= copy(S,5,2)+'.'+copy(S,3,2)+'.'+copy(S,1,2);
  end;
end; { TrueDate2EmarDate }

function LabDate2LongDate(const S: string): string;
var Tmp: string;        { format ddmmyy -> dd.mm.yyyy }
begin
  if ((length(S) = 6) and (StringIsNumeric(S))) then begin
    Result:= copy(S,1,2)+dateseparator+copy(S,3,2)+dateseparator;
    Tmp:= copy(S,5,2);
    try
      if strtoint(Tmp) > 35 then system.Insert('19',Tmp,1) else system.Insert('20',Tmp,1)
    except system.Insert('19',Tmp,1) end;
    Result:= Result+Tmp;
  end;
end; { LabDate2LongDate }

function CalDate2LongDate(const S: string): string;
var Tmp: string;        { format yymmdd -> dd.mm.yyyy }
begin
  if ((length(S) = 6) and (StringIsNumeric(S))) then begin
    Result:= copy(S,5,2)+dateseparator+copy(S,3,2)+dateseparator;
    Tmp:= copy(S,1,2);
    try
      if strtoint(Tmp) > 35 then system.Insert('19',Tmp,1) else system.Insert('20',Tmp,1)
    except system.Insert('19',Tmp,1) end;
    Result:= Result+Tmp;
  end;
end; { CalDate2LongDate }

function DosCp2WinCpF(const S: string): string;
begin
  Result:= S;
  DosCp2WinCp(Result);
end; { DosCp2WinCpF }

function WinCp2DosCpF(const S: string): string;
var
  I: integer;
begin
  Result:= S;
  if Result <> '' then for I:= 1 to length(Result) do begin
    case Result[I] of
      'æ': Result[I]:= Ae;
      'ø': Result[I]:= Oe;
      'å': Result[I]:= Aa;
      'Æ': Result[I]:= Aee;
      'Ø': Result[I]:= Oee;
      'Å': Result[I]:= Aaa;
    end;
  end;
end; { WinCp2DosCpF }

function CreateNewname(const Filename,OldPath,NewPath: string): string;
begin
  Result:= RemoveStr(StrLowCase(OldPath),StrLowCase(Filename));
  insert(NewPath,Result,1);
end; { CreateNewname }

function StrIsDate(const S: string): boolean;
begin
  Result:= true;
  try
    if length(trim(S)) < 8 then begin
      Result:= false;
      exit;
    end;
    if ((copy(S,3,1) <> '.') and (copy(S,6,1) <> '.')) then begin
      Result:= false;
      exit;
    end;
    if (strtoint(copy(S,1,2)) > 31) then begin
      Result:= false;
      exit;
    end;
    if (strtoint(copy(S,4,2)) > 12) then begin
      Result:= false;
      exit;
    end;
    if not StringIsNumeric(copy(S,7,2)) then begin
      Result:= false;
      exit;
    end;
  except end;
end; { StrIsDate }

function StrIsDateLong(const S: string): boolean;
begin                   { dd.mm.yyyy }
  Result:= true;
  try
    if length(trim(S)) < 10 then begin
      Result:= false;
      exit;
    end;
    if ((copy(S,3,1) <> '.') and (copy(S,6,1) <> '.')) then begin
      Result:= false;
      exit;
    end;
    if (strtoint(copy(S,1,2)) > 31) then begin
      Result:= false;
      exit;
    end;
    if (strtoint(copy(S,4,2)) > 12) then begin
      Result:= false;
      exit;
    end;
    if not StringIsNumeric(copy(S,7,4)) then begin
      Result:= false;
      exit;
    end;
  except end;
end; { StrIsDateLong }

function IsValidFilename(const S:string): boolean;
{------------------------------------------------}
var
  I: integer;
begin
  Result:= true;
  if S<>'' then begin
    for I:= 1 to length(S) do if S[I] in ['\','/',':','*','?','"','<','>','|'] then begin
      Result:= false;
      break;
    end;
  end else Result:= false;
end; { IsValidFilename }

function PosofStr(List: TStrings;const S: string): integer;
var I: integer;
begin
  Result:= -1;
  for I:= 0 to List.Count-1 do begin
    if pos(S,List[I]) <> 0 then begin
      Result:= I;
      break;
    end;
  end;
end; { PosofStr }

function PosofStrInFirstField(List: TStrings;
                              const S: string;
                              Separator: char;
                              Entrypoint: integer): integer;
var
  I: integer;
  F1: string;
begin
  Result:= -1;
  for I:= EntryPoint to List.Count-1 do begin
    F1:= GetFieldToken(2,List[I],Separator);
    if S = F1 then begin
      Result:= I;
      break;
    end;
  end;
end; { PosofStrInFirstField }

function GetBirthYearFromCpr(const aCprno: string): string;
begin
  Result:= '';
  if length(aCprno) > 5 then begin
    Result:= copy(aCprno,5,2);
    if Result[1] = '0' then insert('20',Result,1)
    else insert('19',Result,1);
  end;
end; { GetBirthYearFromCpr }

function DecodePlcDate(iDate: integer;aDate: PBcDateTime): boolean;
const
  Base           = 1980;      { plc timebase }
  YearModifier   = 33554432;  { ie.: 2^25 }
  MonthModifier  = 2097152;   { ie.: 2^21 }
  DayModifier    = 65536;     { ie.: 2^16 }
  HourModifier   = 2048;      { ie.: 2^11 }
  MinuteModifier = 32;        { ie.: 2^5  }
begin
  if iDate > 0 then begin
    aDate^.wYear:= (iDate div YearModifier) + Base;
    iDate:= iDate mod YearModifier;
    aDate^.bMonth:= iDate div MonthModifier;
    iDate:= iDate mod MonthModifier;
    aDate^.bDay:= iDate div DayModifier;
    iDate:= iDate mod DayModifier;
    aDate^.bHour:= iDate div HourModifier;
    iDate:= iDate mod HourModifier;
    aDate^.bMinute:= iDate div MinuteModifier;
    aDate^.bSecond:= iDate mod MinuteModifier;
    Result:= true;
  end else Result:= false;
end; { DecodePlcDate }

function ConvDKChar(const Str: String): string;
var
  I: Longint;
  XStr: String;
  Ch: Char;
begin
  Result:= Str;
  for I:=1 to Length(Result) do begin
    XStr:= copy(Result,I,1);
    Case Ord(XStr[1]) of
      145: Result[I]:= 'æ';
      155: Result[I]:= 'ø';
      134: Result[I]:= 'å';
      146: Result[I]:= 'Æ';
      157: Result[I]:= 'Ø';
      143: Result[I]:= 'Å';
      205: Result[I]:= '=';
      218: Result[I]:= #173;
      191: Result[I]:= #173;
      192: Result[I]:= #173;
      217: Result[I]:= #173;
      196:begin
            Result[I]:= #173;
            XStr[1]:= #173;
          end;
      179: Result[I]:= ' ';
    end;
    Ch:=XStr[1];
    Case Ch of
      'ü','Ü': Result[I]:= 'U';
      'ä','Ä': Result[I]:= 'A';
      'ö','Ö': Result[I]:= 'O';
      'ñ','Ñ': Result[I]:= 'N';
    end;
  end;
end; { ConvDKChar }

function GetDosAdress(Filename, AddrInit: String; List: TStrings): Boolean;
var
  Fp: TextFile;
  Line: String;
begin
  Result:= false; Line:= '';
  AssignFile(Fp,Filename);
  {I-} reset(Fp); {I+}
  if IOResult = 0 then begin
    while (not eof(Fp)) and (pos('#'+AddrInit,Line) <> 1) do begin
      readLn(Fp,Line);
      Line:= ConvDKChar(Line);
    end;
    while (not eof(Fp)) and (Length(Line) > 0) do begin
      readLn(Fp,Line);
      Line:= ConvDKChar(Line);
      List.Add(Line);
    end;
  end;
  closefile(Fp);
  if List.Count=0 then
    List.Add('NN')
  else begin
    List[0]:= List[0]+'';
    Result:= true;
  end;
end; { GetDosAddress }

function AdressToYderNo(List: TStrings): TSt6;
var
  I: integer;
  S: string;
begin       
  Result:= '      ';
  if List.Count > 0 then for I:= 0 to List.Count-1 do begin
    S:= List[I];
    if copy(S,1,1) = '%' then begin
      Result:= copy(S,2,6);
      break;
    end;
  end;
end; { AdressToYderNo }

function IsStrMatch(const S1,S2: string): double;
var
  I,iMin,iMax,iSameCount: integer;
begin
  iMax:= Max(length(s1),length(s2));
  iMin:= Min(length(s1),length(s2));
  iSameCount:= - 1;
  for I:= 0 to iMax do begin
    if i > iMin then break;
    if s1[i] = s2[i] then inc(iSameCount) else break;
  end;
  if iSameCount > 0 then Result:= (iSameCount / iMax) * 100 else Result:= 0.00;
end; { IsStrMatch }

{--------------- procedures ---------------}
procedure SwitchCharP(O,N: Char;var S: TSt10);
{--------------------------------------------}
var
  I: integer;
begin { switchchar }
  if S<>'' then for I:= 1 to length(S) do if S[I] = O then S[I]:= N;
end;  { switchcharP }

procedure DosCp2WinCpPChar(P: pchar;Size: cardinal);
var I: longint;
begin
  for I:=0 to Size-1 do begin
    case Ord(pchar(P+I)^) of
      145: pchar(P+I)^:= 'æ';
      155: pchar(P+I)^:= 'ø';
      134: pchar(P+I)^:= 'å';
      146: pchar(P+I)^:= 'Æ';
      157: pchar(P+I)^:= 'Ø';
      143: pchar(P+I)^:= 'Å';
      205: pchar(P+I)^:= '=';
      191,192,196,217,218: pchar(P+I)^:= #173;
      179: pchar(P+I)^:= ' ';
    end;
    case pchar(P+I)^ of
      'ü','Ü': pchar(P+I)^:= 'U';
      'ä','Ä': pchar(P+I)^:= 'A';
      'ö','Ö': pchar(P+I)^:= 'O';
      'ñ','Ñ': pchar(P+I)^:= 'N';
    end;
  end; { loop end }
end;

procedure DosCp2WinCp(var S: string);
var
  I: integer;
begin
  if S <> '' then for I:= 1 to length(S) do begin
    case S[I] of
      #$27: S[I]:= 'æ'; { anomalie for ms winword }
      #$91: S[I]:= 'æ';
      #$9B: S[I]:= 'ø';
      #$86: S[I]:= 'å';
      #$92: S[I]:= 'Æ';
      #$9D: S[I]:= 'Ø';
      #$8F: S[I]:= 'Å';
    end;
  end;
end; { DosCp2WinCp }

procedure SetVersals(var S:string);
{---------------------------------}
var
  S1,S2: string;
begin { SetVersals }
  if S<>'' then begin
    S1:= S; S:= '';
    while length(S1)>0 do begin
      S2:= GetFirstWord(S1);
      S2:= StrLowCase(S2);
      if length(S2)>= 1 then S2[1]:= CharUpCase(S2[1]);
      S:=S+' '+S2;
    end;
    S:= trim(S);
  end;
end;  { SetVersals }

procedure DeleteFirstChar(var S:string);
{--------------------------------------}
begin { deletefirstchar }
  if S<>'' then delete(S,1,1);
end;  { deletefirstchar }

procedure DeleteLastChar(var S:string);
{-------------------------------------}
begin { deletefirstchar }
  if S<>'' then delete(S,length(S),1);
end;  { deletelastchar }

procedure CheckForEndBackslash(var S: string);
{--------------------------------------------}
begin { checkforendbackslash }
  if S<>'' then if S[length(S)]<>'\' then S:= S+'\';
end;  { checkforendbackslash }

procedure SetProperLongStringLength(var S: String; L: integer);
{-------------------------------------------------------------}
begin { setproperstringlength }
  if pos(#0,S)<>0 then while pos(#0,S)<>0 do delete(S,pos(#0,S),1);
  Setlength(S,L);
end;  { setproperstringlength }

procedure IntSort(var A: array of Integer);
{-----------------------------------------}
  procedure QuickSort(var A: array of Integer;iLo,iHi: Integer);
  {............................................................}
  var
    Lo, Hi, Mid, T: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    repeat
      while A[Lo] < Mid do Inc(Lo);
      while A[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort(A, iLo, Hi);
    if Lo < iHi then QuickSort(A, Lo, iHi);
  end; { quicksort }

begin
  QuickSort(A, Low(A), High(A));
end; { intquicksort }

procedure StrSort(var A: array of string;MaxLen: integer);
{--------------------------------------------------------}
  procedure QuickSort(var A: array of string;iLo,iHi: Integer);
  {...........................................................}
  var
    Lo,Hi: Integer;
    Mid,T: string;
  begin
    if iHi = -1 then raise ESortError.Create('Array is Empty') else begin
      Lo := iLo;
      Hi := iHi;
      Mid := copy(A[(Lo + Hi) div 2],1,MaxLen);;
      repeat
        while copy(A[Lo],1,MaxLen) < Mid do Inc(Lo);
        while copy(A[Hi],1,MaxLen) > Mid do Dec(Hi);
        if Lo <= Hi then
        begin
          T := A[Lo];
          A[Lo] := A[Hi];
          A[Hi] := T;
          Inc(Lo);
          Dec(Hi);
        end;
      until Lo > Hi;
      if Hi > iLo then QuickSort(A, iLo, Hi);
      if Lo < iHi then QuickSort(A, Lo, iHi);
    end;
  end; { quicksort }

begin
  QuickSort(A, Low(A), High(A));
end; { strsort }

procedure SortStringList(Strings: TStrings;MaxLen: integer);
{--------------------------------------------------------------}
var
  A: array of string;
  I: integer;
begin
  setlength(A,Strings.Count);
  for I:= 0 to Strings.Count-1 do A[I]:= Strings[I];
  Strings.Clear;
  StrSort(A,MaxLen);
  for I:= low(A) to high(A) do Strings.Add(A[I]);
  A:= nil;
end; { SortStringList }

procedure PutString(SubStr:Tst80;var Dest:Tst80;Index,FieldLen:integer);
{------------------------------------------------------------------------}
var               { if called with FieldLen = -1, no spacepadding is performed }
  SubLen,I: integer; { if length(substr) < Fieldlen, SubStr is right justified }
begin
  if SubStr <> '' then begin
    if Dest = '' then Dest:= Dest+SubStr else begin
      SubLen:= length(SubStr);
//      if Index > length(Dest) then setlength(Dest,Index+SubLen);
      if ((FieldLen = -1) or (SubLen = FieldLen)) then      // no padding needed
        for I:= index to Index+SubLen do Dest[I]:= SubStr[(I-Index)+1]
      else begin                                          // spacepadding needed
        while length(SubStr) < FieldLen do SubStr:= ' '+SubStr;
        for I:= index to Index+FieldLen-1 do begin
          Dest[I]:= SubStr[(I-Index)+1];
        end;
      end;
    end;
  end;
end; { PutString }

{--------------- Screen Routines for console applications ---------------}

procedure GotoXY(X,Y: smallint);
{------------------------------}
var
  Cursor: TCoord;
begin
  Cursor.X:= X; Cursor.Y:= Y;
  SetConsoleCursorPosition(HOut,Cursor);
end;  { gotoxy }

procedure WriteXY(S: string; X,Y: smallint);
{------------------------------------------}
begin
  GotoXY(X,Y);
  write(S);
end;  { writexy }



begin { executed on unitload }
  HOut:= GetStdHandle(STD_OUTPUT_HANDLE);
end.


