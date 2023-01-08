


              {************************************************$
              $  Unit name : bc_advstring.pas                  $
              $  Copyright : (C)2009-2022 cdbc.dk              $
              $  Programmer: Benny Christensen                 $
              $  Created   : 2009-11-13/bc                     $
              $  Updated   : 2022-11-06/bc                     $
              $ ********************************************** $
              $  Purpose   :                                   $
              $  Functions & procedures, which implements      $
              $  operations on a string.                       $
              $************************************************}

unit bc_advstring;
{$mode ObjFPC}{$H+}
interface
uses
  SysUtils,LazUTF8,StrUtils;

const
  { version control }
  UnitVersion = '5.29.12.2022';
  { codepages we can handle for now }
  CPUTF8        = 0; { well duh! }
  CPISO_8859_1  = 1; { central europe }
  CPISO_8859_15 = 2; { western european languages }
  CP1252        = 3; { latin 1 }
  CPUTF8BOM     = 4; { first 3 bytes is byte order mark }
  CPUTF16LE     = 5; { unicode little endian }
  {$ifdef unix} bcPathDelimiter = '/'; {$else} bcPathDelimiter = '\'; {$endif}
  bcExtDelimiter = '.'; { if at all an extension then its separated with a '.' }
type
  { Text alignment type }
  TTextAlignment = (txaLeftJustify, txaRightJustify, txaCenter);
  { type of string i.e, TAlpha, TAlphaNumeric, TNumeric }
  TbcStringType = (TAlpha, TAlphaNumeric, TNumeric);

{ GetFieldToken allows the user to pick a "field" in a string separated by a token/char }
{ usage: Surname:= GetFieldToken(2,'Benny|Christensen','|') => 'Christensen'  or }
{        Firstname:= GetFieldToken(1,'Benny Christensen',' ') => 'Benny' }
{ FieldNo start at 1 and upwards, returns an empty string on error }
function bcGetFieldToken(const FieldNo: ptrint;const S: string;const Token: char): string;
{ bcStrCase usage....
  case bcStrCase(S,['+date','+hello','+data','+quit']) of
    0: // '+date'
    1: // '+hello'
    ...
  end; }
{ bcStrCase is zero-based, returns -1 on not found }
function bcStrCase(const S: string;Elements: array of string): ptrint;
{ bcCloneString copies / clones a string on byte-level, not reference  }
function bcCloneString(const aSource: string): string; { duplicate NOT reference }
{ bcSwapChar swaps all OldChars in the string S, with NewChars in the result string }
function bcSwapChar(const OldChar,NewChar: char;const S: string): string;
{ bcCheckForTrailingPathDelim checks for and adds a trailing delimiter if nescessary }
function bcCheckForTrailingPathDelim(const aStr: string): string;
{ beExtractFileExt takes a filename and returns the extension ~ '.png' or an empty string ~ '' }
function bcExtractFileExt(const aFileName: string): string;
{ bcGetAppDir returns the directory from where the app was started, including trailing path-delim }
function bcGetAppDir: string;
{ bcShortenFilename takes a long filename and shortens it to 2 quarts with '\...\' in between }
function bcShortenFilename(const aFilename: string): string;
{ bcGuessEncoding tries to ascertain the codepage by testing }
function bcGuessEncoding(const aStr: string): word;
{ bcEncodeUTF8 takes a string and returns it encoded with UTF8 multibytes }
function bcEncodeUTF8(const aStr: string;aCodepage: word = CPUTF8): string;
{ bcDecodeUTF8 takes an UTF8 encoded string and returns an ISO-8859-1 string }
function bcDecodeUTF8(const aUtf8Str: string;aCodepage: word = CPISO_8859_15): string;

{ bcStrAlloc returns a fresh pchar of len chars, must be free'd with bcstrdispose }
function bcStrAlloc(aLen: cardinal): pchar;
{ bcStrNew allocates a new pchar and copies the content of astr into it }
function bcStrNew(aStr: string): pchar;
{ bcStrLength returns the amount of memory allocated for pchar aStr allocated with bcStrAlloc  }
function bcStrLength(aStr: pchar): cardinal;
{ bcStrCopy returns a string with a copy of the content of astr }
function bcStrCopy(aStr: pchar): string;
{ bcStrDispose clears the memory allocated with bcStrAlloc }
procedure bcStrDispose(aStr: pchar);

{ bcStrPas converts a pchar to a pascal string }
function bcStrPas(aStr: pchar): string;
{ bcPasStr converts a string to a pchar }
function bcPasStr(aStr: string): pchar;
{ bcFindMatches searches for a pattern in a string, can return multible occurences }
function bcFindMatches(const aPattern,aStr: string;aCaseSensitive: boolean;out aMatches: SizeIntArray;aMatchAll: boolean): boolean;
{ bcMakeStringField produces a string of len length with text s left-, center- or right-aligned,
  padded with padchar }
function bcMakeStringField(const aStr: string;aLen: SizeInt;anAlignment: TTextAlignment;aPadChar: char): string;
{ bcGetStringType returns the type of string we're dealing with alphanumeric etc... }
function bcGetStringType(const aStr: string): TbcStringType;
{ bcStripThousandSeparator returns the value stripped of thousandseparators, StrToCurr doesn't like them! }
function bcStripThousandSeparator(const aStr: string;const aStripChar: char): string;
{ bcCurrencyToString takes a currency value and formats it to a string }
function bcCurrencyToString(aValue: Currency): string;
{ bcStringToCurrency takes a string representation of an amount and returns the value }
function bcStringToCurrency(aStr: string): currency;
var
  bcFormatSettings: TFormatSettings;

implementation
uses CodepagesCommon,LConvEncoding;
// UTF8BOM = #$EF#$BB#$BF;
{ bcStrCase usage....
  case StrCase(S,['+date','+hello','+data','+quit']) of
    0: // '+date'
    1: // '+hello'
    ...
  end; }
{ bcStrCase is zero-based, returns -1 on not found }
function bcStrCase(const S: string;Elements: array of string): ptrint;
var Idx: ptrint;
begin
  Result:= -1; { hence the result ~ ptrint }
  { perform a linear search }
  for Idx:= low(Elements) to high(Elements) do if S = Elements[Idx] then begin
    Result:= Idx;
    break;
  end;
end; { bcStrCase }

function bcCloneString(const aSource: string): string; { duplicate NOT reference }
var Len: cardinal;
begin
  Result:= ''; Len:= Length(aSource);
  if Len > 0 then begin
    SetLength(Result,Len);
    Move(aSource[1],Result[1],Len);
  end;
end; { bcCloneString }

function bcSwapChar(const OldChar,NewChar: char;const S: string): string;
{---------------------------------------------------------------------}
var
  Ch: Char;
  L: integer;
  Source,Dest: pchar;
begin { bcSwapChar }
  L:= length(S);
  SetLength(Result,L);
  Source:= pointer(S);
  Dest:= pointer(Result);
  while L <> 0 do begin
    Ch := Source^;
    if Ch = OldChar then Ch:= NewChar;
    Dest^ := Ch;
    inc(Source);
    inc(Dest);
    dec(L);
  end;
end; { bcSwapChar }

function bcCheckForTrailingPathDelim(const aStr: string): string;
var Len: integer;
begin
  Result:= aStr;
  if aStr <> '' then begin
    Len:= length(aStr);
    while (aStr[Len] = bcPathDelimiter) do dec(Len);
    if Len = length(aStr) then Result:= aStr+bcPathDelimiter
    else SetLength(Result,Len+1); { include one delim, if more were present }
  end;
end; { bcCheckForTrailingPathDelim }

function bcExtractFileExt(const aFileName: string): string;
var
  I: longint;
  EndSep: set of char;
  SOF: boolean; // dot at Start of filename ?

  function bcCharInSet(Ch: ansiChar;Const CSet: TSysCharSet): boolean; inline;
  begin Result:= Ch in CSet; end;

begin
  Result:= '';
  I:= Length(aFileName);
  EndSep:= AllowDirectorySeparators + AllowDriveSeparators + [ExtensionSeparator]; {['\','/','.']}
  while (I > 0) and not bcCharInSet(aFileName[I],EndSep) do dec(I);
  if (I > 0) and (aFileName[I] = ExtensionSeparator) then begin
	  SOF:= (I = 1) or (aFileName[i-1] in AllowDirectorySeparators);
	  if (not SOF) or FirstDotAtFileNameStartIsExtension then Result:= copy(aFileName,I,MaxInt);
	end else Result:= '';
end; { bcExtractFileExt }

function bcGetAppDir: string;
begin
  Result:=  bcCheckForTrailingPathDelim(ExtractFilePath(paramstr(0)));
end; { bcGetAppDir }

function bcShortenFilename(const aFilename: string): string;
var Len,Q1,Q4,Lq,Rest: cardinal;
begin
  Len:= Length(aFilename);
  Lq:= Len div 4;
  Rest:= (Len - (Lq * 4)) + 1; { 0-based math on 1-based string }
  Q1:= 1; Q4:= Lq * 3;
  Result:= copy(aFilename,Q1,Lq) + '\...\' + copy(aFilename,Q4,Lq+Rest);
end; { bcShortenFilename }

function bcGuessEncoding(const aStr: string): word;
var
  Cp: string;
  P,PEnd: pchar; { pend = #0 }
  I: Integer;
begin
  Result:= CPUTF8; { ~ 0 }
  P:= pchar(aStr); PEnd:= P + length(aStr);
  { try UTF-8 BOM (Byte Order Mark) }
  if CompareMem(p,pchar(UTF8BOM),3) then begin
    Result:= CPUTF8BOM; { ~ 4 }
    exit;
  end;
  { try doublebyte unicode BOM FF FE, it's UTF16LE) }
  if (p^=#$FF) and (p[1]=#$FE) then begin Result:= CPUTF16LE; exit; end; { ~ 5 }
  { try UTF-8 (this includes ASCII) }
  repeat
    if ord(P^) < 128 then begin { ASCII }
      if (P^ = #0) and (P >= PEnd) then begin Result:= CPUTF8; exit; end; { ~ 0 }
      inc(p);
    end else begin
      I:= UTF8CodepointStrictSize(P); { returns 1..4 or 0 when p is invalid }
      if I = 0 then begin break; end; { just break loop on error }
      inc(P,I); { increment i character-length bytes ~ 1..4 }
    end;
  until false;
  { if we get here, try to use system encoding }
  Cp:= GetDefaultTextEncoding;
  if NormalizeEncoding(Cp) = EncodingUTF8 then begin
    { the system encoding is UTF-8, but the text is not UTF-8,
      use ISO-8859-1 instead. This encoding has a full 1:1 mapping to unicode,
      so no character is lost during conversion back and forth.
      qts fallback cp is ISO-8859-15 (western europe), i'll use that! }
    Result:= CPISO_8859_15;
  end;
end; { bcGuessEncoding }

(* { codepages we can handle for now }
CPUTF8        = 0; { well duh! }
CPISO_8859_1  = 1; { central europe }
CPISO_8859_15 = 2; { western european languages }
CP1252        = 3; { latin 1 }
CPUTF8BOM     = 4; { first 3 bytes is byte order mark }
CPUTF16LE     = 5; { unicode little endian } *)
function bcEncodeUTF8(const aStr: string;aCodepage: word = CPUTF8): string;
begin                          //ArrayISO_8859_1ToUTF8 ArrayCP1252ToUTF8
  Result:= '';
  case aCodepage of
    0: Result:= aStr;
    1: Result:= SingleByteToUTF8(aStr,ArrayISO_8859_1ToUTF8);
    2: Result:= SingleByteToUTF8(aStr,ArrayISO_8859_15ToUTF8);
    3: Result:= SingleByteToUTF8(aStr,ArrayCP1252ToUTF8);
    4: Result:= UTF8BOMToUTF8(aStr); { just strips the first 3 char #$EF#$BB#$BF ~ UTF8BOM }
    5: Result:= UCS2LEToUTF8(aStr); { little endian }
  end; { case }
end;

function bcDecodeUTF8(const aUtf8Str: string;aCodepage: word = CPISO_8859_15): string;
begin
  Result:= '';
  case aCodepage of
    0: Result:= aUtf8Str;
    1: Result:= UTF8ToISO_8859_1(aUtf8Str,false);
    2: Result:= UTF8ToISO_8859_15(aUtf8Str,false);
    3: Result:= UTF8ToCP1252(aUtf8Str,false);
    4: Result:= UTF8ToUTF8BOM(aUtf8Str);
    5: Result:= UTF8ToUCS2LE(aUtf8Str);
  end; { case }
  //UTF8ToSingleByte(aUtf8Str,@UnicodeToISO_8859_15); ?!? UTF8ToISO_8859_15 UTF8ToCP1252
end;

function bcStrAlloc(aLen: cardinal): pchar;
begin
  inc(aLen,sizeof(cardinal));        // add 4 bytes
  getmem(result,aLen);               // get total memory
  cardinal(pointer(result)^):= aLen; // set length @4 bytes before the actual datapointer
  inc(result,sizeof(cardinal));      // set point result to actual data
end; { bcStrAlloc }

function bcStrNew(aStr: string): pchar;
var
  Len: longint;
begin
  Result:= nil;
  if aStr = '' then exit;
  Len:= Length(aStr)+1; { for the 0# }
  Result:= bcStrAlloc(Len);
  if Result <> nil then move(aStr[1],Result^,len);
end; { bcStrNew }

{ bcStrLength returns the amount of memory allocated for pchar aStr allocated with bcStrAlloc  }
function bcStrLength(aStr: pchar): cardinal;
begin
  if aStr <> nil then Result:= cardinal(pointer(aStr - SizeOf(cardinal))^) - sizeof(cardinal)
  else Result:= 0;
end; { bcStrLength }

function bcStrCopy(aStr: pchar): string;
var L: cardinal;
begin
  L:= bcStrLength(aStr)-1; { skip trailing #0 }
  SetLength(Result,L);
  move(aStr^,Result[1],L); { skip trailing #0 }
end; { bcStrCopy }

procedure bcStrDispose(aStr: pchar);{ bcStrDispose clears the memory allocated with bcStrAlloc }
begin
  if (aStr <> nil) then begin
    dec(aStr,sizeof(cardinal));
    freemem(aStr,cardinal(pointer(aStr)^));
  end;
end; { bcStrDispose }

function bcStrPas(aStr: pchar): string;
begin
  Result:= aStr;
end;

function bcPasStr(aStr: string): pchar;
begin
  Result:= pchar(aStr);
end;

function bcFindMatches(const aPattern, aStr: string;
                       aCaseSensitive: boolean;
                       out aMatches: SizeIntArray;
                       aMatchAll: boolean): boolean;
const
  MATCHESCOUNTRESIZER = 100; //Arbitrary value. Memory used = MATCHESCOUNTRESIZER * sizeof(SizeInt)
var
  MatchesCount: SizeInt; { Stores the amount of occurences found }
  MatchesAllocatedLimit: SizeInt; { Currently allocated space for matches. }

  procedure ResizeAllocatedMatches; { Resizes the allocated space for matches index }
  begin
    MatchesAllocatedLimit:= MatchesCount + MATCHESCOUNTRESIZER;
    SetLength(aMatches,MatchesAllocatedLimit);
  end;

  procedure AddMatch(const aPosition: SizeInt); inline; { Add a match to the result-set }
  begin
    if MatchesCount = MatchesAllocatedLimit then begin
      ResizeAllocatedMatches;
    end;
    aMatches[MatchesCount]:= aPosition;
    inc(MatchesCount);
  end;
var
  lowPattern,LowStr: string;
  I,P,PLen: sizeint;
begin
  MatchesCount:=0;
  MatchesAllocatedLimit:=0;
  SetLength(aMatches,MatchesCount);
  if aPattern = '' then exit;
  case aCaseSensitive of
    false: begin
             lowPattern:= UTF8LowerString(aPattern);
             LowStr:= UTF8LowerString(aStr);
           end;
    true:  begin
             lowPattern:= aPattern;
             LowStr:= aStr;
           end;
  end; { case }
  I:= 1;
  PLen:= UTF8Length(lowPattern);
  P:= UTF8Pos(lowPattern,LowStr,I); { returns the index in the entire string, NOT from offset }
  while P > 0 do begin
    AddMatch(P-1);                   { ~ Pos shoots 1 char past the target ?!? }
    if not aMatchAll then break;
    I:= P+PLen;     { set the new offset to patternlen after the one we found  }
    P:= UTF8Pos(lowPattern,LowStr,I);
  end;
  SetLength(aMatches,MatchesCount);
  Result:= MatchesCount > 0;
end; { bcFindMatches }

function bcMakeStringField(const aStr: string;
                           aLen: SizeInt;
                           anAlignment: TTextAlignment;
                           aPadChar: char): string;
var
  SLen,Idx,I: SizeInt;
  P: pchar;
begin
  if aStr = '' then exit;
  SetLength(Result,aLen); { create our result(buffer) }
  FillChar(Result[1],aLen,aPadChar); { initialize with padding }
  P:= pchar(Result); { point to start of our result }
  SLen:= Length(aStr); { how long is our source string }
  case anAlignment of
    txaLeftJustify:  ; { we're already there }
    txaCenter:       begin { point to middle of result - half of astr }
                       I:= (aLen div 2) - (SLen div 2);
                       inc(P,I); //P+= I;
                     end;
    txaRightJustify: P:= ((P + aLen) - SLen); { point to length of result - length of astr }
  end; { case }
  for Idx:= 1 to SLen do begin
    P^:= aStr[Idx];
    inc(P);
  end;
end; { bcMakeStringField }

function bcGetStringType(const aStr: string): TbcStringType;
var
  P,PEnd: pchar;
  A,N,S: integer;
begin
  Result:= TAlphaNumeric; { best bet is mixed type, also it's very hard to acheive e.g: A=N=S }
  P:= pchar(aStr); PEnd:= P + length(aStr); A:= 0; N:= 0; S:= 0;
  while P < PEnd do begin
    if p^ in ['0'..'9'] then inc(N); { numeric }
    if P^ in ['A'..'Z','a'..'z',' '] then inc(A); { alpha }
    if P^ in [',','.','<','>','-','+','/','(',')','=','?','|',';',':','_','\','*','''','"','!','#'] then inc(S); { symbol }
    inc(P);
  end;
  if ((N > A) and (N > S)) then Result:= TNumeric
  else if ((A > N) and (A > S)) then Result:= TAlpha;
end; { bcGetStringType }

function bcStripThousandSeparator(const aStr: string;const aStripChar: char): string;
var
  Len: integer;
  P,Pe: pchar;
begin
  Result:= ''; Len:= 0;
  if aStr = '' then exit;
  P:= pchar(aStr); Pe:= P + Length(aStr);
  while P < Pe do begin
    if ((P^ <> aStripChar) and (P^ in ['0'..'9',',','-'])) then begin
      Result+= P^;
      inc(Len);
    end;
    inc(P);
  end;
  SetLength(Result,Len);
end; { bcStripThousandSeparator }

function bcCurrencyToString(aValue: Currency): string;
begin
  Result:= CurrToStrF(aValue,ffCurrency,2,bcFormatSettings);
end; { bcCurrencyToString }

function bcStringToCurrency(aStr: string): currency;
begin
  aStr:= bcStripThousandSeparator(aStr,bcFormatSettings.ThousandSeparator);
  Result:= StrToCurr(trim(aStr),bcFormatSettings);
end; { bcStringToCurrency }

{ GetFieldToken allows the user to pick a "field" in a string separated by a token/char }
{ usage: Surname:= GetFieldToken(2,'Benny|Christensen','|') => 'Christensen'  or }
{        Firstname:= GetFieldToken(1,'Benny Christensen',' ') => 'Benny' }
{ FieldNo start at 1 and upwards, returns an empty string on error }
function bcGetFieldToken(const FieldNo: ptrint;
                         const S: string;
                         const Token: char): string;
var
  B,E,C,I,Len: ptrint;
  InField: boolean;
begin
  Len:= system.length(S);                                      { scan sentinel }
  if (Len > 0) and (FieldNo > 0) then begin                 { save clockcycles }
    I:= 0; C:= 1; InField:= false; B:= -1; E:= -1;            { initialization }
    while (C <= Len) do begin
      if (system.copy(S,C,1) = Token) or
         (C = Len) then inc(I);            { check for Token and end of string }
      if (I = FieldNo-1) and not InField then begin        { 0-based by nature }
        B:= C;                             { point b to beginning of substring }
        InField:= true;               { flag field found, now look for the end }
      end;
      if (I = FieldNo) and InField then begin
        E:= C;                                   { point e to end of substring }
        break;                                  { Field found, we're done here }
      end;
      inc(C);                                               { increment cursor }
    end; { while }                                                { continue ? }
    if (B <> -1) and (E <> -1) then begin
      if E = Len then Result:= system.copy(S,B+1,E-B)       { special cases at }
      else if B = 1 then Result:= system.copy(S,B,E-B)     { beginning and end }
      else Result:= system.copy(S,B+1,E-B-1);
    end else Result:= '';                       { return empty string on error }
  end else Result:= '';           { if fed an empty string return it untouched }
end; { bcgetfieldtoken }

initialization
  bcFormatSettings:= DefaultFormatSettings;
  bcFormatSettings.ThousandSeparator:= '.';
  bcFormatSettings.DecimalSeparator:= ',';
  bcFormatSettings.CurrencyString:= '';
finalization
  //
end.
(*
---------> excerpt from syspch.inc & syspchh.inc <------------
{  StrPas converts a PChar to a pascal string  }

function StrPas(Str: PChar): string;
begin
  Result:=Str;
end ;

{  StrAlloc allocates a buffer of Size + 4
   the size of the allocated buffer is stored at result - 4
   StrDispose should be used to destroy the buffer  }

function StrAlloc(Size: cardinal): PChar;
begin
  inc(size,sizeof(cardinal));
  getmem(result,size);
  cardinal(pointer(result)^):=size;
  inc(result,sizeof(cardinal));
end;


{ Allocates a new string using StrAlloc, you need StrDispose to dispose the
  string }

function strnew(p : pchar) : pchar;
var
  len : longint;
begin
  Result:=nil;
  if (p=nil) or (p^=#0) then
   exit;
  len:=strlen(p)+1;
  Result:=StrAlloc(Len);
  if Result<>nil then
   move(p^,Result^,len);
end;


{  StrPCopy copies the pascal string Source to Dest and returns Dest  }

function StrPCopy(Dest: PChar; Const Source: string): PChar;overload;
begin
  result := StrMove(Dest, PChar(Source), length(Source)+1);
end ;

{  StrPLCopy copies MaxLen or less characters from the pascal string
   Source to Dest and returns Dest  }

function StrPLCopy(Dest: PChar; Const Source: string; MaxLen: SizeUInt): PChar;overload;
var Count: SizeUInt;
begin
Result := Dest;
if Result <> Nil then
  begin
    Count := Length(Source);
    if Count > MaxLen then
      Count := MaxLen;
    StrMove(Result, PChar(Source), Count);
    Result[Count] := #0;  { terminate ! }
  end;
end;

{   StrDispose clears the memory allocated with StrAlloc   }

procedure StrDispose(Str: PChar);
begin
  if (Str <> Nil) then
   begin
     dec(Str,sizeof(cardinal));
     Freemem(str,cardinal(pointer(str)^));
   end;
end;

{  StrBufSize returns the amount of memory allocated for pchar Str allocated with StrAlloc  }

function StrBufSize(Str: PChar): Cardinal;
begin
  if Str <> Nil then
   result := cardinal(pointer(Str - SizeOf(cardinal))^)-sizeof(cardinal)
  else
   result := 0;
end ;

*)

