(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDBasics                                                         *)
(* Standard types and routines throughout library                   *)
(********************************************************************)

unit TDBasics;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes;

{---String table resource constants---}
{$I TDStrRes.inc}

{---Compatibility types for Delphi 1---}
{$IFDEF Delphi1}
type
  DWORD = longint;
  AnsiChar = Char;
  PAnsiChar = PChar;
  ShortString = string[255];
  PShortString = ^ShortString;
{$ENDIF}

type
  TtdCompareFunc = function (aData1, aData2 : pointer) : integer;
  TtdDisposeProc = procedure (aData : pointer);
  TtdUpcaseFunc = function(aCh : AnsiChar) : AnsiChar;
  TtdVisitProc = procedure (aData       : pointer;
                            aExtraData  : pointer;
                        var aStopVisits : boolean);

type
  TtdNameString = string[31];
  PtdCharSet = ^TtdCharSet;
  TtdCharSet = set of AnsiChar;

{---Exceptions for library---}
type
  EtdException = class(Exception);
  EtdAssertion = class(EtdException);
  EtdStreamException = class(EtdException);
  EtdTListException = class(EtdException);
  EtdArrayException = class(EtdException);
  EtdNodeMgrException = class(EtdException);
  EtdLinkListException = class(EtdException);
  EtdStackException = class(EtdException);
  EtdQueueException = class(EtdException);
  EtdDequeException = class(EtdException);
  EtdHashTableException = class(EtdException);
  EtdSkipListException = class(EtdException);
  EtdBinTreeException = class(EtdException);
  EtdRandGenException = class(EtdException);
  EtdLZException = class(EtdException);
  EtdHuffmanException = class(EtdException);
  EtdSplayException = class(EtdException);
  EtdStateException = class(EtdException);
  EtdRegexException = class(EtdException);


{---Example compare routines---}
function TDCompareLongint(aData1, aData2 : pointer) : integer;
function TDCompareNullStr(aData1, aData2 : pointer) : integer;
function TDCompareNullStrText(aData1, aData2 : pointer) : integer;
function TDCompareNullStrANSI(aData1, aData2 : pointer) : integer;
function TDCompareNullStrANSIText(aData1, aData2 : pointer) : integer;
function TDCompareShStr(aData1, aData2 : pointer) : integer;
function TDCompareShStrText(aData1, aData2 : pointer) : integer;
function TDCompareShStrANSI(aData1, aData2 : pointer) : integer;
function TDCompareShStrANSIText(aData1, aData2 : pointer) : integer;

{---Example dispose routines---}
procedure TDDisposeObject(aData : pointer);

{---Character/string routines---}
function TDPosCh(aCh : AnsiChar; const S : string) : integer;
function TDChExists(aCh : AnsiChar; const S : string) : boolean;
function TDIsDigit(aCh : AnsiChar) : boolean;

{$IFDEF Delphi1}
{---Handy routines for Delphi 1 found in later Delphis---}
function CompareMem(const aBuf1, aBuf2 : pointer; aCount : word)
                                                            : boolean;
procedure SetLength(var aSt : string; aLen : integer);
{$ENDIF}

{$IFNDEF HasAssert}
{---Assertions for Delphi 1 and 2---}
procedure Assert(aCondition : boolean; aMsg : string);
{$ENDIF}

{$IFDEF Delphi2}
{---System exceptions for Delphi2---}
procedure RaiseLastWin32Error;
{$ENDIF}

{$IFDEF Kylix1Plus}
{---GetTickCount for Kylix---}
function GetTickCount : longword;
{$ENDIF}

implementation

uses
  {$IFDEF Delphi1}
  WinTypes, WinProcs;
  {$ENDIF}
  {$IFDEF Delphi2Plus}
  Windows;
  {$ENDIF}
  {$IFDEF Kylix1Plus}
  Types, Libc;
  {$ENDIF}

{$IFDEF Delphi1}
{$R TDStrRes.r16}
{$ENDIF}
{$IFDEF Delphi2Plus}
{$R TDStrRes.r32}
{$ENDIF}
{$IFDEF Kylix1Plus}
{$R TDStrRes.r32}
{$ENDIF}

const
  UnitName = 'TDBasics';

{===Example compare routines=========================================}
function TDCompareLongint(aData1, aData2 : pointer) : integer;
var
  L1 : longint absolute aData1;
  L2 : longint absolute aData2;
begin
  if (L1 < L2) then
    Result := -1
  else if (L1 = L2) then
    Result := 0
  else
    Result := 1
end;
{--------}
function TDCompareNullStr(aData1, aData2 : pointer) : integer;
begin
  Result := StrComp(PAnsiChar(aData1), PAnsiChar(aData2));
end;
{--------}
function TDCompareNullStrText(aData1, aData2 : pointer) : integer;
begin
  Result := StrIComp(PAnsiChar(aData1), PAnsiChar(aData2));
end;
{--------}
function TDCompareNullStrANSI(aData1, aData2 : pointer) : integer;
begin
  {$IFDEF Delphi1}
  Result := lstrcmp(PAnsiChar(aData1), PAnsiChar(aData2));
  {$ENDIF}
  {$IFDEF Delphi2Plus}
  Result := CompareString(LOCALE_USER_DEFAULT, 0,
                          PAnsiChar(aData1), -1,
                          PAnsiChar(aData2), -1) - 2;
  {$ENDIF}
  {$IFDEF Kylix1Plus}
  Result := strcoll(PAnsiChar(aData1), PAnsiChar(aData2));
  {$ENDIF}
end;
{--------}
function TDCompareNullStrANSIText(aData1, aData2 : pointer) : integer;
begin
  {$IFDEF Delphi1}
  Result := lstrcmpi(PAnsiChar(aData1), PAnsiChar(aData2));
  {$ENDIF}
  {$IFDEF Delphi2Plus}
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                          PAnsiChar(aData1), -1,
                          PAnsiChar(aData2), -1) - 2;
  {$ENDIF}
  {$IFDEF Kylix1Plus}
//  !!!!! To do
  {$ENDIF}
end;
{--------}
function TDCompareShStr(aData1, aData2 : pointer) : integer;
{$IFDEF Delphi1}
begin
  Result := CompareStr(PShortString(aData1)^, PShortString(aData1)^);
end;
{$ENDIF}
{$IFNDEF Delphi1}
var
  NullStr1 : array [0..255] of AnsiChar;
  NullStr2 : array [0..255] of AnsiChar;
begin
  {be careful *not* to call any long string routines}
  Move(PShortString(aData1)^[1], NullStr1[0], length(PShortString(aData1)^));
  NullStr1[length(PShortString(aData1)^)] := #0;
  Move(PShortString(aData2)^[1], NullStr2[0], length(PShortString(aData2)^));
  NullStr2[length(PShortString(aData2)^)] := #0;
  Result := StrComp(NullStr1, NullStr2);
end;
{$ENDIF}
{--------}
function TDCompareShStrText(aData1, aData2 : pointer) : integer;
{$IFDEF Delphi1}
begin
  Result := CompareText(PShortString(aData1)^, PShortString(aData1)^);
end;
{$ENDIF}
{$IFNDEF Delphi1}
var
  NullStr1 : array [0..255] of AnsiChar;
  NullStr2 : array [0..255] of AnsiChar;
begin
  {be careful *not* to call any long string routines}
  Move(PShortString(aData1)^[1], NullStr1[0], length(PShortString(aData1)^));
  NullStr1[length(PShortString(aData1)^)] := #0;
  Move(PShortString(aData2)^[1], NullStr2[0], length(PShortString(aData2)^));
  NullStr2[length(PShortString(aData2)^)] := #0;
  Result := StrIComp(NullStr1, NullStr2);
end;
{$ENDIF}
{--------}
function TDCompareShStrANSI(aData1, aData2 : pointer) : integer;
{$IFDEF Delphi1}
var
  NullStr1 : array [0..255] of AnsiChar;
  NullStr2 : array [0..255] of AnsiChar;
begin
  StrPCopy(NullStr1, PShortString(aData1)^);
  StrPCopy(NullStr2, PShortString(aData2)^);
  Result := lstrcmp(NullStr1, NullStr2);
end;
{$ENDIF}
{$IFDEF Delphi2Plus}
begin
  Result := CompareString(LOCALE_USER_DEFAULT, 0,
                          @PShortString(aData1)^[1],
                             length(PShortString(aData1)^),
                          @PShortString(aData2)^[1],
                             length(PShortString(aData2)^)) - 2;
end;
{$ENDIF}
{$IFDEF Kylix1Plus}
var
  NullStr1 : array [0..255] of AnsiChar;
  NullStr2 : array [0..255] of AnsiChar;
begin
  {be careful *not* to call any long string routines}
  Move(PShortString(aData1)^[1], NullStr1[0], length(PShortString(aData1)^));
  NullStr1[length(PShortString(aData1)^)] := #0;
  Move(PShortString(aData2)^[1], NullStr2[0], length(PShortString(aData2)^));
  NullStr2[length(PShortString(aData2)^)] := #0;
  Result := strcoll(NullStr1, NullStr2);
end;
{$ENDIF}
{--------}
function TDCompareShStrANSIText(aData1, aData2 : pointer) : integer;
{$IFDEF Delphi1}
var
  NullStr1 : array [0..255] of AnsiChar;
  NullStr2 : array [0..255] of AnsiChar;
begin
  StrPCopy(NullStr1, PShortString(aData1)^);
  StrPCopy(NullStr2, PShortString(aData2)^);
  Result := lstrcmpi(NullStr1, NullStr2);
end;
{$ENDIF}
{$IFDEF Delphi2Plus}
var
  Str1 : PShortString absolute aData1;
  Str2 : PShortString absolute aData2;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                          @PShortString(aData1)^[1],
                             length(PShortString(aData1)^),
                          @PShortString(aData2)^[1],
                             length(PShortString(aData2)^)) - 2;
end;
{$ENDIF}
{$IFDEF Kylix1Plus}
begin
//!!!! To do
end;
{$ENDIF}
{====================================================================}


{===Example dispose routines=========================================}
procedure TDDisposeObject(aData : pointer);
begin
  TObject(aData).Free;
end;
{====================================================================}


{===Character/string routines========================================}
function TDChExists(aCh : AnsiChar; const S : string) : boolean;
var
  i : integer;
begin
  Result := true;
  for i := 1 to length(S) do
    if (S[i] = aCh) then
      Exit;
  Result := false;
end;
{--------}
function TDIsDigit(aCh : AnsiChar) : boolean;
begin
  Result := ('0' <= aCh) and (aCh <= '9');
end;
{--------}
function TDPosCh(aCh : ANsiChar; const S : string) : integer;
var
  i : integer;
begin
  Result := 0;
  for i := 1 to length(S) do
    if (S[i] = aCh) then begin
      Result := i;
      Exit;
    end;
end;
{====================================================================}


{$IFDEF Delphi1}
{===Delphi1 routines=================================================}
function CompareMem(const aBuf1, aBuf2 : pointer; aCount : word)
                                                            : boolean;
assembler;
asm
  mov bx, ds        {save ds}
  xor al, al        {al=0 (false)}
  mov cx, aCount    {get count}
  jcxz @@Equal      {if count=0, buffers are equal}
  lds si, aBuf1     {get first buffer}
  les di, aBuf2     {get second buffer}
  cld               {forwards!}
  repe cmpsb        {compare buffers}
  jne @@Exit        {if not equal, exit}
@@Equal:            {if equal...}
  inc al            {...set result to 1 (true)}
@@Exit:             {exit}
  mov ds, bx        {restore ds}
end;
{--------}
procedure SetLength(var aSt : string; aLen : integer);
begin
  aSt[0] := chr(aLen);
end;
{====================================================================}
{$ENDIF}

{$IFNDEF HasAssert}
{===Assertions for Delphi 1 and 2====================================}
procedure Assert(aCondition : boolean; aMsg : string);
begin
  if not aCondition then
    raise EtdAssertion.Create(aMsg);
end;
{====================================================================}
{$ENDIF}

{$IFDEF Delphi2}
{===System exceptions for Delphi2====================================}
procedure RaiseLastWin32Error;
var
  LastError : DWORD;
begin
  LastError := GetLastError;
  if (LastError <> ERROR_SUCCESS) then
    raise Exception.Create(Format('Got Win32 error: %d', [LastError]))
  else
    raise Exception.Create('Unknown Win32 error');
end;
{====================================================================}
{$ENDIF}


{$IFDEF Kylix1Plus}
{===GetTickCount for Kylix===========================================}
function GetTickCount : longword;
var
  TV : TTimeVal;
  TZ : TTimeZone;
begin
  gettimeofday(TV, TZ);
  Result := ((longword(TV.tv_sec) mod (24*60*60)) * 1000) +
            (longword(TV.tv_usec) div 1000);
end;
{====================================================================}
{$ENDIF}


end.
