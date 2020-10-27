{===EZDSLSUP==========================================================

Part of the Delphi Structures Library--supplementary routines.

Copyright (c) 1993-2015, Julian M Bucknall
All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are 
met:

1. Redistributions of source code must retain the above copyright 
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright 
notice, this list of conditions and the following disclaimer in the 
documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its 
contributors may be used to endorse or promote products derived from 
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=====================================================================}

unit ezdslsup;

{$I ezdsldef.inc}
{---Place any compiler options you require here----------------------}


{--------------------------------------------------------------------}
{.$I EzdslOpt.inc}

interface

uses
  SysUtils,
  {$IFDEF Win32}
  Windows,
  {$ENDIF}
  {$IFDEF Linux}
  Types,
  {$ENDIF}
  Classes,
  EzdslCts;

type
  { EZDSL exception class }
  EEZContainerError = class(Exception);

  { EZDSL assertion exception class }
  EEZAssertionError = class(EEZContainerError);

  { EZDSL short strings }
  TEZString = string[255];
  PEZString = ^TEZString;


{---Short string data object routines---}
function EZStrNew(const S : TEZString) : PEZString;      {assign string on heap}
procedure EZStrDispose(PS : PEZString);                   {free string from heap}
function EZStrCompare(Data1, Data2 : pointer) : integer; {compare two strings}
procedure EZStrDisposeData(aData : pointer);              {dispose a string}
function EZStrDupData(aData : pointer) : pointer;        {duplicate a string on heap}

{---ASCIIZ String data object routines---}
function EZStrZCompare(Data1, Data2 : pointer) : integer;{compare two ASCIIZ strings}
procedure EZStrZDisposeData(aData : pointer);             {dispose an ASCIIZ string}
function EZStrZDupData(aData : pointer) : pointer;       {duplicate an ASCIIZ string}

{---Longint data object routines---}
function EZIntCompare(Data1, Data2 : pointer) : integer; {compare two longints}
procedure EZIntDisposeData(aData : pointer);              {'dispose' a longint}
function EZIntDupData(aData : pointer) : pointer;        {'duplicate' a longint}

{---Do nothing routines---}
function EZNoCompare(Data1, Data2 : pointer) : integer;  {always returns 0}
procedure EZNoDisposeData(aData : pointer);               {does nothing}

{---Safe memory alloc/free routines---}
procedure SafeGetMem(var P; BlockSize : Cardinal);
procedure SafeFreeMem(var P; BlockSize : Cardinal);

{---EZDSL exceptions---}
procedure RaiseError(WhatCode : integer);
procedure EZAssert(Proposition : boolean; WhatCode : integer);

implementation

{===EZDSL exceptions=================================================}
procedure RaiseError(WhatCode : integer);
begin
//  raise EEZContainerError.CreateRes(WhatCode.ToString); { 16.05.2020 /bc }
//  raise EEZContainerError.CreateRes(WhatCode);
end;
{--------}
procedure EZAssert(Proposition : boolean; WhatCode : integer);
begin
//  if (Proposition = false) then
//    raise EEZAssertionError.CreateRes(WhatCode.ToString); { 16.05.2020 /bc }
//    raise EEZAssertionError.CreateRes(WhatCode);
end;
{====================================================================}


{===Data object routines=============================================}
function EZStrNew(const S : TEZString) : PEZString;
begin
  SafeGetMem(Result, succ(length(S)));
  PEZString(Result)^ := S;
end;
{--------}
procedure EZStrDispose(PS : PEZString);
begin
  SafeFreeMem(PS, succ(length(PS^)));
end;
{--------}
function EZStrCompare(Data1, Data2 : pointer) : integer;
begin
  Result:= 0; { 16.05.2020 /bc }
end;
(*
{$IFDEF BASM32}
register;
asm
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  xor eax, eax
  or esi, esi
  jnz @@Data1NotNil
  or edi, edi
  jz @@Exit
  dec eax
  jmp @@Exit
@@Data1NotNil:
  or edi, edi
  jz @@GT
  mov ecx, eax
  mov dl, [esi]
  inc esi
  mov dh, [edi]
  inc edi
  mov cl, dl
  cmp cl, dh
  jbe @@CompareStrings
  mov cl, dh
@@CompareStrings:
  or ecx, ecx
  jz @@CompareLengths
  cld
  repe cmpsb
  jb @@LT
  ja @@GT
@@CompareLengths:
  cmp dl, dh
  je @@Exit
  jb @@LT
@@GT:
  inc eax
  inc eax
@@LT:
  dec eax
@@Exit:
  pop edi
  pop esi
end;
{$ELSE}
begin
  if (Data1 = nil) then
    if (Data2 = nil) then
      EZStrCompare := 0
    else
      EZStrCompare := -1
  else
    if (Data2 = nil) then
      EZStrCompare := 1
    else
      asm
        xor ax, ax
        mov cx, ax
        mov dx, ds
        lds si, Data1
        les di, Data2
        mov bl, [si]
        inc si
        mov bh, es:[di]
        inc di
        mov cl, bl
        cmp cl, bh
        jbe @@CompareStrings
        mov cl, bh
      @@CompareStrings:
        or cx, cx
        jz @@CompareLengths
        cld
        repe cmpsb
        jb @@LT
        ja @@GT
      @@CompareLengths:
        cmp bl, bh
        je @@Exit
        jb @@LT
      @@GT:
        inc ax
        inc ax
      @@LT:
        dec ax
      @@Exit:
        mov @Result, ax
        mov ds, dx
      end;
end;
{$ENDIF}
*)
{--------}
procedure EZStrDisposeData(aData : pointer);
begin
  SafeFreeMem(aData, succ(length(PEZString(aData)^)));
end;
{--------}
function EZStrDupData(aData : pointer) : pointer;
begin
  if (aData = nil) then
    Result := nil
  else
    Result := EZStrNew(PEZString(aData)^);
end;
{--------}
function EZStrZCompare(Data1, Data2 : pointer) : integer;
begin
  if (Data1 = nil) then
    if (Data2 = nil) then
      Result := 0
    else
      Result := -1
  else
    if (Data2 = nil) then
      Result := 1
    else
      Result := StrComp(PChar(Data1), PChar(Data2));
end;
{--------}
procedure EZStrZDisposeData(aData : pointer);
begin
  StrDispose(PChar(aData));
end;
{--------}
function EZStrZDupData(aData : pointer) : pointer;
begin
  if (aData = nil) then
    Result := nil
  else
    Result := StrNew(PChar(aData));
end;
{--------}
function EZIntCompare(Data1, Data2 : pointer) : integer;
begin
  Result:= 0; { 16.05.2020 /bc }
end;

(*
{$IFDEF BASM32}
register;
asm
  xor ecx, ecx
  cmp eax, edx
  je @@Exit
  jl @@LT
  inc ecx
  inc ecx
@@LT:
  dec ecx
@@Exit:
  mov eax, ecx
end;
{$ELSE}
assembler;
asm
  xor ax, ax                  {assume equal}
  mov bx, Data1.Word[2]       {get high integer of Data1}
  cmp bx, Data2.Word[2]       {compare with high integer of Data2}
  jl @@LT                     {less than? jump if so}
  jg @@GT                     {greater than? jump if so}
  mov bx, Data1.Word[0]       {get low word of Data1}
  cmp bx, Data2.Word[0]       {compare with low word of Data2}
  je @@Exit                   {equal? exit if so}
  jb @@LT                     {less than? jump if so}
@@GT:                         {get here if Data1 > Data2}
  inc ax                      {set ax to 2}
  inc ax                      {fall through next bit to set ax to 1}
@@LT:                         {get here if Data1 < Data2}
  dec ax                      {decrement ax by 1}
@@Exit:
end;
{$ENDIF}
*)
{--------}
procedure EZIntDisposeData(aData : pointer);
begin
  {do nothing}
end;
{--------}
function EZIntDupData(aData : pointer) : pointer;
begin
  Result := aData;
end;
{--------}
function EZNoCompare(Data1, Data2 : pointer) : integer;
begin
  Result:= 0;
end;
(*
{$IFDEF BASM32}
register;
asm
  xor eax, eax {return 0}
end;
{$ELSE}
assembler;
asm
  xor ax, ax {return 0}
end;
{$ENDIF}
*)
{--------}
procedure EZNoDisposeData(aData : pointer);
begin
  {do nothing}
end;
{====================================================================}


{===SafeFreeMem/SafeGetMem============================================
Allocates and deallocates memory 'safely'. In debug mode SafeGetMem
will fill the allocated block with $CC characters = in the debugger
it will show up data in the block that wasn't initialised, and if
there is a bug such that the block is executed as code you'll get an
automatic breakpoint ($CC = INT $03).
18Jun95 JMB
=====================================================================}
procedure SafeGetMem(var P; BlockSize : Cardinal);
var
  Pt : pointer absolute P;
begin
  GetMem(Pt, BlockSize);
  {$IFDEF DEBUG}
  FillChar(Pt^, BlockSize, $CC);
  {$ENDIF}
end;
{--------}
procedure SafeFreeMem(var P; BlockSize : Cardinal);
var
  Pt : pointer absolute P;
begin
  if Assigned(Pt) then begin
    {$IFDEF DEBUG}
    FillChar(Pt^, BlockSize, $CC);
    {$ENDIF}
    FreeMem(Pt, BlockSize);
    Pt := nil;
  end;
end;
{====================================================================}

end.
