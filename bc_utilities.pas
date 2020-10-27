

{*******************************************************************************
* Unit name : bc_utilities.pas                                                 *
* Copyright : (c) 2020 cdbc.dk                                                 *
* Programmer: Benny Christensen /bc                                            *
* Created   : 2020.05.27 /bc initial version 0.27.05.2020                      *
* Updated   : 2020.05.27 /bc added collection searching, binary & linear       *
*             2020.09.13 /bc added codec for word / ptruint                    *
*                                                                              *
*                                                                              *
*                                                                              *
********************************************************************************
* Abstract:                                                                    *
* Miscalaneous functions and procedures to help                                *
* in development.                                                              *
*                                                                              *
*                                                                              *
*******************************************************************************}

unit bc_utilities;
{$mode objfpc}{$H+}
{$define debug}
interface
uses
  Classes, SysUtils, math;
const
  UnitVersion = '01.27.05.2020';


type
  PPtrInt = ^PtrInt;
  { ptruint / 4*word record }
  PIntRecord = ^TIntRecord;
  TIntRecord = packed record
    b1,b2,b3,b4: word; { 32 bit ~ byte / 64 bit ~ word }
//    b1,b2,b3,b4: byte; { 32 bit ~ byte / 64 bit ~ word }
  end;


  bc_Collection = class(TCollection);
  TSearchCallback = TNotifyEvent;

{ searching collections }
function bcBinarySearch(aValue: ptrint;aCollection: TCollection): ptrint;
function bcLinearSearch(aValue: ptrint;aCollection: TCollection): ptrint;
function bcSearch(aValue: ptrint;aCollection: TCollection): ptrint;
function bcSearchEx(aValue: ptrint;aCollection: TCollection;aCallback: TSearchCallback): ptrint;

{ packing 4 words into a ptruint and unpacking them again }
function EncodePtrUint(const w1, w2, w3, w4: word): ptruint;
function DecodePtrUint(const I: ptruint): TIntRecord;

implementation
const
  { modifiers for date calculations }
  Year_Mod  = $100000;{ ~ 1048576 }
  Month_Mod = $10000; { ~ 65536 }
  Day_Mod   = $100;   { ~ 256 }
  { Version just get added on. }

var lSearchCallback: TSearchCallback;

{ search for an item in a sorted list/array/collection, if not found ~ result = -1
  on very big datasets, it'll need around 20 passes, so on datasets with < 20 items
  a linear search will be quicker }
function bcBinarySearch(aValue: ptrint;aCollection: TCollection): ptrint;
var
  Left, Right, Mid: ptrint;
  MidKey: ptrint;
begin
  Result:= -1; // hence the ptrint                 ok
  Left:= 0;                                     // ok
  Right:= aCollection.Count-1;                  // ok
  while (Left <= Right) do begin                // ok
    Mid:= ((Left + Right) div 2);               // ok
    MidKey:= aCollection.Items[Mid].ID;         // ok
    {$ifdef debug}
      if assigned(lSearchCallback) then begin
        lSearchCallback(aCollection.Items[Mid]);
      end;
    {$endif}
    if (aValue = MidKey) then begin             // ok
      Result:= Mid;                             // ok
      break;                                    // ok
    end;                                        // ok
    if (aValue < MidKey) then Right:= (Mid - 1) // ok
    else Left:= (Mid + 1);                      // ok
  end;
end; { bcBinarySearch }

function bcLinearSearch(aValue: ptrint;aCollection: TCollection): ptrint;
var Idx: ptrint;
begin
  Result:= -1;
  for Idx:= 0 to aCollection.Count-1 do begin
    if aValue = aCollection.Items[Idx].ID then begin
      Result:= Idx;
      break;
    end;
  end;
end;

function bcSearch(aValue: ptrint; aCollection: TCollection): ptrint;
begin
  Result:= -1;
  if aCollection.Count < 20 then begin
    Result:= bcLinearSearch(aValue,aCollection);
  end else begin
    Result:= bcBinarySearch(aValue,aCollection);
  end;
end;

function bcSearchEx(aValue: ptrint; aCollection: TCollection;aCallback: TSearchCallback): ptrint;
begin
  lSearchCallback:= aCallback;
  Result:= -1;
  if aCollection.Count < 20 then begin
    Result:= bcLinearSearch(aValue,aCollection);
  end else begin
    Result:= bcBinarySearch(aValue,aCollection);
  end;
end;

function EncodePtrUint(const w1, w2, w3, w4: word): ptruint;
begin
  Result:= (w4 * Year_Mod) + (w3 * Month_Mod) + (w2 * Day_Mod) + (w1);
end;

function DecodePtrUint(const I: ptruint): TIntRecord;
var
  Rem: ptruint;
begin
  Result.b4:= I div Year_Mod;     { first calculate the first word }
  Rem:= I mod Year_Mod;           { now get the remainder }
  Result.b3:= Rem div Month_Mod;  { calculate the second word }
  Rem:= Rem mod Month_Mod;        { get the remainder }
  Result.b2:= Rem div Day_Mod;    { calculate the third word }
  Result.b1:= Rem mod Day_Mod;    { the rest is the fourth word }
end;

initialization
  lSearchCallback:= nil;
finalization
  lSearchCallback:= nil;

end.

