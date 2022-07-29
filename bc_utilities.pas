

{*******************************************************************************
* Unit name : bc_utilities.pas                                                 *
* Copyright : (c) 2020 - 2022 cdbc.dk                                          *
* Programmer: Benny Christensen /bc                                            *
* Created   : 2020.05.27 /bc initial version 0.27.05.2020                      *
* Updated   : 2020.05.27 /bc added collection searching, binary & linear       *
*             2020.09.13 /bc added codec for word / ptruint                    *
*             2022.06.24 /bc added quicksort routine for integers + compare    *
*             2022.07.01 /bc added intdatecompare routine                      *
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
  Classes, SysUtils;
const
  UnitVersion = '02.01.07.2022';


type
  PPtrInt = ^PtrInt;
  { ptruint / 4*word record }
  PIntRecord = ^TIntRecord;
  TIntRecord = packed record
    b1,b2,b3,b4: word; { 32 bit ~ byte / 64 bit ~ word }
//    b1,b2,b3,b4: byte; { 32 bit ~ byte / 64 bit ~ word }
  end;

  PDateRec = ^TDateRec;
  TDateRec = packed record
    drYear,
    drWeek,
    drDay,
    drMonth: word;
  end;

  PPtrUInt = ^ptruint;

  bc_Collection = class(TCollection);
  TSearchCallback = TNotifyEvent;
  TIntArraySortCompare = function(Item1, Item2: ptruint): longint; // 24.06.2022

{ sorting routines }
procedure SortIntArray(var anArray: array of ptruint); // 25.06.2022

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
{ function prototype for sorting
  TListSortCompare = function(Item1, Item2: pointer): integer;
  TIntArraySortCompare = function(Item1, Item2: ptruint): longint; }

{ sort an array of integer-dates }
function IntDateCompare(Item1, Item2: ptruint): longint; { =^ }
{ 1 = item1 > item2; 0 equal; -1 = item1 < item2 }
begin
  if PDateRec(@Item1)^.drYear > PDateRec(@Item2)^.drYear then Result:= 1
  else if PDateRec(@Item1)^.drYear < PDateRec(@Item2)^.drYear then Result:= -1
  else begin { years equal }
    if PDateRec(@Item1)^.drMonth > PDateRec(@Item2)^.drMonth then Result:= 1
    else if PDateRec(@Item1)^.drMonth < PDateRec(@Item2)^.drMonth then Result:= -1
    else begin { months equal }
      if PDateRec(@Item1)^.drDay > PDateRec(@Item2)^.drDay then Result:= 1
      else if PDateRec(@Item1)^.drDay < PDateRec(@Item2)^.drDay then Result:= -1
      else Result:= 0; { days ~ dates are equal }
    end;
  end;
end; { IntDateCompare }

{ sort an array of integers }
function IntCompare(Item1, Item2: ptruint): longint; { =^ }
{ 1 = item1 > item2; 0 equal; -1 = item1 < item2 }
begin
  if Item1 > Item2 then Result:= 1
  else if Item1 < Item2 then Result:= -1
  else Result:= 0;
end; { IntCompare }

Procedure QuickSortIntArray(var anArray: array of ptruint;L, R: longint;
                            Compare: TIntArraySortCompare);
var
  I, J : longint; // indexes
  P, Q : ptruint; // data
begin
  repeat
    I:= L;
    J:= R;
    P:= anArray[(L + R) div 2];
    repeat
      while Compare(P, anArray[I]) > 0 do I:= I + 1;
      while Compare(P, anArray[J]) < 0 do J:= J - 1;
      If I <= J then begin
        Q:= anArray[I];
        anArray[I]:= anArray[J];
        anArray[J]:= Q;
        I:= I + 1;
        J:= J - 1;
      end;
    until I > J;
    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if J - L < R - I then begin
      if L < J then QuickSortIntArray(anArray,L,J,Compare);
      L:= I;
    end else begin
      if I < R then QuickSortIntArray(anArray,I,R,Compare);
      R:= J;
    end;
  until L >= R;
end; { QuickSortIntArray }

procedure SortIntArray(var anArray: array of ptruint);
begin
  if length(anArray) < 2 then exit; // well duhh, safety check
//  QuickSortIntArray(anArray,low(anArray),high(anArray),@IntCompare);
  QuickSortIntArray(anArray,low(anArray),high(anArray),@IntDateCompare);
end; { SortIntArray }

(*
{ sort a list of pointers }
Procedure QuickSort(FList: PPointerList; L, R : Longint;
                     Compare: TListSortCompare);
var
  I, J : Longint;
  P, Q : Pointer;
begin
 repeat
   I := L;
   J := R;
   P := FList^[ (L + R) div 2 ];
   repeat
     while Compare(P, FList^[i]) > 0 do
       I := I + 1;
     while Compare(P, FList^[J]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FList^[I];
       Flist^[I] := FList^[J];
       FList^[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   // sort the smaller range recursively
   // sort the bigger range via the loop
   // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
   if J - L < R - I then
   begin
     if L < J then
       QuickSort(FList, L, J, Compare);
     L := I;
   end
   else
   begin
     if I < R then
       QuickSort(FList, I, R, Compare);
     R := J;
   end;
 until L >= R;
end;  
*)
  
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

