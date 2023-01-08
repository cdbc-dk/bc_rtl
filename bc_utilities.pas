

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
  TCollectionItemIDCompare = function(anInt1, anInt2: ptruint): shortint;

{ comparing routines }
function CompareItemIds(anInt1, anInt2: ptruint): shortint;

{ sorting routines }
procedure SortIntArray(var anArray: array of ptruint); // 25.06.2022

{ searching collections }
{ bcBinarySearchCollection looks for a matching TCollectionItem.ID, using a binary algorithm,
  optional user provided compare function, otherwise uses a failsafe. returns -1 on not found }
function bcBinarySearchCollection(aValue: ptrint;aCollection: TCollection;aCompare: TCollectionItemIDCompare): ptrint;
function bcBinarySearch(aValue: ptrint;aCollection: TCollection): ptrint;
{ bcLinearSearchCollection looks for a matching TCollectionItem.ID, using a linear algorithm,
  optional user provided compare function, otherwise uses a failsafe. returns -1 on not found }
function bcLinearSearchCollection(aValue: ptrint;aCollection: TCollection;aCompare: TCollectionItemIDCompare): ptrint;
function bcLinearSearch(aValue: ptrint;aCollection: TCollection): ptrint;
{ bcSearchCollection looks for a matching TCollectionItem.ID, uses linear or binary algorithm,
  optional user provided compare function, otherwise uses a failsafe. returns -1 on not found }
function bcSearchCollection(aValue: ptrint;aCollection: TCollection;aCompare: TCollectionItemIDCompare): ptrint;
function bcSearch(aValue: ptrint;aCollection: TCollection): ptrint;
function bcSearchEx(aValue: ptrint;aCollection: TCollection;aCallback: TSearchCallback): ptrint;

{ packing 4 words into a ptruint and unpacking them again }
function EncodePtrUint(const w1, w2, w3, w4: word): ptruint;
function DecodePtrUint(const I: ptruint): TIntRecord;

{ stream routines }
{ writes the contents of stream 2 to the end of stream 1, with or without a
  separator ~ #10' <----->'#10, 20.08.2022 /bc}
function ConcatenateStreams(aStream1, aStream2: TStream;WithSeparator: boolean = true): boolean;
{ bcLoadFromFileUTF8 loads a stringlist (tstrings) from file and if necessary, converts it to utf8 }
procedure bcLoadFromFileUTF8(aStrings: TStrings;const aFilename: string);
{ bcSaveToFileUTF8 saves a stringlist (tstrings) to a file and writes an utf8bom }
procedure bcSaveToFileUTF8(aStrings: TStrings;const aFilename: string);

implementation
uses bc_advstring;
const
  { modifiers for date calculations }
  Year_Mod  = $100000;{ ~ 1048576 }
  Month_Mod = $10000; { ~ 65536 }
  Day_Mod   = $100;   { ~ 256 }
  { Version just get added on. }

var
  lSearchCallback: TSearchCallback;
  bcUTF8bom: string = #$ef#$bb#$bf;
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

function CompareItemIds(anInt1, anInt2: ptruint): shortint;
{ 1 = anInt1 > anInt2; 0 equal; -1 = anInt1 < anInt2 }
begin
  if anInt1 > anInt2 then Result:= 1
  else if anInt1 < anInt2 then Result:= -1
  else Result:= 0;
end;

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

function bcBinarySearchCollection(aValue: ptrint; aCollection: TCollection;aCompare: TCollectionItemIDCompare): ptrint;
var
  Left, Right, Mid: ptrint;
  MidKey: TCollectionItem;
begin
  Result:= -1; // hence the ptrint                 ok
  if not assigned(aCompare) then aCompare:= @CompareItemIds; { well then we use a failsafe }
  Left:= 0;                                     // low
  Right:= aCollection.Count-1;                  // high
  while (Left <= Right) do begin                // ok
    Mid:= ((Left + Right) div 2);               // mid index
    MidKey:= aCollection.Items[Mid];            // searchkey
    if assigned(MidKey) then begin
      {$ifdef debug} if assigned(lSearchCallback) then lSearchCallback(MidKey); {$endif}
      { compare values  1 = anInt1 > anInt2; 0 equal; -1 = anInt1 < anInt2 }
      case aCompare(aValue,MidKey.ID) of
        -1: Right:= (Mid - 1);                    { ok, we're in the lower half }
        0 : begin                                 { ok, we've found it, }
              Result:= Mid;                       { return index and }
              break;                              { exit stage left }
            end;
        1 : Left:= (Mid + 1);                     { ok, we're in the upper half }
      end; { case }
    end; { assigned }
  end; { while }
end; { bcBinarySearchCollection }

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

function bcLinearSearchCollection(aValue: ptrint; aCollection: TCollection;aCompare: TCollectionItemIDCompare): ptrint;
var
  Idx: ptrint;
  Item: TCollectionItem;
begin
  Result:= -1;
  if not assigned(aCompare) then aCompare:= @CompareItemIds; { well then we use a failsafe }
  for Idx:= 0 to aCollection.Count-1 do begin
    Item:= aCollection.Items[Idx];
    if assigned(Item) then begin
      if aCompare(aValue,Item.ID) = 0 then begin
        Result:= Idx;
        break;
      end;
    end;
  end;
end; { bcLinearSearchCollection }

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

function bcSearchCollection(aValue: ptrint; aCollection: TCollection;aCompare: TCollectionItemIDCompare): ptrint;
begin
  Result:= -1;
  if not assigned(aCompare) then aCompare:= @CompareItemIds; { failsafe }
  if aCollection.Count < 20 then begin
    Result:= bcLinearSearchCollection(aValue,aCollection,aCompare); //bcLinearSearch(aValue,aCollection);
  end else begin
    Result:= bcBinarySearchCollection(aValue,aCollection,aCompare); //bcBinarySearch(aValue,aCollection);
  end;
end; { bcSearchCollection }

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

{ writes the contents of stream 2 to the end of stream 1, with or without a separator ~ #10' <----->'#10 }
function ConcatenateStreams(aStream1, aStream2: TStream;WithSeparator: boolean = true): boolean;
const Separator = #10' <----->'#10;
var
  Buffer: pointer;
  Res,Sz: int64;
begin
  Result:= false;
  Sz:= aStream1.Size;
  Getmem(Buffer,aStream2.Size); //  Buffer:= GetMem(aStream2.Size); // works too
  try
    aStream2.Position:= 0;
    aStream1.Seek(0,soEnd); //    aStream2.ReadBuffer(Buffer^,aStream2.Size); // procedure ~ no result
    Res:= aStream2.Read(Buffer^,aStream2.Size); // function ~ results in bytes read
    if WithSeparator then begin { with separator }
      aStream1.Write(Separator[1],length(Separator)); //    aStream1.WriteBuffer(Separator,length(Separator));
      if Res > 0 then aStream1.WriteBuffer(Buffer^,Res);
      Result:= (Sz + Res + length(Separator)) = aStream1.Size;
    end else begin { without separator }
      if Res > 0 then aStream1.WriteBuffer(Buffer^,Res);
      Result:= (Sz + Res) = aStream1.Size;
    end;
  finally Freemem(Buffer); end;
  aStream2.Position:= 0;
  aStream1.Position:= 0;
end;

procedure bcLoadFromFileUTF8(aStrings: TStrings; const aFilename: string);
var
  Fs: TFileStream;
  Buf: string;
  Cp: word;
begin
  Fs:= TFileStream.Create(aFilename,fmOpenRead or fmShareDenyNone);
  try
    SetLength({%H-}Buf,3);
    Fs.Read(Buf[1],3);
    if Buf = bcUTF8bom then begin
      aStrings.LoadFromStream(Fs);
    end else begin
      Fs.Seek(0,soFromBeginning);
      aStrings.LoadFromStream(Fs);
      Cp:= bcGuessEncoding(aStrings.Text);
      aStrings.Text:= bcEncodeUTF8(aStrings.Text,Cp);
    end;
  finally Fs.Free; end;
end; { bcLoadFromFileUTF8 }

procedure bcSaveToFileUTF8(aStrings: TStrings; const aFilename: string);
var
  Fs: TFileStream;
begin
  Fs:= TFileStream.Create(aFilename,fmCreate or fmOpenWrite);
  try
    Fs.Write(bcUTF8bom[1],length(bcUTF8bom));
    aStrings.SaveToStream(Fs);
  finally Fs.Free; end;
end; { bcSaveToFileUTF8 }

initialization
  lSearchCallback:= nil;
finalization
  lSearchCallback:= nil;

end.

