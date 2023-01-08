(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstSorts                                                         *)
(* Sort test suite                                                  *)
(********************************************************************)

program TstSorts;

{$I TDDefine.inc}

{$IFNDEF Delphi1}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  Classes,
  {$IFDEF Delphi1}
  WinTypes,
  WinProcs,
  WinCrt,
  {$ENDIF}
  {$IFDEF Delphi2Plus}
  Windows,
  {$ENDIF}
  {$IFDEF Kylix1Plus}
  Types,
  Libc,
  {$ENDIF}
  TDBasics,
  TDTList,
  TDSorts;


procedure BQuickSort(aSortList: TList; L, R: Integer;
  aCompare : TtdCompareFunc);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := aSortList.List^[(L + R) shr 1];
    repeat
      while aCompare(aSortList.List^[I], P) < 0 do Inc(I);
      while aCompare(aSortList.List^[J], P) > 0 do Dec(J);
      if I <= J then
      begin
        T := aSortList.List^[I];
        aSortList.List^[I] := aSortList.List^[J];
        aSortList.List^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then BQuickSort(aSortList, L, J, aCompare);
    L := I;
  until I >= R;
end;

const
  AveCount = 4;

  CountSlow = 4999;
  {$IFDEF Windows}
  CountMed  = 9999;
  CountFast = 15999;
  {$ELSE}
  CountMed  = 99999;
  CountFast = 99999;
  {$ENDIF}

var
  List, List2, List3 : TList;
  i    : longint;
  Oldvalue: longint;

  StartTime, EndTime : DWORD;
  TotalTime : DWORD;

begin
  writeln('Testing Sorts...');
  try
    List := TList.Create;
    List.Capacity := CountFast + 1;
    Randomize;
    List.Clear;
    for i := 0 to CountFast do
      List.Add(pointer(i));

    writeln('SIMPLE SORTS - ', CountSlow+1, ' elements');

    write('bubble sort - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountSlow);
      StartTime := GetTickCount;
      TDBubbleSort(List, 0, CountSlow, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountSlow do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('shaker sort - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountSlow);
      StartTime := GetTickCount;
      TDShakerSort(List, 0, CountSlow, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountSlow do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('selection sort - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountSlow);
      StartTime := GetTickCount;
      TDSelectionSort(List, 0, CountSlow, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountSlow do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('insertion sort - standard - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountSlow);
      StartTime := GetTickCount;
      TDInsertionSortStd(List, 0, CountSlow, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountSlow do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('insertion sort - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountSlow);
      StartTime := GetTickCount;
      TDInsertionSort(List, 0, CountSlow, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountSlow do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    writeln('FASTER SORTS - ', CountMed+1, ' elements');

    write('comb sort - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountMed);
      StartTime := GetTickCount;
      TDCombSort(List, 0, CountMed, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountMed do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('shellsort - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountMed);
      StartTime := GetTickCount;
      TDShellSort(List, 0, CountMed, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountMed do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    writeln('FASTEST SORTS - ', CountFast+1, ' elements');

    write('mergesort - standard - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountFast);
      StartTime := GetTickCount;
      TDMergeSortStd(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('mergesort - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountFast);
      StartTime := GetTickCount;
      TDMergeSort(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('heapsort - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountFast);
      StartTime := GetTickCount;
      TDHeapsort(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('quicksort - standard - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountFast);
      StartTime := GetTickCount;
      TDQuickSortStd(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('quicksort - no recursion - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountFast);
      StartTime := GetTickCount;
      TDQuickSortNoRecurse(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('quicksort - random - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountFast);
      StartTime := GetTickCount;
      TDQuickSortRandom(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('quicksort - median - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountFast);
      StartTime := GetTickCount;
      TDQuickSortMedian(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    writeln(TotalTime div AveCount);
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;

    write('quicksort - fully optimized - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountFast);
      StartTime := GetTickCount;
      TDQuickSort(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    write(TotalTime div AveCount, '  ');
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;
    {-already sorted}
    TotalTime := 0;
    for i := 1 to AveCount do begin
      StartTime := GetTickCount;
      TDQuickSort(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    write(TotalTime div AveCount, '  ');
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;
    {-reverse sorted}
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListReverse(List, 0, CountFast);
      StartTime := GetTickCount;
      TDQuickSort(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    write(TotalTime div AveCount, '  ');
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;
    writeln;

    write('quicksort - borland - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountFast);
      StartTime := GetTickCount;
      BQuickSort(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    write(TotalTime div AveCount, '  ');
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;
    {-already sorted}
    TotalTime := 0;
    for i := 1 to AveCount do begin
      StartTime := GetTickCount;
      BQuickSort(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    write(TotalTime div AveCount, '  ');
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;
    {-reverse sorted}
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListReverse(List, 0, CountFast);
      StartTime := GetTickCount;
      BQuickSort(List, 0, CountFast, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    write(TotalTime div AveCount, '  ');
    OldValue := -1;
    for i := 0 to CountFast do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;
    writeln;

    {make almost all items the same}
    for i:= 0 to CountMed do
      List[i] := pointer(longint(List[i]) mod 50);

    write('quicksort - fully optimized - ');
    TotalTime := 0;
    for i := 1 to AveCount do begin
      TDListShuffle(List, 0, CountMed);
      StartTime := GetTickCount;
      TDQuickSort(List, 0, CountMed, TDCompareLongint);
      EndTime := GetTickCount;
      inc(TotalTime, EndTime-StartTime);
    end;
    write(TotalTime div AveCount, '  ');
    OldValue := -1;
    for i := 0 to CountMed do begin
      if (longint(List[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List[i]);
    end;
    writeln;

    List.Free;

    writeln('test merge');
    List := TList.Create;
    List.Capacity := 100;
    for i := 0 to 99 do
      List.Add(pointer(Random(1000)));
    TDQuickSort(List, 0, pred(List.Count), TDCompareLongint);
    List2 := TList.Create;
    List2.Capacity := 200;
    for i := 0 to 199 do
      List2.Add(pointer(Random(1000)));
    TDQuickSort(List2, 0, pred(List2.Count), TDCompareLongint);

    List3 := TList.Create;
    TDListMerge(List, List2, List3, TDCompareLongint);
    OldValue := -1;
    for i := 0 to pred(List3.Count) do begin
      if (longint(List3[i]) < OldValue) then
        writeln('error at ', i);
      OldValue := longint(List3[i]);
    end;


  except
    on E:Exception do
      writeln(E.Message);
  end;
  writeln('Done');
  readln;
end.
