(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstRndU3                                                         *)
(* Random number test routines                                      *)
(********************************************************************)

unit TstRndU3;

{$I TDDefine.inc}

interface

uses
  TDRandom;

{$I TDChiTbl.inc}

procedure UniformityTest(RandGen     : TtdBasePRNG;
                     var ChiSquare   : double;
                     var DegsFreedom : integer);

procedure GapTest(RandGen      : TtdBasePRNG;
                  Lower, Upper : double;
              var ChiSquare    : double;
              var DegsFreedom  : integer);

procedure PokerTest(RandGen     : TtdBasePRNG;
                var ChiSquare   : double;
                var DegsFreedom : integer);

procedure CouponCollectorsTest(RandGen     : TtdBasePRNG;
                           var ChiSquare   : double;
                           var DegsFreedom : integer);

implementation

const
  UniformityCount = 30000;
  UniformityIntervals = 100;
  GapBucketCount = 10;
  GapsCount = 30000;
  PokerCount = 30000;
  CouponCount = 30000;

function IntPower(X  : double; N : integer) : double;
  begin
    case N of
      0 : IntPower := 1.0;
      1 : IntPower := X;
      2 : IntPower := Sqr(X);
    else
      if Odd(N) then
        IntPower := Sqr(IntPower(X, N div 2)) * X
      else
        IntPower := Sqr(IntPower(X, N div 2));
    end;{case}
  end;

function Stirling(N, M : integer) : double;
  {-Calculate type-2 Stirling number, n & m are both assumed >= 0}
  var
    Work1, Work2 : double;
  begin
    if (N < M) then
      Stirling := 0.0
    else if (N = M) then
      Stirling := 1.0
    else
      case m of
        0 : Stirling := 0.0;
        1 : Stirling := 1.0;
        2 : Stirling := IntPower(2.0, n-1) - 1.0;
      else
        Work1 := Stirling(n-1, m);
        Work2 := Stirling(n-1, m-1);
        Stirling := (Work1 * m) + Work2;
      end;{case}
  end;

{====================================================================)
The uniformity test.
The random numbers are partitioned into a number of equally sized
buckets between 0.0 and 1.0. On the average, each bucket should have
the same number of random numbers; ie they should be evenly spread
over the range [0.0, 0.1). Apply Chi-Squared test to the buckets.
(====================================================================}
procedure UniformityTest(RandGen     : TtdBasePRNG;
                     var ChiSquare   : double;
                     var DegsFreedom : integer);
var
  BucketNumber,
  i : integer;
  Expected, ChiSqVal : double;
  Bucket : array [0..pred(UniformityIntervals)] of integer;
begin
  {Fill buckets}
  FillChar(Bucket, sizeof(Bucket), 0);
  for i := 0 to pred(UniformityCount) do begin
    BucketNumber := trunc(RandGen.AsDouble * UniformityIntervals);
    inc(Bucket[BucketNumber]);
  end;
  {calc chi squared}
  Expected := UniformityCount / UniformityIntervals;
  ChiSqVal := 0.0;
  for i := 0 to pred(UniformityIntervals) do
    ChiSqVal := ChiSqVal + (Sqr(Expected - Bucket[i]) / Expected);
  {return values}
  ChiSquare := ChiSqVal;
  DegsFreedom := pred(UniformityIntervals);
end;

{====================================================================)
The gap test.
Each random number is tested to be in the range Lower..Upper. If it
is a value of 1 is assigned, if not 0 is assigned. You'll get a stream
of 0's and 1's. The lengths of the runs of 0's are then counted. These
lengths are then bucketed, you'll get lengths of 0 upwards. These
lengths are the 'gaps' between 1's. Apply Chi-Squared test to the
buckets.
(====================================================================}
procedure GapTest(RandGen      : TtdBasePRNG;
                  Lower, Upper : double;
              var ChiSquare    : double;
              var DegsFreedom  : integer);
var
  NumGaps  : integer;
  GapLen   : integer;
  i        : integer;
  p        : double;
  Expected : double;
  ChiSqVal : double;
  R        : double;
  Bucket   : array [0..pred(GapBucketCount)] of integer;
begin
  {calc gaps and fill buckets}
  FillChar(Bucket, sizeof(Bucket), 0);
  GapLen := 0;
  NumGaps := 0;
  while (NumGaps < GapsCount) do begin
    R := RandGen.AsDouble;
    if (Lower <= R) and (R < Upper) then begin
      if (GapLen >= GapBucketCount) then
        GapLen := pred(GapBucketCount);
      inc(Bucket[GapLen]);
      inc(NumGaps);
      GapLen := 0;
    end
    else
      if (GapLen < GapBucketCount) then
        inc(GapLen);
  end;
  p := Upper - Lower;
  ChiSqVal := 0.0;
  {do all but the last bucket}
  for i := 0 to GapBucketCount-2 do begin
    Expected := p * IntPower(1-p, i) * NumGaps;
    ChiSqVal := ChiSqVal + (Sqr(Expected - Bucket[i]) / Expected);
  end;
  {do the last bucket}
  i := pred(GapBucketCount);
  Expected := IntPower(1-p, i) * NumGaps;
  ChiSqVal := ChiSqVal + (Sqr(Expected - Bucket[i]) / Expected);
  {return values}
  ChiSquare := ChiSqVal;
  DegsFreedom := pred(GapBucketCount);
end;


{====================================================================)
The poker test.
The random numbers are grouped into 'hands' of 5, and the numbers are
converted into a digit from 0..9. The number of different digits in
each hand is then counted (1..5), and this result is bucketed. Because
the probability of only one digit repeated 5 times is so low, it is
grouped into the 2-different-digit category. Apply Chi-Squared test to
the buckets.
(====================================================================}
procedure PokerTest(RandGen     : TtdBasePRNG;
                var ChiSquare   : double;
                var DegsFreedom : integer);
var
  i, j,
  BucketNumber,
  NumFives : integer;
  Accum, Divisor,
  Expected, ChiSqVal : double;
  Bucket : array [0..4] of integer;
  Flag : array [0..9] of boolean;
  p : array [0..4] of double;
begin
  {prepare}
  FillChar(Bucket, sizeof(Bucket), 0);
  NumFives := PokerCount div 5;
  {calc probabilities for each bucket, algorithm from Knuth}
  Accum := 1.0;
  Divisor := IntPower(10.0, 5);
  for i := 0 to 4 do begin
    Accum := Accum * (10.0 - i);
    p[i] := Accum * Stirling(5, succ(i)) / Divisor;
  end;
  {for each group of five random numbers, convert all five to a
   number between 1 and 10, count the number of different digits}
  for i := 1 to NumFives do begin
    FillChar(Flag, sizeof(Flag), 0);
    for j := 1 to 5 do begin
      Flag[trunc(RandGen.AsDouble * 10.0)] := true;
    end;
    BucketNumber := -1;
    for j := 0 to 9 do
      if Flag[j] then inc(BucketNumber);
    inc(Bucket[BucketNumber]);
  end;

  {Accumulate the first bucket into the second, do calc separately -
   it'll be the sum of the 'all the same' and 'two different digits'
   buckets}
  inc(Bucket[1], Bucket[0]);
  Expected := (p[0]+p[1]) * NumFives;
  ChiSqVal := Sqr(Expected - Bucket[1]) / Expected;
  {write the other buckets}
  for i := 2 to 4 do begin
    Expected := p[i] * NumFives;
    ChiSqVal := ChiSqVal + (Sqr(Expected - Bucket[i]) / Expected);
  end;
  {return values}
  ChiSquare := ChiSqVal;
  DegsFreedom := 3;
end;


{====================================================================)
The coupon collectors test.
The random numbers are read one by one, converted into a number from
0 to 4. The length of the sequence required to get a complete set of
the digits 0..4 is counted, this will vary from 5 upwards. Once a full
set is obtained, start over. Bucket the lengths of these sequences.
Apply Chi-Squared test to the buckets.
(====================================================================}
procedure CouponCollectorsTest(RandGen     : TtdBasePRNG;
                           var ChiSquare   : double;
                           var DegsFreedom : integer);
var
  NumSeqs, LenSeq, NumVals, NewVal,
  i : integer;
  Expected, ChiSqVal : double;
  Bucket : array [5..20] of integer;
  Occurs : array [0..4] of boolean;
  p : array [5..20] of double;
begin
  {calc probabilities for each bucket, algorithm from Knuth}
  p[20] := 1.0;
  for i := 5 to 19 do begin
    p[i] := (120.0 * Stirling(i-1, 4)) / IntPower(5.0, i);
    p[20] := p[20] - p[i];
  end;
  {an alternative to calculate the last probability value:
    p[last] := 1.0 - ((120.0 * Stirling(last-1, 5)) / IntPower(5.0, last-1)); }

  NumSeqs := 0;
  FillChar(Bucket, sizeof(Bucket), 0);
  while (NumSeqs < CouponCount) do begin
    {keep getting coupons (ie random numbers) until we have collected
     all five}
    LenSeq := 0;
    NumVals := 0;
    FillChar(Occurs, sizeof(Occurs), 0);
    repeat
      inc(LenSeq);
      NewVal := trunc(RandGen.AsDouble * 5);
      if not Occurs[NewVal] then begin
        Occurs[NewVal] := true;
        inc(NumVals);
      end;
    until (NumVals = 5);
    {update the relevant bucket depending on the number of coupons we
     had to collect}
    if (LenSeq > 20) then
      LenSeq := 20;
    inc(Bucket[LenSeq]);
    inc(NumSeqs);
  end;

  {calculate chisquare value}
  ChiSqVal := 0.0;
  for i := 5 to 20 do begin
    Expected := p[i] * NumSeqs;
    ChiSqVal := ChiSqVal + (Sqr(Expected - Bucket[i]) / Expected);
  end;

  {return values}
  ChiSquare := ChiSqVal;
  DegsFreedom := 15;
end;

end.
