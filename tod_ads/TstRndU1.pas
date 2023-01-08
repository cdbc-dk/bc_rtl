(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstRndU1                                                         *)
(* Random number test suite - user interface                        *)
(********************************************************************)

unit TstRndU1;

{$I TDDefine.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF Kylix1Plus}
  QGraphics, QForms, QStdCtrls, QControls, QExtCtrls,
  {$ELSE}
  Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  {$ENDIF}
  TDRandom, TstRndU3;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    Results: TLabel;
    Button2: TButton;
    ShowChi: TCheckBox;
    Image1: TImage;
    Label2: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    RandGen : TtdBasePRNG;
  public
    { Public declarations }
    procedure PrintChiSqaureResults(TestName    : string;
                                    ChiSquare   : double;
                                    DegsFreedom : integer);
  end;

var
  Form1: TForm1;

implementation

{$IFDEF Kylix1Plus}
{$R *.xfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TForm1.FormActivate(Sender: TObject);
begin
  Listbox1.ItemIndex := 0;
  Image1.Canvas.FillRect(Rect(0, 0, Image1.Width, Image1.Height));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Clear;
  Image1.Canvas.FillRect(Rect(0, 0, Image1.Width, Image1.Height));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ChiSquare   : double;
  DegsFreedom : integer;
  i : longint;
  X, Y : double;
  XInt, YInt : integer;
begin
  if (ListBox1.ItemIndex <> -1) then begin
    case ListBox1.ItemIndex of
      0 : begin
            RandGen := TtdSystemPRNG.Create(0);
            TtdSystemPRNG(RandGen).Randomize;
          end;  
      1 : RandGen := TtdMinStandardPRNG.Create(0);
      2 : RandGen := TtdCombinedPRNG.Create(0, 0);
      3 : RandGen := TtdAdditiveGenerator.Create(0);
      4 : RandGen := TtdShuffleGenerator.Create(0);
    end;
    try
      Memo1.Text := Memo1.Text +
                    Format('===%s===', [ListBox1.Items[ListBox1.ItemIndex]]) + ^M^J;
      {the uniformity test}
      UniformityTest(RandGen, ChiSquare, DegsFreedom);
      PrintChiSqaureResults('Uniformity Test', ChiSquare, DegsFreedom);
      {the various gap tests}
      GapTest(RandGen, 0.0, 0.5, ChiSquare, DegsFreedom);
      PrintChiSqaureResults('Gap Test (0.0 - 0.5)', ChiSquare, DegsFreedom);
      GapTest(RandGen, 0.5, 1.0, ChiSquare, DegsFreedom);
      PrintChiSqaureResults('Gap Test (0.5 - 1.0)', ChiSquare, DegsFreedom);
      GapTest(RandGen, 0.0, 1.0/3.0, ChiSquare, DegsFreedom);
      PrintChiSqaureResults('Gap Test (0.0 - 0.33)', ChiSquare, DegsFreedom);
      GapTest(RandGen, 1.0/3.0, 2.0/3.0, ChiSquare, DegsFreedom);
      PrintChiSqaureResults('Gap Test (0.33 - 0.67)', ChiSquare, DegsFreedom);
      GapTest(RandGen, 2.0/3.0, 1.0, ChiSquare, DegsFreedom);
      PrintChiSqaureResults('Gap Test (0.67 - 1.0)', ChiSquare, DegsFreedom);
      {the poker test}
      PokerTest(RandGen, ChiSquare, DegsFreedom);
      PrintChiSqaureResults('Poker Test', ChiSquare, DegsFreedom);
      {the coupon collectors test}
      CouponCollectorsTest(RandGen, ChiSquare, DegsFreedom);
      PrintChiSqaureResults('Coupon Collectors Test', ChiSquare, DegsFreedom);

      Image1.Canvas.FillRect(Rect(0, 0, Image1.Width, Image1.Height));
      {$IFDEF Kylix1Plus}
      Image1.Canvas.Pen.Color := clRed;
      {$ENDIF}
      for i := 1 to 2000000 do begin
        X := RandGen.AsDouble;
        Y := RandGen.AsDouble;
        if X < 0.001 then begin
          XInt := Trunc(X * 1000.0 * Image1.Width);
          YInt := Image1.Height - Trunc(Y * Image1.Height);
          {$IFDEF Kylix1Plus}
          Image1.Canvas.DrawPoint(XInt, YInt);
          {$ELSE}
          Image1.Canvas.Pixels[XInt, YInt] := clRed;
          {$ENDIF}
        end;
      end;
    finally
      RandGen.Free;
    end;
  end;
end;

procedure TForm1.PrintChiSqaureResults(TestName    : string;
                                       ChiSquare   : double;
                                       DegsFreedom : integer);
var
  TestResult : string;
begin
  if (ChiTable5[DegsFreedom] <= ChiSquare) and
     (ChiSquare <= ChiTable95[DegsFreedom]) then
    TestResult := 'Success'
  else
    TestResult := '**FAILED at 5% level**';

  if ShowChi.Checked then
    Memo1.Text := Memo1.Text +
                  Format('%s', [TestName]) + ^M^J +
                  Format('[Chi-Square %f, DegsFreedom %d]',
                         [ChiSquare, DegsFreedom]) + ^M^J +
                  TestResult + ^M^J
  else
    Memo1.Text := Memo1.Text +
                  Format('%s: %s', [TestName, TestResult]) + ^M^J;
end;

end.
