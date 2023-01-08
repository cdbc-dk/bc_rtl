(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstRand                                                          *)
(* Random number test suite                                         *)
(********************************************************************)

program TstRand;

{$I TDDefine.inc}

uses
  {$IFDEF Kylix1Plus}
  QForms,
  {$ELSE}
  Forms,
  {$ENDIF}
  TstRndU1 in 'TstRndU1.pas' {Form1},
  TstRndU3 in 'TstRndU3.pas';

{$R *.res}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
