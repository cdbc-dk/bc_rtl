(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstNCpy                                                          *)
(* Single producer/many consumer test suite                         *)
(********************************************************************)

program TstNCpy;

{$I TDDefine.inc}

{$IFNDEF Delphi2Plus}
Error! This test program is for Win32 Delphi compilers only
{$ENDIF}

uses
  Forms,
  TstNCpyu in 'TstNCpyu.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
