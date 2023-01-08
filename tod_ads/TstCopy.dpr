(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstCopy                                                          *)
(* Single producer/single consumer test suite                       *)
(********************************************************************)

program TstCopy;

{$I TDDefine.inc}

{$IFNDEF Delphi2Plus}
Error! This test program is for Win32 Delphi compilers only
{$ENDIF}

uses
  Forms,
  TstCopyu in 'TstCopyu.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
