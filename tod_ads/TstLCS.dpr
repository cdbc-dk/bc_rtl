(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstLCS                                                           *)
(* Sort test suite                                                  *)
(********************************************************************)

program TstLCS;

{$I TDDefine.inc}

{$IFNDEF Delphi1}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF Delphi1}
  WinCrt,
  {$ENDIF}
  SysUtils,
  Classes,
  TDLCS;

{$IFDEF Kylix1Plus}
const
  LogFile1 = '../change1.log';
  LogFile2 = '../change2.log';
  LogFile3 = '../change3.log';
  LogFile4 = '../change4.log';
{$ELSE}
const
  LogFile1 = 'c:\change1.log';
  LogFile2 = 'c:\change2.log';
  LogFile3 = 'c:\change3.log';
  LogFile4 = 'c:\change4.log';
{$ENDIF}

var
  StrLCS  : TtdStringLCS;
  FileLCS : TtdFileLCS;

begin
  StrLCS := TtdStringLCS.Create('BEGIN', 'FINISH');
  try
    StrLCS.WriteChanges(LogFile1);
  finally
    StrLCS.Free;
  end;
  writeln('Press enter to continue...');
  readln;


  StrLCS := TtdStringLCS.Create('illiteracy', 'innumeracy');
  try
    StrLCS.WriteChanges(LogFile2);
  finally
    StrLCS.Free;
  end;
  writeln('Press enter to continue...');
  readln;


  StrLCS := TtdStringLCS.Create('algorithms', 'alfresco');
  try
    StrLCS.WriteChanges(LogFile3);
  finally
    StrLCS.Free;
  end;
  writeln('Press enter to continue...');
  readln;

  {$IFNDEF Delphi1}
  FileLCS := TtdFileLCS.Create('TDLCS.~pas', 'TDLCS.pas');
  try
    FileLCS.WriteChanges(LogFile4);
  finally
    FileLCS.Free;
  end;
  {$ENDIF}

  writeln('Done. Press enter...');
  readln;
end.
