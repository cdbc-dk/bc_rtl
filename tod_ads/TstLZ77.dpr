(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstLZ77                                                          *)
(* LZ77 compression test suite                                      *)
(********************************************************************)

program TstLZ77;

{For every file in the current directory: compresses it to C:\LLL.LZA
and decompresses it to C:\LLL.OUT and then verifies that the original
and final files are the same.}

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
  TDBasics,
  TDStrms,
  TDLZHash in 'TDLZHash.pas',
  TDLZSWin in 'TDLZSWin.pas',
  TDLZCmpr in 'TDLZCmpr.pas';

const
  {$IFDEF Kylix1Plus}
  CompressFile = '../compressedfile.test';
  DecompressFile = '../decompressedfile.test';
  {$ELSE}
  CompressFile = 'c:\ENCODE.fil';
  DecompressFile = 'c:\DECODE.fil';
  {$ENDIF}

procedure PackUnpack(const aFileName : string);
var
  InStrm : TFileStream;
  OutStrm : TFileStream;
  InBufStm  : TtdBufferedInputStream;
  OutBufStm : TtdBufferedOutputStream;
  Equal : boolean;
  OrigSize : longint;
  Ratio : double;
begin
  write('Processing ', aFileName);

  {compression}
  InStrm := nil;
  OutStrm := nil;
  InBufStm := nil;
  OutBufStm := nil;
  try
    InStrm := TFileStream.Create(aFileName, fmOpenRead OR fmShareDenyNone);
    OutStrm := TFileStream.Create(CompressFile, fmCreate OR fmOpenReadWrite);
    InBufStm := TtdBufferedInputStream.Create(InStrm);
    OutBufStm := TtdBufferedOutputStream.Create(OutStrm);
    OrigSize := InStrm.Size;
    TDLZCompress(InBufStm, OutBufStm);
  finally
    OutBufStm.Free;
    InBufStm.Free;
    OutStrm.Free;
    InStrm.Free;
  end;

  {decompression}
  InStrm := nil;
  OutStrm := nil;
  InBufStm := nil;
  OutBufStm := nil;
  try
    InStrm := TFileStream.Create(CompressFile, fmOpenRead);
    OutStrm := TFileStream.Create(DecompressFile, fmCreate OR fmOpenReadWrite);
    InBufStm := TtdBufferedInputStream.Create(InStrm);
    OutBufStm := TtdBufferedOutputStream.Create(OutStrm);
    if OrigSize = 0 then
      Ratio := -1
    else
      Ratio := (1 - (InStrm.Size / OrigSize)) * 100.0;
    TDLZDecompress(InBufStm, OutBufStm);
  finally
    OutBufStm.Free;
    InBufStm.Free;
    OutStrm.Free;
    InStrm.Free;
  end;

  {verification}
  InStrm := nil;
  OutStrm := nil;
  try
    InStrm := TFileStream.Create(aFileName, fmOpenRead OR fmShareDenyNone);
    OutStrm := TFileStream.Create(DecompressFile, fmOpenRead);
    Equal := CompareStreams(InStrm, OutStrm);
  finally
    OutStrm.Free;
    InStrm.Free;
  end;

  if Equal then
    writeln('  Passed ', Ratio:5:1, '%')
  else begin
    writeln('  Failed');
    readln;
  end;

end;

var
  Dir : string;
  SR  : TSearchRec;
  Res : integer;
begin
  if (ParamCount = 1) then
    Dir := ParamStr(1)
  else
    Dir := ExtractFilePath(ParamStr(0));
  {$IFDEF Kylix1Plus}
  if (Dir[length(Dir)] <> '/') then
    Dir := Dir + '/';
  Res := FindFirst(Dir + '*', faAnyfile, SR);
  {$ELSE}
  if (Dir[length(Dir)] <> '\') then
    Dir := Dir + '\';
  Res := FindFirst(Dir + '*.*', faAnyfile, SR);
  {$ENDIF}
  while  Res = 0 do begin
    if ((SR.Attr and faDirectory) = 0) then
      PackUnpack(Dir + SR.Name);
    Res := FindNext(SR);
  end;
  FindClose(SR);

  writeln('All done');
  readln;
end.
