
{***************************************************************************
*        Unit name : bc_errorlog.pas                                       *
*        Copyright : (C)cdbc.dk 2022                                       *
*        Programmer: Benny Christensen                                     *
*        Created   : 2020.04.29 /bc basic error log singleton.             *
*        Updated   : 2022.08.01 /bc added thread safety.                   *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
****************************************************************************
*        Purpose:                                                          *
*        A class for logging errors and other strings to file. By default  *
*        it places "FileName" in "app_directory". Given no name it will    *
*        default to "app_directory/error.log".                             *
*                                                                          *
*                                                                          *
*                                                                          *
*        TODO:                                                             *
****************************************************************************
*        License:                                                          *
*        "Beer License" - If you meet me one day, you'll buy me a beer :-) *
*        I'm NOT liable for anything! Use at your own risk!!!              *
***************************************************************************}

unit bc_errorlog;

{$mode objfpc}{$H+}
{.$define debug}
interface

uses
  Classes, SysUtils, FileUtil, LMessages, LCLIntf,
  bc_guardian,
  bc_types;

const
  { version control }
  UnitVersion = '5.01.08.2022';
  OSDIRSEPARATOR = '/';
  CRLF = #10;
  { communication messages }
  LM_ERRORLOG_FILENAMECHANGED = LM_USER + 8700;

type
  { *** TErrorLog *** }
  TErrorLog = class
  protected
    fFilename: string; { 01.05.2020 /bc }
    fFile: TFileStream;
    fError: string;
    fVersion: TVersion; { 01.05.2020 /bc }
    function get_Filename: string; virtual;
    procedure set_Filename(aValue: string); virtual;
    function CheckFile: boolean; virtual; { 01.05.2020 /bc }
    procedure CreateLogfile; virtual; { 01.05.2020 /bc }
    procedure OpenLogfile; virtual; { 01.05.2020 /bc }
  public
    constructor Create(const aFilename: string = ''); { 29.04.2020 /bc }{ 01.05.2020 /bc }
    destructor Destroy; override;
    procedure LogLn(const anErrorLine: string); virtual; { 01.05.2020 /bc }
    property Version: TVersion read fVersion; { 01.05.2020 /bc }
    property Filename: string read get_Filename write set_Filename;
  end;

{ a guardian object for file-ops 01.05.2020 /bc }
var ErrorGuard: TGuardian;
{ factory creates a singleton errorlog }
function ErrorLog: TErrorLog;

implementation

var   Singleton: TErrorLog;

function ErrorLog: TErrorLog;
begin
	if not assigned(Singleton) then Singleton:= TErrorLog.Create;
	Result:= Singleton;
end;

function TErrorLog.get_Filename: string;
begin
  if fFilename <> '' then begin
    Result:= ExtractFileName(fFilename);
  end else Result:= fFilename;
end;

procedure TErrorLog.set_Filename(aValue: string);
begin
  if assigned(fFile) then FreeAndNil(fFile);
  if aValue = '' then fFilename:= ExtractFilePath(ParamStr(0)) + 'error.log' { 29.04.2020 /bc }
  else fFilename:= ExtractFilePath(ParamStr(0)) + aValue; { 29.04.2020 /bc }
  if not CheckFile then CreateLogfile else OpenLogfile; { 01.05.2020 /bc }
end;

{ *** TErrorLog *** }
function TErrorLog.CheckFile: boolean; { 01.05.2020 /bc }
begin
  Result:= FileExists(fFilename);
end;

procedure TErrorLog.CreateLogfile; { 01.08.2022 /bc }
begin
  ErrorGuard.Lock;
  try
    fFile:= TFileStream.Create(fFilename,fmcreate or fmOpenReadWrite or fmShareDenyNone);
    fFile.Seek(0,soFromEnd); // appending logs to end of file
  finally ErrorGuard.UnLock; end;
end;

procedure TErrorLog.OpenLogfile; { 01.08.2022 /bc }
begin
  ErrorGuard.Lock;
  try
    fFile:= TFileStream.Create(fFilename,fmOpenReadWrite or fmShareDenyNone);
    fFile.Seek(0,soFromEnd); // appending logs to end of file
  finally ErrorGuard.UnLock; end;
end;

constructor TErrorLog.Create(const aFilename: string = ''); { 29.04.2020 /bc }
begin
  inherited Create;
  set_Filename(aFilename);
  fVersion:= TVersion.Create(UnitVersion); { 01.05.2020 /bc }
end; { TErrorLog }

destructor TErrorLog.Destroy;
begin
  fVersion.Free; { 01.05.2020 /bc }
  fFile.Free;
  inherited Destroy;
end; { TErrorLog }

procedure TErrorLog.LogLn(const anErrorLine: string);
var S: string;
begin
  ErrorGuard.Lock;
  try
    S:= anErrorLine+CRLF;
    fFile.Write(S[1],length(S));
  finally ErrorGuard.UnLock; end;
end; { TErrorLog }
{ *** TErrorLog *** }

initialization
  Singleton:= nil;
  ErrorGuard:= TGuardian.Create; { a global variable, this time, 01.05.2020 /bc }
finalization
  FreeAndNil(Singleton);
  FreeAndNil(ErrorGuard); { 01.05.2020 /bc }
end.

