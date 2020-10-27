unit bc_errorlog;

{$mode objfpc}{$H+}
{$define debugmode}
interface

uses
  Classes, SysUtils, FileUtil,
  bc_guardian,
  bc_types;

const
  { version control }
  UnitVersion = '4.02.05.2020';
  OSDIRSEPARATOR = '/';
  CRLF = #10;

type
  { *** TErrorLog *** }
  TErrorLog = class
  protected
    fFilename: string; { 01.05.2020 /bc }
    fFile: TFileStream;
    fError: string;
    fVersion: TVersion; { 01.05.2020 /bc }
    function CheckFile: boolean; { 01.05.2020 /bc }
    procedure CreateLogfile; { 01.05.2020 /bc }
    procedure OpenLogfile; { 01.05.2020 /bc }
  public
    constructor Create(const aFilename: string = ''); { 29.04.2020 /bc }{ 01.05.2020 /bc }
    destructor Destroy; override;
    procedure LogLn(const anErrorLine: string); { 01.05.2020 /bc }
    property Version: TVersion read fVersion; { 01.05.2020 /bc }
  end;

{ a guardian object for file-ops 01.05.2020 /bc }
var FileGuard: TGuardian;
{ factory creates a singleton errorlog }
function ErrorLog: TErrorLog;

implementation

var   Singleton: TErrorLog;

function ErrorLog: TErrorLog;
begin
	if not assigned(Singleton) then Singleton:= TErrorLog.Create;
	Result:= Singleton;
end;

{ *** TErrorLog *** }
function TErrorLog.CheckFile: boolean; { 01.05.2020 /bc }
begin
  Result:= FileExists(fFilename);
end;

procedure TErrorLog.CreateLogfile; { 01.05.2020 /bc }
begin
  fFile:= TFileStream.Create(fFilename,fmcreate or fmOpenReadWrite or fmShareDenyNone);
end;

procedure TErrorLog.OpenLogfile; { 01.05.2020 /bc }
begin
  fFile:= TFileStream.Create(fFilename,fmOpenReadWrite or fmShareDenyNone);
end;

constructor TErrorLog.Create(const aFilename: string = ''); { 29.04.2020 /bc }
begin
  inherited Create;
  if aFilename = '' then fFilename:= ExtractFilePath(ParamStr(0)) + 'error.log' { 29.04.2020 /bc }
  else fFilename:= ExtractFilePath(ParamStr(0)) + aFilename; { 29.04.2020 /bc }
  if not CheckFile then CreateLogfile else OpenLogfile; { 01.05.2020 /bc }
  fFile.Seek(0,soFromEnd); // appending logs to end of file
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
  S:= anErrorLine+CRLF;
  fFile.Write(S[1],length(S));
end; { TErrorLog }
{ *** TErrorLog *** }

initialization
  Singleton:= nil;
  FileGuard:= TGuardian.Create; { a global variable, this time, 01.05.2020 /bc }
finalization
  FreeAndNil(Singleton);
  FreeAndNil(FileGuard); { 01.05.2020 /bc }
end.

