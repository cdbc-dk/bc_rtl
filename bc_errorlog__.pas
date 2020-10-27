unit bc_errorlog;

{$mode objfpc}{$H+}
{$define debugmode}
interface

uses
  Classes, SysUtils, FileUtil, bc_guardian;

const
  { version control }
  UnitVersion = '2.31.03.2020';
  {$ifdef Linux}
    OSDIRSEPARATOR = '/';
    CRLF = #10;
  {$endif}
  {$ifdef win32}
    OSDIRSEPARATOR = '\';
    CRLF = #13#10;
  {$endif}

type
  { TErrorLog }
  TErrorLog = class
  protected
    fFile: TFileStream;
    fError: string;
  public
    constructor Create(const aFilename: string = '');
    destructor Destroy; override;
    procedure LogLn(const anErrorLine: string);
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
{ TErrorLog }
constructor TErrorLog.Create(const aFilename: string = '');
var Filename: string;
begin
  inherited Create;
  Filename:= ExtractFilePath(ParamStr(0)) + 'error.log';
  fFile:= TFileStream.Create(Filename,fmcreate or fmOpenReadWrite or fmShareDenyNone);
  fFile.Seek(0,soFromEnd); // appending logs to end of file
end; { TErrorLog }

destructor TErrorLog.Destroy;
begin
  fFile.Free;
  inherited Destroy;
end; { TErrorLog }

procedure TErrorLog.LogLn(const anErrorLine: string);
var S: string;
begin
  S:= anErrorLine+CRLF;
  fFile.Write(S[1],length(S));
end; { TErrorLog }

initialization
  Singleton:= nil;
  FileGuard:= TGuardian.Create;
finalization
  FreeAndNil(Singleton);
  FreeAndNil(FileGuard);
end.

