unit tlogger;

{$mode objfpc}{$H+}
{$define FireActionEvents}

interface

uses
  Classes, SysUtils, FileUtil, LCLIntf;

const
  UnitVersion = '2.31.03.2020';
type
  { TThreadLock implements critical sections }
  TThreadLock = class
  private
    fCriticalSection: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LockData;
    procedure UnLockData;
    property CritSect: TRTLCriticalSection read fCriticalSection; // readonly
  end;

  { events to be fired from thread }
  TShowCreatedFileEvent = procedure(AFileName: string) of Object;
  TThreadActionEvent = procedure(aDescription: string) of Object;
  { LogInfo }

  LogInfo = class(TThread)
  private
    fExceptionRaised: Exception;
    fInfo: string;    // field members are by convention / tradition in pascal named "f"+FieldName
    fSuffix: string;  // looks like you've done a bit of PHP ;-)
    fName: string;
    fIsNew: boolean;
    fWTime: boolean;
    fShowCreatedFileEvent: TShowCreatedFileEvent; // procedural variable, but still a fieldmember ie.: "f"+....
    fThreadActionEvent: TThreadActionEvent;
  protected
    procedure Execute; override;
    procedure DoOnLogFileCreated;
    procedure DoOnThreadAction(const anAction: string);
  public
    constructor Create(CreateSuspended: boolean; AInfo : string; ASuffix : string = ''; AFileName : string = ''; WithTimeStamp : boolean = false);
    property OnLogFileCreated: TShowCreatedFileEvent read fShowCreatedFileEvent write fShowCreatedFileEvent;
    property OnThreadAction: TThreadActionEvent read fThreadActionEvent write fThreadActionEvent;
    property ExceptionRaised: Exception read fExceptionRaised;
  end;

function ThreadLock: TThreadLock; // singleton pattern

implementation

var __ThreadLock: TThreadLock;

function ThreadLock: TThreadLock;
begin
  if not assigned(__ThreadLock) then __ThreadLock:= TThreadLock.Create;
  Result:= __ThreadLock;
end;

{ TThreadLock }

constructor TThreadLock.Create;
begin
  inherited;
  System.InitCriticalSection(fCriticalSection); // platform independent
end;

destructor TThreadLock.Destroy;
begin
  System.DoneCriticalsection(fCriticalSection); // platform independent
  inherited Destroy;
end;

procedure TThreadLock.LockData;
begin
  System.EnterCriticalsection(fCriticalSection); // platform independent
end;

procedure TThreadLock.UnLockData;
begin
  System.LeaveCriticalsection(fCriticalSection); // platform independent
end;

constructor LogInfo.Create(CreateSuspended: boolean; AInfo : string; ASuffix : string = ''; AFileName : string = ''; WithTimeStamp : boolean = false);
begin
  // create thread suspended, gives us time to setup some variables before .Execute
  inherited Create(true);
  fInfo:= AInfo;
  fSuffix:= ASuffix;
  fWTime:= WithTimeStamp;
  if AFileName <> '' then fName:= AFileName else begin
    fIsNew:= true;
    fName:= ''; // better safe than sorry
  end;
  FreeOnTerminate:= true;
  // make sure our exception is nil'ed
  fExceptionRaised:= nil;
  // depending on startup params, launch thread :-)
  if not CreateSuspended then Start; // .Resume is deprecated....
end;

procedure LogInfo.Execute;
const
  Log_File_Size = 5 * (1024 * 1024 * 1024);  //in bytes, result in Mb
var
  F : TextFile;
  searchResult  : TSearchRec;
  copied_file   : string;

begin
  {$ifdef FireActionEvents}
    Sleep(15);
    DoOnThreadAction('Logger Thread created, ThreadID = '+inttostr(Self.ThreadID));
  {$endif}
  // check for termination
  if not Terminated then try
    {$ifdef FireActionEvents}
    Sleep(15);
    DoOnThreadAction('Logger Thread executing, ThreadID = '+inttostr(Self.ThreadID));
    {$endif}
    { Create new file }
    if fIsNew then begin
      {$ifdef FireActionEvents}
      Sleep(15);
      DoOnThreadAction('Logger Thread executing - create new file, ThreadID = '+inttostr(Self.ThreadID));
      {$endif}
      case fWTime of
        true : fName:= FormatDateTime('yyyy-mm-dd-hhnnss', Now) + '_' + fSuffix + '.csv';
        false: fName:= fSuffix + '.csv';
      end;
      // write to current file, protect with a threadlock
      ThreadLock.LockData;
      try
        assign(F,fName);
        reWrite(F);
        append(F);
        WriteLn(F,fInfo + #13#10);
        CloseFile(F);
      finally ThreadLock.UnLockData; end;
      // now fire the event, eventhandler will be run in THIS thread ie.: NO MESSING WITH GUI!!!!
      DoOnLogFileCreated;
    end else begin
      {$ifdef FireActionEvents}
      Sleep(15);
      DoOnThreadAction('Logger Thread executing - using existing file, ThreadID = '+inttostr(Self.ThreadID));
      {$endif}
      if FindFirst(fName,faAnyFile,searchResult) = 0 then begin
        if searchResult.Size > Log_File_Size then begin
          {$ifdef FireActionEvents}
          Sleep(15);
          DoOnThreadAction('Logger Thread executing - Logfile too big, creating new, ThreadID = '+inttostr(Self.ThreadID));
          {$endif}
          //rename current file
          ThreadLock.LockData;
          try
          copied_file:= fName + '_' + FormatDateTime('yyyy-mm-dd-hhnnss', Now);
          CopyFile(fName,copied_file);
          if FileExists(copied_file) then DeleteFile(fName);
          if not FileExists(fName) then begin
            Assign(F,fName);    //create new file
            ReWrite(F);
            CloseFile(F);
          end;
          finally ThreadLock.UnLockData; end;
        end;
        ThreadLock.LockData; // protect data
        try
          Assign(F,fName);
          Append(F);
          WriteLn(F,fInfo + #13#10);
          CloseFile(F);
        finally ThreadLock.UnLockData; end;
        FindClose(searchResult); // when you use FindFirst and FindNext, you MUST use FindClose, they allocate resources!
      end;
    end;
  except on E:Exception do begin
    // setup exception property...
    fExceptionRaised:= E;
    {$ifdef FireActionEvents}
    DoOnThreadAction('Logger Thread exeption: '+E.Message+', ThreadID = '+inttostr(Self.ThreadID));
    {$endif}
    end;
  end;
  {$ifdef FireActionEvents}
  Sleep(15);
  DoOnThreadAction('Logger Thread done executing, Bye, ThreadID = '+inttostr(Self.ThreadID));
  {$endif}
end;

procedure LogInfo.DoOnLogFileCreated;
begin
  if assigned(fShowCreatedFileEvent) then fShowCreatedFileEvent(fName);
end;

procedure LogInfo.DoOnThreadAction(const anAction: string);
begin
  if assigned(fThreadActionEvent) then fThreadActionEvent(anAction);
end;

finalization
  FreeAndNil(__ThreadLock);

end.
