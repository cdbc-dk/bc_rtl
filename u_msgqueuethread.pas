unit u_msgqueuethread;

{$mode objfpc}{$H+}
{$define debugmode}
interface

uses
  Classes, SysUtils, mtlinklist, u_errorlog;

const
 {$ifdef Linux}
   OSDIRSEPARATOR = '/';
   CRLF = #10;
 {$endif}
 {$ifdef win32}
   OSDIRSEPARATOR = '\';
   CRLF = #13#10;
 {$endif}
 { message defines for use with TBaseThread with messagequeue }
 TM_NOMSG = -1000;
 TM_WRITEFILE = -999;
 TM_TICKCOUNT = -998;

type
  PThreadMsg = ^TThreadMsg;
  TThreadMsg = record
    lMsg: longint;
    sParam: string[255];
    pParam: ptrint;
  end;

  { TBaseThread }
  TBaseThread = class(TThread)
  protected
    fMsgQueue: TaaQueue;
    fWaitEvent: PRtlEvent;
    fWaitTimeout: longint;
    function NewMsg: PThreadMsg;
    function GetMsg(var sParam: string;var pParam: ptrint): longint; { internal }
    procedure DoExecute; virtual; { this is the method to override in descendants }
  public
    constructor Create(const aWaitTimeout: longint);
    destructor Destroy; override;
    procedure Execute; override;
    procedure PostMsg(aMsg: longint;sParam: string;pParam: ptrint);
    property MsgWaitTimeout: longint read fWaitTimeout write fWaitTimeout;
  end;

implementation

{ TBaseThread }

function TBaseThread.NewMsg: PThreadMsg;
begin
  Result:= GetMem(SizeOf(TThreadMsg));
  FillChar(Result^,SizeOf(TThreadMsg),0);
end;

constructor TBaseThread.Create(const aWaitTimeout: longint);
begin
  inherited Create(true);
  fMsgQueue:= TaaQueue.Create;
  fWaitEvent:= RTLEventCreate; // initialize wait object
  fWaitTimeout:= aWaitTimeout;
  FreeOnTerminate:= true;
end;

destructor TBaseThread.Destroy;
var
  Msg: PThreadMsg;
begin try
  while not fMsgQueue.IsEmpty do begin
    Msg:= fMsgQueue.Dequeue;
    Freemem(Msg);
  end;
  FreeAndNil(fMsgQueue);
  RTLeventdestroy(fWaitEvent); // finalize wait object
  except on E:Exception do
    ErrorLog.LogLn('Error in TBaseThread.Destroy: '+E.Message);
  end;
  inherited Destroy;
end;

procedure TBaseThread.Execute;
begin
  while not Terminated do try
    DoExecute;
  except on E:Exception do
    ErrorLog.LogLn('Error in TBaseThread.Execute -> '+E.Message);
  end;
end;

procedure TBaseThread.PostMsg(aMsg: longint; sParam: string; pParam: ptrint);
var
  Msg: PThreadMsg;
begin
  Msg:= NewMsg;
  Msg^.lMsg:= aMsg;
  Msg^.sParam:= sParam;
  Msg^.pParam:= pParam;
  fMsgQueue.Enqueue(Msg);
  RtlEventSetEvent(fWaitEvent); // trigger wait object
end;

function TBaseThread.GetMsg(var sParam: string; var pParam: ptrint): longint;
var
  Msg: PThreadMsg;
begin
  try  // wait until event is set by msg in queue or timeout
    if fMsgQueue.IsEmpty then RtlEventWaitFor(fWaitEvent,fWaitTimeout);
    if not fMsgQueue.IsEmpty then begin
      Msg:= fMsgQueue.Dequeue;
      Result:= Msg^.lMsg;
      sParam:= Msg^.sParam;
      pParam:= Msg^.pParam;
      Freemem(Msg);
      RTLeventResetEvent(fWaitEvent);
    end else begin
      Result:= TM_NOMSG; // -1000
      sParam:= '';
      pParam:= 0;
    end;
  except on E:Exception do
    ErrorLog.LogLn('Error in TBaseThread.GetMsg: '+E.Message);
  end;
end;

procedure TBaseThread.DoExecute;
var
  S: string;
  pI: ptrint;
begin
  S:= ''; pI:= 0;
  case GetMsg(S,pI) of
    TM_WRITEFILE: ErrorLog.LogLn('TBaseThread.DoExecute: TM_WRITEFILE -> '+S+' ['+inttostr(pI)+']');
    TM_TICKCOUNT: ErrorLog.LogLn('TBaseThread.DoExecute: TM_TICKCOUNT -> '+S+' ['+inttostr(pI)+']');
//    TM_NOMSG: ErrorLog.LogLn('TBaseThread.DoExecute: TM_NOMSG -> '+S+' ['+inttostr(pI)+']');
  end;
end;
end.

