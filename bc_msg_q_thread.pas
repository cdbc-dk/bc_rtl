

            {************************************************
            * Unit name : bc_msg_q_thread.pas                *
            * Copyright : (c) 2020 cdbc.dk                  *
            * Programmer: Benny Christensen /bc             *
            * Created   : 2020.05.01 /bc                    *
            * Updated   : 2020.05.01 /bc debugging thread   *
            *             2020.05.01 /bc added fileguard    *
            *             2020.05.02 /bc refactored code    *
            *                                               *
            *************************************************
            * Abstract:                                     *
            * ErrorLogMT is a global singleton, that        *
            * provides error-logging for your program.      *
            * No interface, just add bc_errorlog_mt to your *
            * uses clause and pass lines to log.            *
            ************************************************}

unit bc_msg_q_thread;
{$mode objfpc}{$H+}
{$define debug}
interface
uses
  Classes, SysUtils,
  bc_guardian,
  bc_msgqueue,
  bc_datetime;

const
  UnitVersion = '01.02.05.2020';
  {$ifdef Linux}
    OSDIRSEPARATOR = '/';
    CRLF = #10;
  {$endif}

type
  { TMsgQEvent is a callback from the message queue }
  TMsgQEvent = procedure(aData: ptrint) of object; { 02.05.2020 /bc }
  { TMessageQThread }
  TMessageQThread = class(TThread)
  protected
    fMsgQueue: TbcMessageQueue;
    fWaitEvent: PRtlEvent;
    fWaitTimeout: ptrint;
    fErrorLog: TObject;
    fHandle: ptrint;
    fOnCallback: TMsgQEvent; { 02.05.2020 /bc }
    function GetMsg(var aHandle: ptrint;var aWParam: ptrint;var aLParam: ptrint;var aSParam: string): ptrint; { internal }
    function get_AsPtrint: ptrint; { 02.05.2020 /bc }
    procedure DoLogMsg(const aLogLine: string);
    procedure DoOnCallback; { 02.05.2020 /bc }
  public
    constructor Create(const aHandle: ptrint;const aWaitTimeout: ptrint); { 02.05.2020 /bc }
    destructor Destroy; override;
    procedure Execute; override;
    procedure PostMsg(aMsg: TbcMessage); overload;
    procedure PostMsg(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string); overload;
    property MsgWaitTimeout: ptrint read fWaitTimeout write fWaitTimeout;
    property OnCallback: TMsgQEvent read fOnCallback write fOnCallback; { 02.05.2020 /bc }
    property AsPtrint: ptrint read get_AsPtrint; { 02.05.2020 /bc }
  end;

{ global lock for TMessageQThread, messages only! NO file-mickey stuff }
var
  MsgQGuard: TGuardian;
{ ErrorLogMt is a global logging mechanism, just add it to your uses clause
  and it will run in the background, use the "PostMsg" to send messages to
  it. Important: remember to enable "cthreads" in the project file }
function ErrorLogMT: TMessageQThread;

implementation
uses
  bc_errorlog;

var Singleton: TMessageQThread;

{ factory }
function ErrorLogMT: TMessageQThread;
begin
  if not assigned(Singleton) then Singleton:= TMessageQThread.Create(-1,1000);
  Result:= Singleton;
end;

{ *** TMessageQThread *** }
function TMessageQThread.GetMsg(var aHandle: ptrint;var aWParam: ptrint;var aLParam: ptrint;var aSParam: string): ptrint; { internal }
var Msg: TbcMessage;
begin
  try                     { wait until event is set by msg in queue or timeout }
    if fMsgQueue.IsEmpty then RtlEventWaitFor(fWaitEvent,fWaitTimeout);
    if not fMsgQueue.IsEmpty then begin  { are there any messages in our queue }
      MsgQGuard.Lock;                                { enter protected section }
      try
        Msg:= fMsgQueue.Dequeue;      { try to get the 1.st message from queue }
      finally MsgQGuard.UnLock; end;{ make sure we leave the protected section }
      aHandle:= Msg.Handle;           { assign msg properties to variables and }
      Result:= Msg.Msg;                      { the msg is returned in "Result" }
      aWParam:= Msg.WParam;
      aLParam:= Msg.LParam;
      aSParam:= Msg.SParam;
      Msg.Free;                    { remember to free the message, users duty! }
      RTLeventResetEvent(fWaitEvent);                  { reset our wait-object }
    end else begin                          { "Result" = -1 equals "no message"}
      aHandle:= 0;
      Result:= -1;
      aWParam:= 0;
      aLParam:= 0;
      aSParam:= '';
    end;
  except on E:Exception do
    begin
      FileGuard.Lock;                           { enter protected file section }
      try                                    { write error message to log-file }
        TErrorLog(fErrorLog).LogLn('Error in TMessageQThread.GetMsg: '+E.Message);
      finally FileGuard.UnLock; end;{ make sure we leave the protected section }
    end;
  end;
end;

function TMessageQThread.get_AsPtrint: ptrint; { 02.05.2020 /bc }
begin
  Result:= ptrint(Self);         { convenience property, for use with callback }
end;

procedure TMessageQThread.DoLogMsg(const aLogLine: string);
begin
  FileGuard.Lock;                               { enter protected file section }
  try                               { write string with time stamp to log-file }
    TErrorLog(fErrorLog).LogLn('['+bcTimeToStr(now)+'] '+aLogLine);
  finally FileGuard.UnLock; end;    { make sure we leave the protected section }
end;

procedure TMessageQThread.DoOnCallback; { 02.05.2020 /bc }
begin
  if assigned(fOnCallback) then fOnCallback(Self.AsPtrint); { 02.05.2020 /bc }
end;

constructor TMessageQThread.Create(const aHandle: ptrint;const aWaitTimeout: ptrint);
begin
  inherited Create(true);                            { create thread suspended }
  fHandle:= aHandle;                                  { get a handle to the ui }
  fMsgQueue:= TbcMessageQueue.Create;                    { create our own msgq }
  fWaitEvent:= RTLEventCreate;                        { initialize wait object }
  fWaitTimeout:= aWaitTimeout;                            { initialize timeout }
  fErrorLog:= TErrorLog.Create('msg_q.log');{ we don't like exceptions in a thread }
  FreeOnTerminate:= true;                                       { conveinience }
  Start;                                        { don't forget to get us going }
  Guardian.Lock;                     { enter the application protected section }
  try                                      { send a message to the application }
    MsgQ.EnQueue(TbcMessage.Create(-1,LM_THREAD,-1,ThreadID,'TMessageQThread created'));
  finally Guardian.UnLock; end;     { make sure we leave the protected section }
end;

destructor TMessageQThread.Destroy;
var Msg: TbcMessage;
begin 
  try
    MsgQGuard.Lock;                          { enter protected message section }
    try
      while not fMsgQueue.IsEmpty do begin
        Msg:= fMsgQueue.Dequeue;  { any messages left in queue, then free them }
        Msg.Free;
      end;
    finally MsgQGuard.UnLock; end;  { make sure we leave the protected section }
    FreeAndNil(fMsgQueue);                                             { Done! }
    RTLeventdestroy(fWaitEvent);                        { finalize wait object }
  except on E:Exception do
    begin
      FileGuard.Lock;                           { enter protected file section }
      try                                { write our error message to log-file }
        TErrorLog(fErrorLog).LogLn('Error in TMessageQThread.Destroy: '+E.Message);
      finally FileGuard.UnLock; end;{ make sure we leave the protected section }
    end;
  end;
  FreeAndNil(TErrorLog(fErrorLog));                 { we own it, so we free it }
  inherited Destroy;                                   { call ancester destroy }
end;

procedure TMessageQThread.Execute;
var
  lHandle,lWParam,lLParam: ptrint;
  lSParam: string;
begin
  Guardian.Lock;                     { enter the application protected section }
  try                                { send a message to the ui, we're running }
    MsgQ.EnQueue(TbcMessage.Create(-1,LM_THREAD,-1,ThreadID,'TMessageQThread is running'));
  finally Guardian.UnLock; end;     { make sure we leave the protected section }
  while not Terminated do try                          { enter our thread-loop }
    case GetMsg(lHandle,lWParam,lLParam,lSParam) of       { check messagequeue }
      LM_LOGMSG: begin                      { we need to write to the log-file }
                   DoLogMsg(MsgToStr(LM_LOGMSG)+' '+lWParam.ToString+' '+lLParam.ToString+' '+lSParam);
                 end;
      LM_DONE    : begin                      { we've been terminated, bye bye }
                     DoLogMsg('TMessageQThread.Execute: Received LM_DONE, terminating');
                     Terminate;                                           { quit }
                   end;
      LM_CALLBACK: begin
                     DoOnCallback; { call the callback function with self as parameter }
                   end;
    end;
    Sleep(100);                                        { take a short breather }
  except on E:Exception do
    begin
      FileGuard.Lock;                           { enter protected file section }
      try                                { write our error message to log-file }
        TErrorLog(fErrorLog).LogLn('Error in TMessageQThread.Execute -> '+E.Message);
      finally FileGuard.UnLock; end;{ make sure we leave the protected section }
    end;
  end;
  Guardian.Lock;                     { enter the application protected section }
  try                            { send a message to the ui, we're terminating }
    MsgQ.EnQueue(TbcMessage.Create(fHandle,LM_THREAD,-1,ThreadID,'TMessageQThread is terminating'));
  finally Guardian.UnLock; end;     { make sure we leave the protected section }
end;

procedure TMessageQThread.PostMsg(aMsg: TbcMessage);
begin
  MsgQGuard.Lock;                  { enter the message queue protected section }
  try
    fMsgQueue.Enqueue(aMsg);        { put the message in our own command queue }
    RtlEventSetEvent(fWaitEvent);                        { trigger wait object }
  finally MsgQGuard.UnLock; end;    { make sure we leave the protected section }
end;

procedure TMessageQThread.PostMsg(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string);
var Msg: TbcMessage;
begin
  MsgQGuard.Lock;                  { enter the message queue protected section }
  try                               { put the message in our own command queue }
    Msg:= TbcMessage.Create(aHandle,aMsg,aWParam,aLParam,aSParam); { well duh! }
    fMsgQueue.Enqueue(Msg);
    RtlEventSetEvent(fWaitEvent);                        { trigger wait object }
  finally MsgQGuard.UnLock; end;    { make sure we leave the protected section }
end;

initialization
  Singleton:= nil;                              { gets created on demand (jit) }
  MsgQGuard:= TGuardian.Create;                             { gets created now }
finalization
  Singleton.Terminate;                          { terminate the running thread }
  Singleton.WaitFor;                { wait for the running thread to terminate }
  FreeAndNil(Singleton);                                { teardown and "nil" it}
  FreeAndNil(MsgQGuard);                                { teardown and "nil" it}
end.

