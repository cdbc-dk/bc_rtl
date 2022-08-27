

            {************************************************
            * Unit name : bc_errorlog_mt.pas                *
            * Copyright : (c) 2020-2022 cdbc.dk             *
            * Programmer: Benny Christensen /bc             *
            * Created   : 2020.05.01 /bc                    *
            * Updated   : 2020.05.01 /bc debugging thread   *
            *             2020.05.01 /bc added fileguard    *
            *             2020.05.02 /bc refactored code    *
            *             2022.08.25 /bc code got on a diet *
            *                            refactoring        *
            *                                               *
            *************************************************
            * Abstract:                                     *
            * ErrorLogMT is a global singleton, that        *
            * provides error-logging for your program.      *
            * No interface, just add bc_errorlog_mt to your *
            * uses clause and post messages to it           *
            ************************************************}

unit bc_errorlog_mt;
{$mode objfpc}{$H+}
{-$define debug}
interface
uses
  Classes, SysUtils, bc_guardian, bc_msgqueue, bc_datetime;
const
  UnitVersion = '02.25.08.2022';
  {$ifdef Linux}
    OSDIRSEPARATOR = '/';
    CRLF = #10;
  {$endif}

type
  { TMsgQEvent is a callback from the message queue }
  TMsgQEvent = procedure(aData: ptrint) of object; { 02.05.2020 /bc }
  { TThreadCallback is a mechanism, that allows the user to run code in a thread, msg = LM_CALLBACK }
  TThreadCallback = procedure(Sender: TThread;Handle: THandle;Data: pointer) of object; { 25.08.2022 /bc }
  { TMessageQThread }
  TMessageQThread = class(TThread)
  protected
    fMsgQueue: TbcMessageQueue;
    fWaitEvent: PRtlEvent;
    fWaitTimeout: ptrint;
    fErrorLog: TObject;
    fHandle: ptrint;
    fOnCallback: TThreadCallback; { 25.08.2022 /bc }
    function GetMsg(var aHandle: ptrint;var aWParam: ptrint;var aLParam: ptrint;var aSParam: string): ptrint; { internal }
    function get_AsPtrint: ptrint; { 02.05.2020 /bc }
    procedure DoLogMsg(const aLogLine: string);
    procedure DoOnCallback; { 02.05.2020 /bc }
    procedure Execute; override;
    { this one is to be overridden instead of execute }
    procedure ExecuteNew(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string); virtual;
  public
    constructor Create(const aHandle: ptrint;const aWaitTimeout: ptrint); { 02.05.2020 /bc }
    destructor Destroy; override;
    procedure PostMsg(aMsg: TbcMessage); overload;
    procedure PostMsg(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string); overload;
    property MsgWaitTimeout: ptrint read fWaitTimeout write fWaitTimeout;
    property OnCallback: TThreadCallback read fOnCallback write fOnCallback; { 02.05.2020 /bc }
    property AsPtrint: ptrint read get_AsPtrint; { 02.05.2020 /bc }
  end; { TMessageQThread }

{ global lock for TMessageQThread, messages only! NO file-mickey stuff }
var
  AppQGuard: TGuardian;
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
      Msg:= fMsgQueue.Dequeue;        { try to get the 1.st message from queue } //bm
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
      ErrorGuard.Lock;                          { enter protected file section }
      try                                    { write error message to log-file }
        TErrorLog(fErrorLog).LogLn('Error in TMessageQThread.GetMsg: '+E.Message);
      finally ErrorGuard.UnLock; end;{ make sure we leave the protected section }
    end;
  end;
end;

function TMessageQThread.get_AsPtrint: ptrint; { 02.05.2020 /bc }
begin
  Result:= ptrint(Self);         { convenience property, for use with callback }
end;

procedure TMessageQThread.DoLogMsg(const aLogLine: string);
begin
  ErrorGuard.Lock;                               { enter protected file section }
  try                               { write string with time stamp to log-file }
    TErrorLog(fErrorLog).LogLn('['+bcTimeToStr(now)+'] '+aLogLine);
  finally ErrorGuard.UnLock; end;    { make sure we leave the protected section }
end;

procedure TMessageQThread.DoOnCallback; { 02.05.2020 /bc }
begin
  if assigned(fOnCallback) then fOnCallback(Self,fHandle,pchar('Dette er en text fra tråden :-)')); { 25.08.2022 /bc }
end;

constructor TMessageQThread.Create(const aHandle: ptrint;const aWaitTimeout: ptrint);
begin
  inherited Create(true);                            { create thread suspended }
  fHandle:= aHandle;                                  { get a handle to the ui }
  fMsgQueue:= TbcMessageQueue.Create;                    { create our own msgq }
  fWaitEvent:= RTLEventCreate;                        { initialize wait object }
  fWaitTimeout:= aWaitTimeout;                            { initialize timeout }
  fErrorLog:= TErrorLog.Create('error.log');{ we don't like exceptions in a thread }
  Start;                                        { don't forget to get us going }
  {$ifdef debug}
  AppQGuard.Lock;                     { enter the application protected section }
  try                                      { send a message to the application }
    MsgQ.EnQueue(TbcMessage.Create(-1,LM_THREAD,-1,ThreadID,'TMessageQThread created'));
  finally AppQGuard.UnLock; end;     { make sure we leave the protected section }
  {$endif}
end;

destructor TMessageQThread.Destroy;
var Msg: TbcMessage;
begin 
  try
    while not fMsgQueue.IsEmpty do begin
      Msg:= fMsgQueue.Dequeue;   { TbcQueue locks, any messages left in queue? }
      Msg.Free;                                              {  then free them }
    end;
    FreeAndNil(fMsgQueue);                                             { Done! }
    RTLeventdestroy(fWaitEvent);                        { finalize wait object }
  except on E:Exception do
    begin
      ErrorGuard.Lock;                           { enter protected file section }
      try                                { write our error message to log-file }
        TErrorLog(fErrorLog).LogLn('Error in TMessageQThread.Destroy: '+E.Message);
      finally ErrorGuard.UnLock; end;{ make sure we leave the protected section }
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
  {$ifdef debug}
  AppQGuard.Lock;                    { enter the application protected section }
  try                                { send a message to the ui, we're running }
    MsgQ.EnQueue(TbcMessage.Create(-1,LM_THREAD,-1,ThreadID,'TMessageQThread is running'));
  finally AppQGuard.UnLock; end;    { make sure we leave the protected section }
  {$endif}
  while not Terminated do try                          { enter our thread-loop }
    case GetMsg(lHandle,lWParam,lLParam,lSParam) of       { check messagequeue }
      LM_LOGMSG  : begin                    { we need to write to the log-file }
                     DoLogMsg(MsgToStr(LM_LOGMSG)+' '+lWParam.ToString+' '+lLParam.ToString+' '+lSParam);
                   end;
      LM_DONE    : begin                      { we've been terminated, bye bye }
                     DoLogMsg('TMessageQThread.Execute: Received LM_DONE, terminating');
                     Terminate;                                         { quit }
                   end;
      LM_CALLBACK: begin
                     DoOnCallback; { call the callback function with self as parameter }
                   end;
      else         begin { this condition basically acts like a "fall through" }
                     ExecuteNew(lHandle,-1,lWParam,lLParam,lSParam); { gets called a LOT }
                   end;
    end;
    Sleep(100);                                        { take a short breather }
  except on E:Exception do
    begin
      ErrorGuard.Lock;                           { enter protected file section }
      try                                { write our error message to log-file }
        TErrorLog(fErrorLog).LogLn('Error in TMessageQThread.Execute -> '+E.Message);
      finally ErrorGuard.UnLock; end;{ make sure we leave the protected section }
    end;
  end;
  {$ifdef debug}
  { it's dangerous to communicate here, as we're shutting down, terminated }
  AppQGuard.Lock;                    { enter the application protected section }
  try                            { send a message to the ui, we're terminating }
    MsgQ.EnQueue(TbcMessage.Create(fHandle,LM_THREAD,-1,ThreadID,'TMessageQThread is terminating'));
  finally AppQGuard.UnLock; end;    { make sure we leave the protected section }
  {$endif}
end;

procedure TMessageQThread.ExecuteNew(aHandle, aMsg, aWParam, aLParam: ptrint;
  aSParam: string);
begin
  // TODO: something, i'm called from execute...
  if not Terminated then begin
//    DoLogMsg('Hello from "ExecuteNew" '+aWParam.ToString+' '+aLParam.ToString+' '+aSParam);
/// do something ///

  end;
// or  while not Terminated do begin
//       { here you can run till the end of days, if you want to }
//     end;
end;

procedure TMessageQThread.PostMsg(aMsg: TbcMessage);
begin
  fMsgQueue.Enqueue(aMsg);        { put the message in our own command queue }
  RtlEventSetEvent(fWaitEvent);                        { trigger wait object }
end;

procedure TMessageQThread.PostMsg(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string);
var Msg: TbcMessage;
begin
  Msg:= TbcMessage.Create(aHandle,aMsg,aWParam,aLParam,aSParam); { well duh! }
  fMsgQueue.Enqueue(Msg);
  RtlEventSetEvent(fWaitEvent);                        { trigger wait object }
end;

initialization
  Singleton:= nil;                              { gets created on demand (jit) }
  AppQGuard:= TGuardian.Create;                             { gets created now }
finalization
  Singleton.Terminate;                          { terminate the running thread }
  Singleton.WaitFor;                { wait for the running thread to terminate }
  FreeAndNil(Singleton); { teardown and "nil" it, my preference over "FreeOnTerminate"}
  FreeAndNil(AppQGuard);                                { teardown and "nil" it}
end.

