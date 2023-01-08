
unit bc_msgqthread;
{$mode objfpc}{$H+}
{-$define debug}
interface
uses
  Classes, SysUtils,
  bc_guardian,
  bc_msgqueue, Masks,
  bc_datetime;

const
  UnitVersion = '01.26.08.2022';
  {$ifdef Linux}
    OSDIRSEPARATOR = '/';
    CRLF = #10;
  {$endif}

type
  { a callback mechanism to allow the user to run code in a thread, message = LM_CALLBACK }
  TThreadCallback = procedure(Sender: TThread;Handle: THandle;Data: pointer) of object; { 25.08.2022 /bc }
  { TMsgQThread }
  TMsgQThread = class(TThread)
  protected
    fMsgQueue: TbcMessageQueue;
    fWaitEvent: PRtlEvent;
    fWaitTimeout: ptrint;
    fThrottle: word;
    fHandle: ptrint;
    fOnCallback: TThreadCallback; { 25.08.2022 /bc }
    function GetMsg(var aHandle: ptrint;var aWParam: ptrint;var aLParam: ptrint;var aSParam: string): ptrint; { internal }
    function get_Throttle: word; virtual; { 25.08.2022 /bc }
    procedure set_Throttle(aValue: word); virtual; { 25.08.2022 /bc }
    procedure DoLogMsg(aMsg: ptrint;const aLogLine: string); virtual;
    procedure DoOnCallback; virtual;{ 02.05.2020 /bc }
    procedure Execute; override;
    { this one is to be overridden instead of execute }
    procedure ExecuteNew(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string); virtual;
    procedure TearDown; virtual;
  public
    constructor Create(const aHandle: ptrint;const aWaitTimeout: ptrint); { 02.05.2020 /bc }
    destructor Destroy; override;
    { API }
    function ThreadMsgInfo: string; virtual; { call this to get the message names i understand }
    procedure PostMsg(aMsg: TbcMessage); overload;
    procedure PostMsg(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string); overload;
    property MsgWaitTimeout: ptrint read fWaitTimeout write fWaitTimeout;
    property OnCallback: TThreadCallback read fOnCallback write fOnCallback; { 02.05.2020 /bc }
    { throttle regulates speed in steps ~ 0..10 where 10 is full throttle }
    property Throttle: word read get_Throttle write set_Throttle; { 26.08.2022 /bc }
  end;

{ global lock for TMessageQThread, messages only! NO file-mickey stuff,
  you'll find the global message queue in bc_msgqueue.MsgQ singleton }
var
  AppQGuard: TGuardian;
{ Important: remember to enable "cthreads" in the project file }
//function MsgQThread: TMsgQThread;

implementation
//uses bc_errorlog;

var
  Singleton: TMsgQThread;
  Started: boolean;

{ factory }
function MsgQThread: TMsgQThread;
begin
  if not assigned(Singleton) then begin
    Singleton:= TMsgQThread.Create(-1,1000);
    Singleton.Start; { remember to get us going }
    Started:= true;
  end;
  Result:= Singleton;
end;

{ *** TMsgQThread *** }
procedure TMsgQThread.set_Throttle(aValue: word);
begin
  if aValue < 0 then aValue:= 0 else if aValue > 10 then aValue:= 10;
  AppQGuard.Lock;
  try
    fThrottle:= 10 - aValue;
  finally AppQGuard.UnLock; end;
end;

function TMsgQThread.get_Throttle: word;
begin
  AppQGuard.Lock;
  try
    Result:= 10 - fThrottle;
  finally AppQGuard.UnLock; end;
end;

function TMsgQThread.GetMsg(var aHandle: ptrint;var aWParam: ptrint;var aLParam: ptrint;var aSParam: string): ptrint; { internal }
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

    end;
  end;
end;

procedure TMsgQThread.DoLogMsg(aMsg: ptrint;const aLogLine: string);
begin
  // for now do nothing
end;

procedure TMsgQThread.DoOnCallback; { 02.05.2020 /bc }
begin
  if assigned(fOnCallback) then fOnCallback(Self,fHandle,nil); { 25.08.2022 /bc }
end;

constructor TMsgQThread.Create(const aHandle: ptrint;const aWaitTimeout: ptrint);
begin
  inherited Create(true);                            { create thread suspended }
  fHandle:= aHandle;                                  { get a handle to the ui }
  fMsgQueue:= TbcMessageQueue.Create;                    { create our own msgq }
  fWaitEvent:= RTLEventCreate;                        { initialize wait object }
  fWaitTimeout:= aWaitTimeout;                            { initialize timeout }
  fThrottle:= 5;                                   { right smack in the middle }
//  Start;                                        { don't forget to get us going }
end;

destructor TMsgQThread.Destroy;
begin
  TearDown;                                { dismantles our part of the thread }
  inherited Destroy;                                   { call ancester destroy }
end;

function TMsgQThread.ThreadMsgInfo: string;
begin
  Result:= 'LM_LOGMSG,LM_DONE,LM_CALLBACK';
end;

procedure TMsgQThread.Execute;
var
  lHandle,lWParam,lLParam: ptrint;
  lSParam: string;
begin
  while not Terminated do try                          { enter our thread-loop }
    case GetMsg(lHandle,lWParam,lLParam,lSParam) of       { check messagequeue }
      LM_LOGMSG  : begin                    { we need to write to the log-file }
                     fHandle:= lHandle;                      { just to be sure }
                     DoLogMsg(LM_LOGMSG,MsgToStr(LM_LOGMSG)+' '+lWParam.ToString+' '+lLParam.ToString+' '+lSParam);
                   end;
      LM_DONE    : begin                      { we've been terminated, bye bye }
                     DoLogMsg(LM_DONE,'TMsgQThread.Execute: Received LM_DONE, terminating');
                     Terminate;                                         { quit }
                   end;
      LM_CALLBACK: begin
                     fHandle:= lHandle;                      { just to be sure }
                     DoOnCallback; { call the callback function with self as parameter }
                   end;
      else         begin { this condition basically acts like a "fall through" }
                     ExecuteNew(lHandle,-1,lWParam,lLParam,lSParam); { gets called a LOT }
                   end;
    end;
    Sleep(25*fThrottle);    { take a short breather, 0..250 ms, in 25 ms steps }
  except on E:Exception do
    begin
      // add the exception to a stringlist...
    end;
  end;
end;

procedure TMsgQThread.ExecuteNew(aHandle, aMsg, aWParam, aLParam: ptrint;
  aSParam: string);
begin
  // TODO: something, i'm called from execute...
  if not Terminated then begin
//    DoLogMsg('Hello from "ExecuteNew" '+aWParam.ToString+' '+aLParam.ToString+' '+aSParam);
/// do something ///
  end;
end;

procedure TMsgQThread.TearDown;
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
      // for now do nothing
    end;
  end;
end;

procedure TMsgQThread.PostMsg(aMsg: TbcMessage);
begin
  fMsgQueue.Enqueue(aMsg);        { put the message in our own command queue }
  RtlEventSetEvent(fWaitEvent);                        { trigger wait object }
end;

procedure TMsgQThread.PostMsg(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string);
var Msg: TbcMessage;
begin
  Msg:= TbcMessage.Create(aHandle,aMsg,aWParam,aLParam,aSParam); { well duh! }
  fMsgQueue.Enqueue(Msg);
  RtlEventSetEvent(fWaitEvent);                        { trigger wait object }
end;

initialization
  Singleton:= nil;                              { gets created on demand (jit) }
  Started:= false;                                           { just a reminder }
  AppQGuard:= TGuardian.Create;                             { gets created now }
finalization
//  Singleton.Terminate;                          { terminate the running thread }
//  Singleton.WaitFor;                { wait for the running thread to terminate }
//  FreeAndNil(Singleton); { teardown and "nil" it, my preference over "FreeOnTerminate"}
  FreeAndNil(AppQGuard);                                { teardown and "nil" it}
end.

