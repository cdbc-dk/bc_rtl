

                   {**************************************$
                   $  Unit name : bc_msgqueue.pas         $
                   $  Copyright : (C)2020 cdbc.dk         $
                   $  Programmer: Benny Christensen       $
                   $  Created   : 2020.03.26 /bc          $
                   $  Updated   : 2020.04.30 /bc          $
                   $              2020.05.02 /bc          $
                   $              2020.05.06 /bc          $
                   $                                      $
                   $                                      $
                   $ ************************************ $
                   $  Purpose   :                         $
                   $  Implements a message queue and      $
                   $  message-objects for use with multi- $
                   $  threading communication.            $
                   $                                      $
                   $**************************************}

unit bc_msgqueue;
{$define fpc}
{.$define debug}
{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}
interface
uses
  Classes, Sysutils, LMessages, LCLIntf,
  bc_guardian,
  bc_mtlinklist,
  bc_datetime;

const
  { unit version control to keep track of development }
  UnitVersion = '3.06.05.2020';
  { internal messages for use between daemon & app }
  LM_CREATE   = LM_USER+1;
  LM_LISTEN   = LM_USER+3;
  LM_ACCEPT   = LM_USER+5;
  LM_WORKING  = LM_USER+7;
  LM_DONE     = LM_USER+11;
  LM_DESTROY  = LM_USER+13;
  LM_LOADED   = LM_USER+17;
  LM_SENT     = LM_USER+19;
  LM_LOGMSG   = LM_USER+23; { 29.04.2020 /bc }
  LM_THREAD   = LM_USER+29; { 01.05.2020 /bc }
  LM_CALLBACK = LM_USER+31; { 02.05.2020 /bc }
  { result codes from functions, more flexible than boolean }
  HR_OK = 0;
  HR_ERROR = -1;

type
  { *** TbcMessage *** }
  TbcMessage = class
  private
    fHandle: ptrint;
    fMsg: ptrint;
    fWParam: ptrint;
    fLParam: ptrint;
    fSParam: string;
    function CloneString(const aString: string): string;
  public
    constructor Create(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string);
    function Clone: TbcMessage;
    property Handle: ptrint read fHandle write fHandle;
    property Msg: ptrint read fMsg write fMsg;
    property WParam: ptrint read fWParam write fWParam;
    property LParam: ptrint read fLParam write fLParam;
    property SParam: string read fSParam write fSParam; // optional string param
  end;
  { a message queue for thread uncoupling/detachment }
  { TbcMessageQueue }
  TbcMessageQueue = class(TbcQueue)
    procedure EnQueue(aMessage: TbcMessage);
    procedure PostMsg(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string); { 06.05.2020 /bc }
    function Peek: TbcMessage; { does NOT remove from queue, only looking }
    function DeQueue: TbcMessage;
  end;

{ factory sets up a global messagequeue when needed }
function MsgQ: TbcMessageQueue;
{ utility function }
function MsgToStr(const aMsg: ptrint): string; { 29.04.2020 /bc }

implementation
uses bc_errorlog;

{ factory sets up a global messagequeue when needed. a mix of factory and singleton patterns }
var SingletonQ: TbcMessageQueue;
function MsgQ: TbcMessageQueue;
begin
  if not Assigned(SingletonQ) then SingletonQ:= TbcMessageQueue.Create;
  Result:= SingletonQ;
end; { MsgQ }

function MsgToStr(const aMsg: ptrint): string; { 29.04.2020 /bc }
begin
  case aMsg of
    LM_CREATE  : Result:= 'Lm_Create';
    LM_LISTEN  : Result:= 'Lm_Listen';
    LM_ACCEPT  : Result:= 'Lm_Accept';
    LM_WORKING : Result:= 'Lm_Working';
    LM_DONE    : Result:= 'Lm_Done';
    LM_DESTROY : Result:= 'Lm_Destroy';
    LM_LOADED  : Result:= 'Lm_Loaded';
    LM_SENT    : Result:= 'Lm_Sent';
    LM_LOGMSG  : Result:= 'Lm_LogMsg';
    LM_THREAD  : Result:= 'Lm_Thread';
    LM_CALLBACK: Result:= 'Lm_Callback';
  else
    Result:= 'Unknown';
  end;
end;

{ *** TbcMessage *** }
function TbcMessage.CloneString(const aString: string): string;
var Len: ptrint;
begin
  Len:= Length(aString);
  SetLength(Result,Len);
  move(aString[1],Result[1],Len); // should make it unique?!?
end;

constructor TbcMessage.Create(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string);
begin
  inherited Create;
  fHandle:= aHandle;
  fMsg:= aMsg;
  fWParam:= aWParam;
  fLParam:= aLParam;
  fSParam:= CloneString(aSParam); { should be unique?!? }
end;

function TbcMessage.Clone: TbcMessage;
begin
  Result:= TbcMessage.Create(Self.fHandle,
                             Self.fMsg,
                             Self.fWParam,
                             Self.fLParam,
                             Self.fSParam);
end;

{ *** TbcMessageQueue *** }
procedure TbcMessageQueue.EnQueue(aMessage: TbcMessage);
begin
  En_Queue(pointer(aMessage));
end;
{ shortcut to enqueue a message, 06.05.2020 /bc }
procedure TbcMessageQueue.PostMsg(aHandle, aMsg, aWParam, aLParam: ptrint; aSParam: string);
var Msg: TbcMessage;
begin
  Msg:= TbcMessage.Create(aHandle,aMsg,aWParam,aLParam,aSParam); { well duh! }
  Enqueue(Msg);
end;

function TbcMessageQueue.Peek: TbcMessage;
begin
  Result:= TbcMessage(Examine);
end;

function TbcMessageQueue.DeQueue: TbcMessage;
begin
  Result:= TbcMessage(De_Queue);
end;

initialization
  SingletonQ:= nil;
finalization
  FreeAndNil(SingletonQ); { tear down the global messagequeue }
end.
