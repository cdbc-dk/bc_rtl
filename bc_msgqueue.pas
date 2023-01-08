

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
  UnitVersion = '3.28.08.2022';
  { internal messages for use between daemon & app }
  LM_CREATE         = LM_USER+1;
  LM_LISTEN         = LM_USER+3;
  LM_ACCEPT         = LM_USER+5;
  LM_WORKING        = LM_USER+7;
  LM_DONE           = LM_USER+11;
  LM_DESTROY        = LM_USER+13;
  LM_LOADED         = LM_USER+17;
  LM_SENT           = LM_USER+19;
  LM_LOGMSG         = LM_USER+23; { 29.04.2020 /bc }
  LM_THREADNOTIFY   = LM_USER+27; { 17.05.2020 /bc }
  LM_THREADSTART    = LM_USER+28; { 15.12.2022 /bc }
  LM_THREADPROGRESS = LM_USER+29; { 01.05.2020 /bc }
  LM_THREADWORK     = LM_USER+30; { 01.05.2020 /bc }
  LM_CALLBACK       = LM_USER+31; { 02.05.2020 /bc }
  LM_THREADDONE     = LM_USER+32;
  LM_SAVEFILE       = LM_USER+37; { 26.08.2022 /bc }
  LM_LOGTODB        = LM_USER+41; { 26.08.2022 /bc }
  LM_PROGRESS       = LM_USER+43; { 13.12.2022 /bc }
  LM_DATAINQUEUE    = 4711;       { 15.12.2022 /bc }
  LM_TASKSTART      = LM_USER+44; { 15.12.2022 /bc }
  LM_TASKWORK       = LM_USER+45; { 15.12.2022 /bc }
  LM_TASKDONE       = LM_USER+46; { 15.12.2022 /bc }
  LM_THREADERROR    = LM_USER+47; { 15.12.2022 /bc }
  { result codes from functions, more flexible than boolean }
  HR_OK    = 0;
  HR_BUSY  = 1; { 26.08.2022 /bc }
  HR_ERROR = -1;
  { comm response pings etc... }
  RES_OK =    'OK';
  RES_ERROR = 'ERROR';

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
    constructor Create(aHandle,aMsg,aWParam,aLParam: ptrint;aSParam: string); overload;
    constructor Create(const aSerializedString: string); overload;
    function Clone: TbcMessage;
    function Serialize: string;
    property Handle: ptrint read fHandle write fHandle;
    property Msg: ptrint read fMsg write fMsg;
    property WParam: ptrint read fWParam write fWParam;
    property LParam: ptrint read fLParam write fLParam;
    property SParam: string read fSParam write fSParam; // optional string param
    property AsString: string read Serialize;
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
{ utility functions }
function MsgToStr(const aMsg: ptrint): string; { 29.08.2022 /bc }
function StrToMsg(const aStr: string): ptrint;

implementation
uses bc_errorlog, bc_strings, bc_advstring;
var bcMessages: TbcKeyValueList;
{ factory sets up a global messagequeue when needed. a mix of factory and singleton patterns }
var SingletonQ: TbcMessageQueue;
function MsgQ: TbcMessageQueue;
begin
  if not Assigned(SingletonQ) then SingletonQ:= TbcMessageQueue.Create;
  Result:= SingletonQ;
end; { MsgQ }

procedure InitbcMessages;
begin
  bcMessages.MoveBeforeFirst;
  bcMessages.Add(LM_CREATE,'Lm_Create');
  bcMessages.Add(LM_LISTEN,'Lm_Listen');
  bcMessages.Add(LM_ACCEPT,'Lm_Accept');
  bcMessages.Add(LM_WORKING,'Lm_Working');
  bcMessages.Add(LM_DONE,'Lm_Done');
  bcMessages.Add(LM_DESTROY,'Lm_Destroy');
  bcMessages.Add(LM_LOADED,'Lm_Loaded');
  bcMessages.Add(LM_SENT,'Lm_Sent');
  bcMessages.Add(LM_LOGMSG,'Lm_LogMsg');
  bcMessages.Add(LM_THREADPROGRESS,'Lm_ThreadProgress');
  bcMessages.Add(LM_CALLBACK,'Lm_Callback');
  bcMessages.Add(LM_SAVEFILE,'Lm_Savefile');
  bcMessages.Add(LM_LOGTODB,'Lm_LogToDb');
  bcMessages.Add(LM_PROGRESS,'Lm_Progress');
  bcMessages.Add(LM_DATAINQUEUE,'Lm_DataInQueue');
  bcMessages.Add(LM_THREADERROR,'Lm_ThreadError');
  bcMessages.Add(HR_OK,RES_OK);
  bcMessages.Add(HR_ERROR,RES_ERROR);
  bcMessages.Add(1,'.jpg'); { added for image manipulation }
  bcMessages.Add(2,'.png'); { added for image manipulation }
  bcMessages.Add(3,'.gif'); { added for image manipulation }
  bcMessages.Add(4,'.bmp'); { added for image manipulation }
  bcMessages.MoveBeforeFirst; { reset cursor to beginning }
end; { InitbcMessages }

function MsgToStr(const aMsg: ptrint): string; { 29.04.2020 /bc }
begin
  Result:= bcMessages.FindValue(aMsg); { not found ~ Result:= '' }
end;

function StrToMsg(const aStr: string): ptrint;
begin
  Result:= bcMessages.FindKey(aStr); { not found ~ Result:= -1 }
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

constructor TbcMessage.Create(const aSerializedString: string);
begin
  inherited Create;
  fHandle:= bcGetFieldToken(1,aSerializedString,'|').ToInt64;
  fMsg:= bcGetFieldToken(2,aSerializedString,'|').ToInt64;
  fWParam:= bcGetFieldToken(3,aSerializedString,'|').ToInt64;
  fLParam:= bcGetFieldToken(4,aSerializedString,'|').ToInt64;
  fSParam:= bcGetFieldToken(5,aSerializedString,'|');
end;

function TbcMessage.Clone: TbcMessage;
begin
  Result:= TbcMessage.Create(Self.fHandle,
                             Self.fMsg,
                             Self.fWParam,
                             Self.fLParam,
                             Self.fSParam);
end;

{ serializes message fields to a string }
function TbcMessage.Serialize: string;
begin
  Result:= fHandle.ToString+'|'+
           fMsg.ToString+'|'+
           fWParam.ToString+'|'+
           fLParam.ToString+'|'+
           fSParam;
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
  bcMessages:= TbcKeyValueList.Create;
  InitbcMessages;
finalization
  FreeAndNil(SingletonQ); { tear down the global messagequeue }
  FreeAndNil(bcMessages);
end.
