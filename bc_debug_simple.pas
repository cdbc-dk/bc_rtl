
{*******************************************************************************
*        Unit name : bc_debug_simple.pas                                       *
*        Copyright : (C)cdbc.dk 2022                                           *
*        Programmer: Benny Christensen                                         *
*        Created   : 03.12.2022 /bc quick'n'dirty debug client for dbgsrv.     *
*        Updated   : 04.12.2022 /bc changed the locking mechanism.             *
*                                                                              *
********************************************************************************
*        Purpose:                                                              *
*        Helper function "DebugLn" log debug lines on a server                 *
*                                                                              *
********************************************************************************
*        License:                                                              *
*        "Beer License" - If you meet me one day, you'll buy me a beer :-)     *
*        I'm NOT liable for anything! Use at your own risk!!!                  *
*******************************************************************************}

unit bc_debug_simple;
{$mode objfpc}{$H+}
{.$define debug}
interface
uses
  Classes, SysUtils, blcksock, bc_guardian;

{ DebuLn logs a message to a running debugserver }
procedure DebugLn(aParam1,aParam2: ptrint;aStr: string);

var
  Sync: IGuardian;
  ErrorMsg: string;

implementation
uses bc_msgqueue;
var
  PrivateSync: TGuardian;

procedure PrivateDebugLn(aParam1,aParam2: ptrint;aStr: string);
var
  CliSock: TTCPBlockSocket;
  Msg: TbcMessage;
  S: string;
begin
  try
    Msg:= TbcMessage.Create(-1,StrToMsg('Lm_LogMsg'),aParam1,aParam2,aStr);
    CliSock:= TTCPBlockSocket.Create;
    CliSock.Bind('127.0.0.1','8723');                     { this is nescesary! }
    CliSock.Connect('127.0.0.1','8723');
    if CliSock.LastError = 0 then with CliSock do begin
      if CanWrite(1000) then begin
        SendString('Lm_LogMsg');      { send the Lm_LogMsg ~ 1047 to server ~ handshake }
        if ((LastError = 0) and CanRead(1000)) then begin
          S:= RecvPacket(1000);                  { read the received data ~ OK }
          if ((S = 'OK') and CanWrite(1000)) then begin         { handshake ok }
            SendString(Msg.AsString);          { now send our debug / log line }
            if LastError <> 0 then ErrorMsg:= LastErrorDesc;
          end;
        end;
      end;
    end;
    CliSock.CloseSocket;
    FreeAndNil(CliSock);
    FreeAndNil(Msg);
  except end;
end; { PrivateDebugLn }

procedure DebugLn(aParam1,aParam2: ptrint;aStr: string);
begin
  if Sync.BeginWrite then try
    PrivateDebugLn(aParam1,aParam2,aStr);
  finally
    Sync.EndWrite;
  end;
end; { DebugLn }

initialization
  PrivateSync:= TGuardian.Create;
  PrivateSync.GetInterface(SGUIDGuardian,Sync);

finalization 
  FreeAndNil(PrivateSync);
  
end.

