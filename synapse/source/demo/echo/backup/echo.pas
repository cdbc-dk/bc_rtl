unit Echo;

{$MODE Delphi}

interface

uses
  Classes,
  LMessages,
  LCLIntf,
  sysutils,
  blcksock,
  synsock;

const
  { internal messages for use between daemon & app }
  LM_CREATE           = LM_USER+1;
  LM_LISTEN           = LM_USER+3;
  LM_ACCEPT           = LM_USER+5;
  LM_WORKING          = LM_USER+7;
  LM_DONE             = LM_USER+11;
  LM_DESTROY          = LM_USER+13;

type
  { TTCPEchoDaemon }
  TTCPEchoDaemon = class(TThread)
  private
    fHandle: THandle;
    fAddress: string;
    fPort: string;
    fSock: TTCPBlockSocket;
  public
    Constructor Create(const aHandle: THandle);
    Destructor Destroy; override;
    procedure Execute; override;
    property Address: string read fAddress write fAddress;
    property Port: string read fPort write fPort;
  end;

  TTCPEchoThrd = class(TThread)
  private
    fHandle: THandle;
    fSock:TTCPBlockSocket;
    CSock: TSocket;
  public
    Constructor Create(hSock: TSocket;aHandle: THandle);
    procedure Execute; override;
  end;

implementation

{ TEchoDaemon }

constructor TTCPEchoDaemon.Create(const aHandle: THandle);
begin
  inherited create(true);
  fSock:=TTCPBlockSocket.create;  { this is the server socket, it only listens }
  FreeOnTerminate:= true;                                 { when done, go away }
  fAddress:= '0.0.0.0';                                        { listen to all }
  fPort:= '8723';                            { port assigned to echo?!? ~ 8008 }
  fHandle:= aHandle;                     { used for inter-thread communication }
  Start;                                              { RUN Forrest run!!! :-) }
  PostMessage(fHandle,LM_CREATE,strtoint(fPort),longint(pchar('Echo daemon created...')));
end;

destructor TTCPEchoDaemon.Destroy;
begin
  fSock.free;
  PostMessage(fHandle,LM_DESTROY,strtoint(fPort),longint(pchar('Echo daemon destroyed...')));
end;

procedure TTCPEchoDaemon.Execute;
var
  ClientSock:TSocket;
begin
  with fSock do begin
    CreateSocket;
    SetLinger(true,10000);
    Bind(fAddress,fPort); //ææ
    Listen;
    PostMessage(fHandle,LM_LISTEN,strtoint(fPort),longint(pchar('Echo daemon is listening...')));
    repeat
      if Terminated then break;
      if CanRead(1000) then begin
        ClientSock:= Accept;
        if LastError = 0 then begin
          TTCPEchoThrd.Create(ClientSock,fHandle);
          PostMessage(fHandle,LM_ACCEPT,strtoint(fPort),longint(pchar('Worker thread created...')));
        end;
      end;
    until false;
  end;
  PostMessage(fHandle,LM_DONE,strtoint(fPort),longint(pchar('Echo daemon done.')));
end;

{ TEchoThrd }
Constructor TTCPEchoThrd.Create(hSock:TSocket;aHandle: THandle);
begin
  inherited Create(true); // 20.03.2020 / bc:   inherited create(false);
  CSock:= hSock;
  FreeOnTerminate:= true;
  fHandle:= aHandle;
  Start; { safer this way, thread may start running before the properties are set! }
end;

procedure TTCPEchoThrd.Execute;
var S: string;
begin
  fSock:= TTCPBlockSocket.create;
  try
    fSock.Socket:= CSock;
    fSock.GetSins;
    with fSock do begin
      repeat
        if Terminated then break;
        S:= RecvPacket(60000);
        if LastError <> 0 then break;
        PostMessage(fHandle,LM_WORKING,length(S),longint(pchar('Worker thread is running...')));
        SendString(S);
        S:= 'Sent: '+S;
//        PostMessage(fHandle,LM_WORKING,length(S),longint(pchar(S)));
        if LastError <> 0 then break;
      until false;
    end;
  finally
    fSock.Free;
  end;
  PostMessage(fHandle,LM_WORKING,0,longint(pchar('Worker thread is Done.')));
end;

end.
