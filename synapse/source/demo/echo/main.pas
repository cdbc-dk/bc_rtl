unit main;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, LMessages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Echo;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    Memo1: TMemo;
    StartServer: TButton;
    DoneServer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure StartServerClick(Sender: TObject);
    procedure DoneServerClick(Sender: TObject);
  private
    fEchoSrv: TTCPEchoDaemon;
  protected
    procedure LMCreate(var Message: TLMessage); message LM_CREATE;
    procedure LMListen(var Message: TLMessage); message LM_LISTEN;
    procedure LMAccept(var Message: TLMessage); message LM_ACCEPT;
    procedure LMWorking(var Message: TLMessage); message LM_WORKING;
    procedure LMDone(var Message: TLMessage); message LM_DONE;
    procedure LMDestroy(var Message: TLMessage); message LM_DESTROY;
  public
    procedure ThreadTerminated(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

procedure TfrmMain.StartServerClick(Sender: TObject);
begin
  StartServer.Enabled:= false;
  Memo1.Clear;
  fEchoSrv:= TTCPEchoDaemon.Create(Handle);
  fEchoSrv.OnTerminate:= {@}ThreadTerminated; // in delphi mode...
  DoneServer.Enabled:= true;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DoneServer.Enabled:= false;
  Caption:= Caption + ' [Port: 8723]';
end;

procedure TfrmMain.DoneServerClick(Sender: TObject);
begin
  fEchoSrv.Terminate;
  fEchoSrv.WaitFor;
end;

procedure TfrmMain.LMCreate(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMListen(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMAccept(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMWorking(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMDone(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMDestroy(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.ThreadTerminated(Sender: TObject);
begin
  StartServer.Enabled:= true;
  DoneServer.Enabled:= false;
  ShowMessage('Terminating Echo thread id: '+inttostr(TThread(Sender).Handle));
end;

end.
