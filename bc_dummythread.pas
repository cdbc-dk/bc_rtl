unit bc_dummythread;
{$mode delphi}
interface
uses Classes, SysUtils, ftptsend, FileUtil,Forms;
const
  UnitVersion = '1.04.04.2020';
type
  { a dummy-"thread" created for debug purposes, if it works here, it'll work in a thread }
  TTFTPDaemonThread = class(TObject)
  private
    fTFTPDaemon: TTFTPSend;
    fIPAdress: string;
    fPort: string;
    fLogMessage: string;
    fTerminated: boolean;
    procedure UpdateLog;
  protected
    procedure Execute; {override;}
    procedure Synchronize(aMethod: TThreadMethod);
  public
    constructor Create(anIPAdress,aPort:string);
    procedure Start;
    procedure Terminate;
    procedure WaitFor;
    property Terminated: boolean read fTerminated;
  end;

implementation
uses MainBox;

{ *** TTFTPDaemonThread *** }
procedure TTFTPDaemonThread.UpdateLog;
begin
  frmMain.Log.Lines.Add(fLogMessage);
end;

procedure TTFTPDaemonThread.Execute;
var RequestType: word;
    FileName: string;
begin
  fTFTPDaemon := TTFTPSend.Create;
  fLogMessage := 'ServerThread created on Port ' + fPort;
  Synchronize(UpdateLog);
  fTFTPDaemon.TargetHost := fIPAdress;
  fTFTPDaemon.TargetPort := fPort;
  try
    while not Terminated do begin
      Application.ProcessMessages; // a bit of a hack :-/
      if fTFTPDaemon.WaitForRequest(RequestType,FileName) then begin
        // Fill the Log-Memo whith Infos about the request
        case RequestType of
          1:fLogMessage := 'Read-Request from '
                           + fTFTPDaemon.RequestIP + ':' + fTFTPDaemon.RequestPort;
          2:fLogMessage := 'Write-Request from '
                           + fTFTPDaemon.RequestIP + ':' + fTFTPDaemon.RequestPort;
        end;
        Synchronize(UpdateLog);
        Application.ProcessMessages; // a bit of a hack :-/
        fLogMessage := 'File: ' + Filename;
        Synchronize(UpdateLog);
        Application.ProcessMessages; // a bit of a hack :-/
        // Process the Request
        case RequestType of
          1: begin  // Read request (RRQ) { *Converted from FileExists*  FileExistsUTF8 }
               if FileExists(frmMain.edtPath.Text + FileName) then begin
                 fLogMessage := 'File: ' + Filename + ' does exist.';
                 Synchronize(UpdateLog);
                 Application.ProcessMessages; // a bit of a hack :-/
                 fTFTPDaemon.Data.LoadFromFile(frmMain.edtPath.Text + FileName);
                 fLogMessage := 'File: ' + Filename + ' loaded from disc.';
                 Synchronize(UpdateLog);
                 Application.ProcessMessages; // a bit of a hack :-/
                 if fTFTPDaemon.ReplySend then begin
                   fLogMessage := '"' + frmMain.edtPath.Text + FileName + '" successfully sent.';
                   Synchronize(UpdateLog);
                   Application.ProcessMessages; // a bit of a hack :-/
                 end;
               end else begin
                 fTFTPDaemon.ReplyError(1,'File not Found');
                 fLogMessage:= '"' + frmMain.edtPath.Text + FileName + '" File not Found.';
                 Synchronize(UpdateLog);
                 Application.ProcessMessages; // a bit of a hack :-/
               end;
             end;
          2: begin  // Write request (WRQ) { *Converted from FileExists* }
               if not FileExists(frmMain.edtPath.Text + FileName) then begin
                 if fTFTPDaemon.ReplyRecv then begin
                   fTFTPDaemon.Data.SaveToFile(frmMain.edtPath.Text + FileName);
                   fLogMessage := 'File sucessfully stored to ' + frmMain.edtPath.Text + FileName;
                   Synchronize(UpdateLog);
                 end;
               end else fTFTPDaemon.ReplyError(6,'File already exists');
             end;
        end; // end case
      end;
      Application.ProcessMessages; // a bit of a hack :-/
    end;
  finally fTFTPDaemon.Free; end;
  fLogMessage := 'ServerThread is done!';
  Synchronize(UpdateLog);
  Application.ProcessMessages; // a bit of a hack :-/
  Self.Free;
end;

procedure TTFTPDaemonThread.Synchronize(aMethod: TThreadMethod);
begin
  if assigned(aMethod) then aMethod;
end;

constructor TTFTPDaemonThread.Create(anIPAdress, aPort: string);
begin
  fIPAdress:= anIPAdress;
  fPort:= aPort;
  fTerminated:= false;
//  inherited Create(false);
end;

procedure TTFTPDaemonThread.Start;
begin
  Execute; // will run without a thread, just blocking :-D
end;

procedure TTFTPDaemonThread.Terminate;
begin
  fTerminated:= true;
end;

procedure TTFTPDaemonThread.WaitFor;
begin
  Sleep(1000); // simulate the real waitfor :-)
end;

end.

