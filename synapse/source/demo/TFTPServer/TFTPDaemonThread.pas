{
   TFTP supports five types of packets, all of which have been mentioned
   above:
      opcode     operation

        1        Read request (RRQ)
        2        Write request (WRQ)
        3        Data (DATA)
        4        Acknowledgment (ACK)
        5        Error (ERROR)


   Error Codes
     Value       Meaning

       0         Not defined, see error message (if any).
       1         File not found.
       2         Access violation.
       3         Disk full or allocation exceeded.
       4         Illegal TFTP operation.
       5         Unknown transfer ID.
       6         File already exists.
       7         No such user.


}

unit TFTPDaemonThread;

{$MODE Delphi}

interface

uses Classes, SysUtils, ftptsend, FileUtil;

type
  TTFTPDaemonThread = class(TThread)
  private
    fTFTPDaemon: TTFTPSend;
    fIPAdress: string;
    fPort: string;
    fLogMessage: string;
    procedure UpdateLog;
  protected
    procedure Execute; override;
  public
    constructor Create(anIPAdress,aPort:string);
  end;

implementation

uses MainBox;

constructor TTFTPDaemonThread.Create(anIPAdress,aPort:string);
begin
  fIPAdress := anIPAdress;
  fPort     := aPort;
  inherited Create(false);
end;

procedure TTFTPDaemonThread.UpdateLog;
begin
  MainForm.Log.Lines.Add(fLogMessage);
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
      if fTFTPDaemon.WaitForRequest(RequestType,FileName) then begin
        // Fill the Log-Memo whith Infos about the request
        case RequestType of
          1:fLogMessage := 'Read-Request from '
                           + fTFTPDaemon.RequestIP + ':' + fTFTPDaemon.RequestPort;
          2:fLogMessage := 'Write-Request from '
                           + fTFTPDaemon.RequestIP + ':' + fTFTPDaemon.RequestPort;
        end;
        Synchronize(UpdateLog);
        fLogMessage := 'File: ' + Filename;
        Synchronize(UpdateLog);
        // Process the Request
        case RequestType of
          1: begin  // Read request (RRQ) { *Converted from FileExists*  FileExistsUTF8 }
               if FileExists(MainForm.PathEdit.Text + FileName) then begin
                 fTFTPDaemon.Data.LoadFromFile(MainForm.PathEdit.Text + FileName);
                 if fTFTPDaemon.ReplySend then begin
                   fLogMessage := '"' + MainForm.PathEdit.Text + FileName + '" successfully sent.';
                   Synchronize(UpdateLog);
                 end;
               end else fTFTPDaemon.ReplyError(1,'File not Found');
             end;
          2: begin  // Write request (WRQ) { *Converted from FileExists* }
               if not FileExists(MainForm.PathEdit.Text + FileName) then begin
                 if fTFTPDaemon.ReplyRecv then begin
                   fTFTPDaemon.Data.SaveToFile(MainForm.PathEdit.Text + FileName);
                   fLogMessage := 'File sucessfully stored to ' + MainForm.PathEdit.Text + FileName;
                   Synchronize(UpdateLog);
                 end;
               end else fTFTPDaemon.ReplyError(6,'File already exists');
             end;
        end; // end case
      end;
    end;
  finally fTFTPDaemon.Free; end;
end;

end.
