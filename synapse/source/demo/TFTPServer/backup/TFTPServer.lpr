program TFTPServer;

{$MODE Delphi}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Forms, Interfaces,
  MainBox in 'MainBox.pas' {MainForm},
  TFTPDaemonThread in 'TFTPDaemonThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.