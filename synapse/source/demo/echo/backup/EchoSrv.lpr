program EchoSrv;

{$MODE Delphi}

uses
  cthreads,
  Forms, Interfaces,
  main in 'main.pas' {Form1},
  echo in 'echo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
