program TFTPClient;

{$MODE Delphi}

uses
  cthreads,
  Forms, Interfaces,
  MainBox in 'MainBox.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
