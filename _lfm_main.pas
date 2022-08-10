unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Buttons, RackCtls, FileUtil;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnSearch: TBitBtn;
    btnBrowseDir: TButton;
    edtPattern: TEdit;
    edtDirectory: TEdit;
    gbxOptions: TGroupBox;
    gbxResults: TGroupBox;
    lblPattern: TLabel;
    lblDirectory: TLabel;
    memResults: TMemo;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    stbMain: TStatusBar;
    procedure btnBrowseDirClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fFileSearcher: TListFileSearcher;
  public
    procedure OnFileFound(FileIterator: TFileIterator);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnBrowseDirClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then begin
    edtDirectory.Text:= SelectDirectoryDialog1.FileName;
  end;
end;

procedure TfrmMain.btnSearchClick(Sender: TObject);
begin
  fFileSearcher.Search(edtDirectory.Text,edtPattern.Text,true,false);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fFileSearcher:= TListFileSearcher.Create(memResults.Lines);
  fFileSearcher.OnFileFound:= @OnFileFound;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fFileSearcher.OnFileFound:= nil;
  FreeAndNil(fFileSearcher);
end;

procedure TfrmMain.OnFileFound(FileIterator: TFileIterator);
begin
  stbMain.SimpleText:= FileIterator.FileName;
  Application.ProcessMessages;
end;

end.

