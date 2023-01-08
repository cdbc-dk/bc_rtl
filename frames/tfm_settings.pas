unit tfm_settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, Buttons, LMessages, LCLIntf;
const
  LM_CLOSETABSHEET = LM_USER + 97; { for use in trickery }
  LM_INSERTPAGE    = LM_USER + 98; { for service use }
type
  TLMCloseTabsheet = record
    Msg : Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    WParam: ptrint;
    LParam: ptrint;
    Result: ptrint;
  end; { for use in trickery }
  { TfraSettings }
  TfraSettings = class(TFrame)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    chbAutoBackup: TCheckBox;
    chbCachedUpd: TCheckBox;
    chbCaseSensitive: TCheckBox;
    chbMatchAll: TCheckBox;
    dlgOpenDir: TSelectDirectoryDialog;
    dlgOpenFile: TOpenDialog;
    edtBackupStorage: TLabeledEdit;
    edtCachedUpd: TLabeledEdit;
    edtDataStorage: TLabeledEdit;
    gbxEngine: TGroupBox;
    gbxStorage: TGroupBox;
    ImageList1: TImageList;
    lblEngineInfo: TLabel;
    lblLback: TLabel;
    lblLastB: TLabel;
    pnlBottom: TPanel;
    rbtMonthly: TRadioButton;
    rbtWeekly: TRadioButton;
    rbtDaily: TRadioButton;
    sbxMain: TScrollBox;
    btnService: TSpeedButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnServiceClick(Sender: TObject);
    procedure edtBackupStorageClick(Sender: TObject);
    procedure edtDataStorageClick(Sender: TObject);
    procedure rbtDailyClick(Sender: TObject);
    procedure rbtMonthlyClick(Sender: TObject);
    procedure rbtWeeklyClick(Sender: TObject);
  private
    fParentForm: TForm;
    fOwner: TWinControl;
    fFreq: integer;
    fSettings: TObject; //TDDSettings;
    fOnChange: TNotifyEvent;
    procedure DoOnChange;
  public
    procedure Init(aSettings: TObject);
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

function CreateSettingsFrame(anOwner: TWinControl;aSettings: TObject): TfraSettings;

implementation
uses ComCtrls, dd_settings, bc_pcthelp,bc_datetime;

function CreateSettingsFrame(anOwner: TWinControl; aSettings: TObject): TfraSettings;
begin
  Result:= TfraSettings.Create(anOwner);
  Result.Parent:= anOwner; //TWinControl(anOwner);
  Result.fOwner:= anOwner;               // fOwner.Parent.Parent.Handle:=;
  Result.fParentForm:= bcGetParentForm(anOwner); //TForm(anOwner.Parent.Parent); //
  Result.Align:= alClient; //  page  pgctrl form
  Result.Init(aSettings);
end;

{$R *.lfm}

{ TfraSettings }

procedure TfraSettings.btnOkClick(Sender: TObject);
var S: TDDSettings;
begin
  if fSettings is TDDSettings then S:= TDDSettings(fSettings) else exit;
  S.Databasename:= edtDataStorage.Text;
  S.BackupPath:= edtBackupStorage.Text;
  S.AutoBackup:= chbAutoBackup.Checked;
  S.BackupFrequency:= fFreq; { controlled by the radiobuttons }
  S.BatchUpdates:= chbCachedUpd.Checked;
  S.BatchCount:= StrToInt(edtCachedUpd.Text);
  S.CaseSensitive:= chbCaseSensitive.Checked;
  S.MatchAll:= chbMatchAll.Checked;
  S.Update; { update our settings object }
  DoOnChange; { is anyone listening?, fire the event with settings as object }
  btnCancelClick(Sender); { enjoy the spoils of work already done :o) }
end;

{ send message to mainform, instructing it to insert a new page }
procedure TfraSettings.btnServiceClick(Sender: TObject); {*}
var
  Dt: TIsoDate;
  S: string;
begin
  S:= InputBox('Insert choice page','Enter a date of the form: dd.mm.yyyy','');
  if S <> '' then begin
    Dt:= TIsoDate.Create(S);
    if fParentForm <> nil then
      PostMessage(fParentForm.Handle,LM_INSERTPAGE,Dt.AsInteger,0); { trickery }
    Dt.Free;
    btnService.Enabled:= false;
    btnCancelClick(Sender);
  end;
end;

procedure TfraSettings.edtBackupStorageClick(Sender: TObject);
begin { Directory := IncludeTrailingPathDelimiter(Directory); }
  if dlgOpenDir.Execute then edtBackupStorage.Text:= dlgOpenDir.FileName+'/';
end;

procedure TfraSettings.edtDataStorageClick(Sender: TObject);
begin
  if dlgOpenFile.Execute then begin
    edtDataStorage.Text:= dlgOpenFile.FileName;
  end;
end;

procedure TfraSettings.rbtDailyClick(Sender: TObject);
begin
  fFreq:= 0;
end;

procedure TfraSettings.rbtMonthlyClick(Sender: TObject);
begin
  fFreq:= 2;
end;

procedure TfraSettings.rbtWeeklyClick(Sender: TObject);
begin
  fFreq:= 1;
end;

procedure TfraSettings.DoOnChange;
begin
  if Assigned(fOnChange) then fOnChange(fSettings);
end;

procedure TfraSettings.Init(aSettings: TObject);
var S: TDDSettings;
begin
  if aSettings is TDDSettings then S:= TDDSettings(aSettings) else exit;
  fSettings:= aSettings;
  edtDataStorage.Text:= S.Databasename;
  edtBackupStorage.Text:= S.BackupPath;
  chbAutoBackup.Checked:= S.AutoBackup;
  case S.BackupFrequency of
    0: rbtDaily.Checked:= true;
    1: rbtWeekly.Checked:= true;
    2: rbtMonthly.Checked:= true;
  end;
  fFreq:= S.BackupFrequency;
  lblLback.Caption:= S.LastBackup;
  chbCachedUpd.Checked:= S.BatchUpdates;
  edtCachedUpd.Text:= S.BatchCount.ToString;
  chbCaseSensitive.Checked:= S.CaseSensitive;
  chbMatchAll.Checked:= S.MatchAll;
end;

procedure TfraSettings.btnCancelClick(Sender: TObject);
var
  pct: TPageControl;
  pg: TTabSheet;
begin
  if fOwner is TTabSheet then begin
    pct:= TTabSheet(fOwner).PageControl;
    pg:= pct.FindNextPage(TTabSheet(fOwner),false,false);
    if pg <> nil then begin
      pct.ActivePage:= pg;
      if fParentForm <> nil then
        PostMessage(fParentForm.Handle,LM_CLOSETABSHEET,ptrint(fOwner),0); { trickery }
    end;
  end;
end;

end.

