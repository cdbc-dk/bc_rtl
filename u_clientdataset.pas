unit u_clientdataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db,
  memdataset,
  litedb,
  u_cds_intf; // IClientDataset definition + interface factory

const
  cClientDataset_Version = '1.03.01.2014';
  // modification states
  mNone = 0;
  mInsert = 1;
  mEdit = 2;
  mDelete = 3;

type
  { TClientDataset }

  TClientDataset = class(TMemDataset,IClientDataset)
  private
    fDeleteSQL: string;
    fInsertSQL: string;
    fLiteDb: TLiteDb;
    fOnExecDelete: TExecSqlEvent;
    fOnExecInsert: TExecSqlEvent;
    fOnExecUpdate: TExecSqlEvent;
    fSelectSQL: string;
    fUpdateSQL: string;
    fDeltaQ: TMemDataset; { Update action, cliendataset-like behaviour }
    { observer pattern }
    fObservers: TList;
    fState: integer;
    { observer pattern }

    procedure CDSAddToQueue;
    procedure CDSInitializeDelta;
    procedure CDSFinalizeDelta;
    function get_Cds_Version: string;
    function get_Dataset: TDataset;
    function get_DeleteSQL: string;
    function get_InsertSQL: string;
    function get_IsActive: boolean;
    function get_LiteDb: TLiteDb;
    function get_OnExecDelete: TExecSqlEvent;
    function get_OnExecInsert: TExecSqlEvent;
    function get_OnExecUpdate: TExecSqlEvent;
    function get_SelectSQL: string;
    function get_UpdateSQL: string;
    procedure set_DeleteSQL(aValue: string);
    procedure set_InsertSQL(aValue: string);
    procedure set_LiteDb(AValue: TLiteDb);
    procedure set_OnExecDelete(aValue: TExecSqlEvent);
    procedure set_OnExecInsert(aValue: TExecSqlEvent);
    procedure set_OnExecUpdate(aValue: TExecSqlEvent);
    procedure set_SelectSQL(aValue: string);
    procedure set_UpdateSQL(aValue: string);
    { observer pattern }
    procedure Register_Observer(anObserver: IObserver);
    procedure UnRegister_Observer(anObserver: IObserver);
    procedure Notify;
    function get_State: integer;
    procedure set_State(const Value: integer);
    { observer pattern }
  protected
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalDelete; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure OpenPersistent;
    procedure Update(DoRefresh: boolean = false); { Update action, cliendataset-like behaviour }
    procedure Refresh;

    property OnExecInsert: TExecSqlEvent read get_OnExecInsert write set_OnExecInsert;
    property OnExecUpdate: TExecSqlEvent read get_OnExecUpdate write set_OnExecUpdate;
    property OnExecDelete: TExecSqlEvent read get_OnExecDelete write set_OnExecDelete;

    property LiteDb: TLiteDb read get_LiteDb write set_LiteDb;
    property SelectSQL: string read get_SelectSQL write set_SelectSQL;
    property InsertSQL: string read get_InsertSQL write set_InsertSQL;
    property UpdateSQL: string read get_UpdateSQL write set_UpdateSQL;
    property DeleteSQL: string read get_DeleteSQL write set_DeleteSQL;
    property Cds_Version: string read get_Cds_Version;
    property Dataset: TDataset read get_Dataset;
    property IsActive: boolean read get_IsActive;
    { observer pattern }
    property State: integer read get_State write set_State;
    { observer pattern }
  end;

implementation

{ TClientDataset }

procedure TClientDataset.CDSAddToQueue;
var
  F,DF: TField;
  I: integer;
begin
  if FieldByName('modified').AsInteger <> mNone then begin
    fDeltaQ.Append;
    for I:= 0 to Fields.Count-1 do begin
      F:= Fields[I];
      DF:= fDeltaQ.Fields[I];
      DF.Assign(F);
    end;
    fDeltaQ.Post;
  end;
  if fDeltaQ.RecordCount >= 3 then Update(true);
end;

procedure TClientDataset.CDSInitializeDelta;
begin
  fDeltaQ.CopyFromDataset(Self,false); { do NOT copy data }
  fDeltaQ.Open;
end;

procedure TClientDataset.CDSFinalizeDelta;
begin
  if fDeltaQ.Active then if fDeltaQ.RecordCount > 0 then Update;
  fDeltaQ.Clear(true);
  fDeltaQ.Close;
end;

function TClientDataset.get_Cds_Version: string; { 20.07.2012 }
begin
  Result:= cClientDataset_Version;
end;

function TClientDataset.get_Dataset: TDataset; { 27.07.2012 }
begin
  Result:= Self;
end;

function TClientDataset.get_DeleteSQL: string; { 20.07.2012 }
begin
  Result:= fDeleteSQL;
end;

function TClientDataset.get_InsertSQL: string; { 20.07.2012 }
begin
  Result:= fInsertSQL;
end;

function TClientDataset.get_IsActive: boolean; { 27.07.2012 }
begin
  Result:= Self.Active;
end;

function TClientDataset.get_LiteDb: TLiteDb; { 20.07.2012 }
begin
  Result:= fLiteDb;
end;

function TClientDataset.get_OnExecDelete: TExecSqlEvent; { 20.07.2012 }
begin
  Result:= fOnExecDelete;
end;

function TClientDataset.get_OnExecInsert: TExecSqlEvent; { 19.07.2012 }
begin
  Result:= fOnExecInsert;
end;

function TClientDataset.get_OnExecUpdate: TExecSqlEvent; { 19.07.2012 }
begin
  Result:= fOnExecUpdate;
end;

function TClientDataset.get_SelectSQL: string; { 20.07.2012 }
begin
  Result:= fSelectSQL;
end;

function TClientDataset.get_UpdateSQL: string; { 20.07.2012 }
begin
  Result:= fUpdateSQL;
end;

procedure TClientDataset.set_DeleteSQL(aValue: string); { 20.07.2012 }
begin
  fDeleteSQL:= aValue;
end;

procedure TClientDataset.set_InsertSQL(aValue: string); { 20.07.2012 }
begin
  fInsertSQL:= aValue;
end;

procedure TClientDataset.set_LiteDb(aValue: TLiteDb); { 20.07.2012 }
begin
  fLiteDb:= aValue;
end;

procedure TClientDataset.set_OnExecDelete(aValue: TExecSqlEvent); { 20.07.2012 }
begin
  fOnExecDelete:= aValue;
end;

procedure TClientDataset.set_OnExecInsert(aValue: TExecSqlEvent); { 19.07.2012 }
begin
  fOnExecInsert:= aValue;
end;

procedure TClientDataset.set_OnExecUpdate(aValue: TExecSqlEvent); { 19.07.2012 }
begin
  fOnExecUpdate:= aValue;
end;

procedure TClientDataset.set_SelectSQL(aValue: string); { 20.07.2012 }
begin
  fSelectSQL:= aValue;
end;

procedure TClientDataset.set_UpdateSQL(aValue: string); { 20.07.2012 }
begin
  fUpdateSQL:= aValue;
end;

procedure TClientDataset.Register_Observer(anObserver: IObserver);
begin
  if fObservers.IndexOf(pointer(anObserver)) = -1 then fObservers.Add(pointer(anObserver));
end;

procedure TClientDataset.UnRegister_Observer(anObserver: IObserver);
var I: integer;
begin
  I:= fObservers.IndexOf(pointer(anObserver));
  if I <> -1 then fObservers.Delete(I);
end;

procedure TClientDataset.Notify;
var I: integer;
begin
  for I:= 0 to fObservers.Count - 1 do IObserver(fObservers[I]).Update(Self);
end;

function TClientDataset.get_State: integer;
begin
  Result:= fState;
end;

procedure TClientDataset.set_State(const Value: integer);
begin
  fState:= Value;
end;

procedure TClientDataset.InternalOpen;
begin
  inherited InternalOpen;
  CDSInitializeDelta; { copy structure to queue }
end;

procedure TClientDataset.InternalPost;
begin
  inherited InternalPost;
  CDSAddToQueue;
end;

procedure TClientDataset.InternalDelete;
begin
  FieldByName('modified').AsInteger:= mDelete;
  CDSAddToQueue;
  inherited InternalDelete;
end;

constructor TClientDataset.Create(AOwner: tComponent);
begin
  inherited Create(AOwner);
  fDeltaQ:= TMemDataset.Create(nil);
  { observer pattern }
  fObservers:= TList.Create;
  { observer pattern }
end;

destructor TClientDataset.Destroy;
begin
  { observer pattern }
  fState:= -999;
  Notify;
  fObservers.Clear;
  fObservers.Free;
  { observer pattern }
  CDSFinalizeDelta;
  fDeltaQ.Free;
  fLiteDb:= nil;
  inherited Destroy;
end;

procedure TClientDataset.OpenPersistent;
begin
  if assigned(fLiteDb) then begin
    if not fLiteDb.Connected then fLiteDb.Connect;
    fLiteDb.QuerySQL(fSelectSQL,Self);
    Open;
    fDeltaQ.Clear(false);
  end;
end;

procedure TClientDataset.Update(DoRefresh: boolean = false);
begin
  // lock
  if not fLiteDb.Connected then fLiteDb.Connect;
  try
    fDeltaQ.First;
    while not fDeltaQ.EOF do begin
      case fDeltaQ.FieldByName('modified').AsInteger of
        mInsert: begin
                   if assigned(fOnExecInsert) then fOnExecInsert(fDeltaQ,fInsertSQL);
                   fLiteDb.RunSQL(fInsertSQL);
                 end;
        mEdit:   begin
                   if assigned(fOnExecUpdate) then fOnExecUpdate(fDeltaQ,fUpdateSQL);
                   fLiteDb.RunSQL(fUpdateSQL);
                 end;
        mDelete: begin
                   if assigned(fOnExecDelete) then fOnExecDelete(fDeltaQ,fDeleteSQL);
                   fLiteDb.RunSQL(fDeleteSQL);
                 end;
      end;
      fDeltaQ.Next;
    end;
  fDeltaQ.Clear(false);
  if DoRefresh then Refresh;
  finally { unlock } end;
end;

procedure TClientDataset.Refresh;
begin
  OpenPersistent;
end;

end.

