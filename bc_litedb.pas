unit bc_litedb; 
{************************************************
*                                               *
* A lightweight database connection for SQLite3 *
* Wraps a TSQLite3Connection, TSQLTransaction   *
* and a TSQLQuery all into one.                 *
* BEWARE NO BLOBS!                              *
* Revision 2.13.03.2020 /bc                     *
*                                               *
************************************************}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3conn;

const
  cLiteDb_Version = '2.13.03.2020';

type
  { TLiteDb }
  TLiteDb = class(TInterfacedObject)
  private
    fDb: TSQLite3Connection;
    fTrans: TSQLTransaction;
    fQuery: TSQLQuery;
    fDbName: string;
    function get_Connected: boolean;
    function get_Version: string;
    procedure set_DbName(aValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Connect(const aFilename: string): boolean; overload;
    function Connect: boolean; overload; { 08.06.2012 /bc }
    procedure DisConnect;
    procedure RunSQL(const aStatement: string); { writing: insert, update & delete }
    function QuerySQL(const aStatement: string; aMemDataSet: TDataset): boolean; { reading: select }
    property DbName: string read fDbName write set_DbName;
    property Connected: boolean read get_Connected;
    property Version: string read get_Version;
  end;

implementation
uses bc_memdataset;
{ TLiteDb }

function TLiteDb.get_Connected: boolean;
begin
  Result:= fDb.Connected;
end;

function TLiteDb.get_Version: string;
begin
  Result:= cLiteDb_Version;
end;

procedure TLiteDb.set_DbName(aValue: string);
begin
  if fDbName <> aValue then begin
    fDbName:= aValue;
    fDb.DatabaseName:= aValue;
  end;
end;

constructor TLiteDb.Create;
begin
  fDb:= TSQLite3Connection.Create(nil);
  fTrans:= TSQLTransaction.Create(nil);
  fTrans.DataBase:= fDb;
  fQuery:= TSQLQuery.Create(nil);
  fQuery.DataBase:= fDb;
  fQuery.Transaction:= fTrans;
end;

destructor TLiteDb.Destroy;
begin
  if Connected then DisConnect; { sanity check }
  FreeAndNil(fQuery);
  FreeAndNil(fTrans);
  FreeAndNil(fDb);
  inherited Destroy;
end;

function TLiteDb.Connect(const aFilename: string): boolean;
begin
  if Connected then DisConnect; { sanity check }
  set_DbName(aFilename);
  fDb.Open;
  Result:= fDb.Connected;
end;

function TLiteDb.Connect: boolean;
begin
  Result:= false;
  if fDbName = '' then raise Exception.Create('Error! TLiteDb.Connect: DbName is empty! Cannot connect!')
  else begin
    fDb.Open;
    Result:= fDb.Connected;
  end;
end;

procedure TLiteDb.DisConnect;
begin
  if fQuery.Active then fQuery.Close;
  if fTrans.Active then fTrans.Commit;
  if fDb.Connected then fDb.Close;
end;

procedure TLiteDb.RunSql(const aStatement: string); { writing: insert, update & delete }
begin
  if not fTrans.Active then fTrans.StartTransaction;
  try
    fQuery.Close;
    fQuery.SQL.Text:= aStatement;
    fQuery.Prepare;
    fQuery.ExecSQL;
    fQuery.Close;
  finally fTrans.Commit; end;
end;

function TLiteDb.QuerySQL(const aStatement: string; aMemDataSet: TDataset): boolean; { reading: select }
begin
  if not fTrans.Active then fTrans.StartTransaction;
  try
    Result:= false;
    fQuery.Close;
    fQuery.SQL.Text:= aStatement;
    fQuery.Open;
    TMemDataset(aMemDataSet).Close;      { cannot swap data in an open dataset }
    TMemDataset(aMemDataSet).Clear(true); { remove existing data and fielddefs }
    TMemDataset(aMemDataSet).CopyFromDataset(fQuery,true); { now copy from persistent data }
    fQuery.Close;
    Result:= true;
  finally fTrans.Commit; end;
end;

end.

