unit bc_litedb;
{************************************************
*                                               *
* A lightweight database connection for SQLite3 *
* Wraps a TSQLite3Connection, TSQLTransaction   *
* and a TSQLQuery all into one.                 *
*                                               *
* Revision 4.27.07.2022 /bc                     *
*                                               *
************************************************}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3conn;

const
  cLiteDb_Version = '4.27.07.2022';
  { lasterror constants }
  LE_OK       = 0;
  LE_NODBNAME = 1;
  { lasterror descriptions }
  LE_DESC: array[0..4] of string = ('Ok',
                                    'Error: No databasename specified',
                                    'Reserved',
                                    'Reserved',
                                    'Reserved');

type
  { TLiteDb }
  TLiteDb = class                                    { (TInterfacedObject) ?!? }
  private
    function get_Exec: TSQLQuery;
    function get_LastInsertedId: int64;
    function get_LibVersion: string;
  protected
    fDb: TSQLite3Connection;
    fTrans: TSQLTransaction;
    fQuery: TSQLQuery;
    fExec: TSQLQuery; // advice from 'rvk' used for execsql etc. 27.07.2022 /bc
    fDbName: string;
    fLastError: ptruint;                                  { 30.01.2021 /bc }
    function get_Connected: boolean; //*
    function get_Connection: TSQLite3Connection;          { 30.01.2021 /bc }
    function get_Transaction: TSQLTransaction;            { 30.01.2021 /bc }
    function get_Query: TSQLQuery;                        { 30.01.2021 /bc }
    function get_Version: string; //*
    function get_DbName: string;                          { 30.01.2021 /bc }
    function get_LastError: ptruint;                      { 05.06.2020 /bc }
    function get_LastErrorDesc: string;                   { 05.06.2020 /bc }
    procedure set_DbName(aValue: string); //*
  public
    constructor Create; overload;
    constructor Create(const aDatabaseName: string); overload;
    destructor Destroy; override;
    function Connect(const aFilename: string): boolean; overload;
    function Connect: boolean; overload;                  { 08.06.2012 /bc }
    procedure DisConnect;
    procedure RunSQL(const aStatement: string); { writing: insert, update & delete }
    function QuerySQL(const aStatement: string; aMemDataSet: TDataset): boolean; { reading: select }
    property LastInsertedId: int64 read get_LastInsertedId; { 25.07.2022 /bc }
    property DbName: string read fDbName write set_DbName;
    property Connected: boolean read get_Connected;
    property Version: string read get_Version;
    property LibVersion: string read get_LibVersion;
    { surface the connection details, for blob/parameters use }
    property Connection: TSQLite3Connection read get_Connection; { 26.04.2020 /bc }
    property Transaction: TSQLTransaction read get_Transaction;  { 26.04.2020 /bc }
    property Exec: TSQLQuery read get_Exec;                 { 27.07.2022 /bc }
    property Query: TSQLQuery read get_Query;               { 26.04.2020 /bc }
    property LastError: ptruint read get_LastError;   { 0 = succes, >0 = error }
    property LastErrorDesc: string read get_LastErrorDesc;    { human readable }
  end;

{ *** singleton factory *** }
function LiteDb: TLiteDb;

implementation
uses bc_memdataset;
var Singleton: TLiteDb;

{ singleton factory }
function LiteDb: TLiteDb;
begin
  if not assigned(Singleton) then Singleton:= TLiteDb.Create;
  Result:= Singleton;
end;

{ TLiteDb }
function TLiteDb.get_Connected: boolean;
begin
  Result:= fDb.Connected;
end;

function TLiteDb.get_Connection: TSQLite3Connection;
begin
  Result:= fDb;
end;

function TLiteDb.get_Transaction: TSQLTransaction;
begin
  Result:= fTrans;
end;

function TLiteDb.get_Exec: TSQLQuery;
begin
  Result:= fExec;
end;

function TLiteDb.get_Query: TSQLQuery;
begin
  Result:= fQuery;
end;

function TLiteDb.get_Version: string;
begin
  Result:= cLiteDb_Version;
end;

function TLiteDb.get_DbName: string;
begin
  Result:= fDbName;
end;

{ a integer representation of the lasterror }
function TLiteDb.get_LastError: ptruint;
begin
  Result:= fLastError;
end;

{ a string representation of the lasterror }
function TLiteDb.get_LastErrorDesc: string;
begin
  Result:= LE_DESC[fLastError]; { flasterror >= 0 }
end;

{ setup the name of database file to be used }
procedure TLiteDb.set_DbName(aValue: string);
begin
  if fDbName <> aValue then begin
    fDbName:= aValue;
    fDb.DatabaseName:= aValue;
  end;
end;

{ create litedb object, set dbname later }
constructor TLiteDb.Create;
begin
  fDb:= TSQLite3Connection.Create(nil); { keep connection open till destruction }
  fTrans:= TSQLTransaction.Create(nil);
  fTrans.DataBase:= fDb;
  { query for select statements, reading. advice from 'rvk' }
  fQuery:= TSQLQuery.Create(nil);
  fQuery.DataBase:= fDb;
  fQuery.Transaction:= fTrans;
  { query for exec statements, create, update, delete. NO select! advice from 'rvk' }
  fExec:= TSQLQuery.Create(nil);
  fExec.DataBase:= fDb;
  fExec.Transaction:= fTrans;
end;

{ create with database name }
constructor TLiteDb.Create(const aDatabaseName: string);
begin
  Create; { use colleague }
  set_DbName(aDatabaseName);
end;

{ elvis is leaving the building :) }
destructor TLiteDb.Destroy;
begin
  if Connected then DisConnect; { connection will be closed here }
  FreeAndNil(fExec); { \  }
  FreeAndNil(fQuery); {  \ }
  FreeAndNil(fTrans); {  |- kill'em all :-) }
  FreeAndNil(fDb);    { /  }
  inherited Destroy;
end;

{ connect to database with specified filename }
function TLiteDb.Connect(const aFilename: string): boolean;
begin
  if Connected then DisConnect; { sanity check }
  set_DbName(aFilename);
  fDb.Open;
  Result:= fDb.Connected;
end;

{ connect to a known database, ie: dbname <> '' }
function TLiteDb.Connect: boolean;
begin
  Result:= false;
  if fDbName = '' then begin
    fLastError:= LE_NODBNAME;
    raise Exception.Create('Error! TLiteDb.Connect: DbName is empty! Cannot connect!')
  end else begin
    if not fDb.Connected then fDb.Open;
    Result:= fDb.Connected;
  end;
end;

{ disconnect query, transaction & connection }
procedure TLiteDb.DisConnect;
begin
  if fExec.Active then fExec.Close;
  if fQuery.Active then fQuery.Close;
  if fTrans.Active then fTrans.Commit;
  if fDb.Connected then fDb.Close;
end;

{ writing: insert, update & delete, works on simple tables ONLY! }
procedure TLiteDb.RunSQL(const aStatement: string); { writing: insert, update & delete }
begin
  if not fTrans.Active then fTrans.StartTransaction;
  try
    fExec.SQL.Text:= aStatement;
    fExec.ExecSQL;
  finally fTrans.Commit; end; { or fTrans.CommitRetaining; advice from 'rvk' }
end;

{ does what the name says }
function TLiteDb.get_LastInsertedId: int64;
begin
  Result:= fDb.GetInsertID;
end;

{ returns the version of the SQLite library itself }
function TLiteDb.get_LibVersion: string;
begin
  Result:= fDb.GetConnectionInfo(citServerVersionString);
end;

{ reading: select, doesn't support blobs }
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

