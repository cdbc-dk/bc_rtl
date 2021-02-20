unit bc_litedb_blob;
{$mode objfpc}{$H+}
{$define new_blob_code} { 26.04.2020 /bc refactored code }
interface
uses
  Classes, SysUtils, db, sqldb, sqlite3conn;

const
  {$ifdef new_blob_code}
    UnitVersion = '02.26.04.2020';
    SLiteDbGUID = '{D62946BD-B561-43B3-983C-51A7EA8F757C}';
    { lasterror constants }
    LE_OK       = 0;
    LE_NODBNAME = 1;
    { lasterror descriptions }
    LE_DESC: array[0..4] of string = ('Ok',
                                      'Error: No databasename specified',
                                      'Reserved',
                                      'Reserved',
                                      'Reserved');

  {$else}
    UnitVersion = '1.23.04.2020';
  {$endif}

type
  {$ifdef new_blob_code}
  { *** ILiteDb *** }

  { ILiteDb }

  ILiteDb = interface [SLiteDbGUID]
  { private }
    function get_Connected: boolean;             { private }
    function get_Connection: TSQLite3Connection; { private }
    function get_DbName: string;                 { private }
    function get_Query: TSQLQuery;               { private }
    function get_Transaction: TSQLTransaction;   { private }
    function get_Version: string;                { private }
    function get_LastError: ptruint;             { private 05.06.2020 /bc }
    function get_LastErrorDesc: string;          { private 05.06.2020 /bc }
    procedure set_DbName(aValue: string);        { private }
  { public api }
    function Connect(const aFilename: string): boolean; overload;
    function Connect: boolean; overload; { 08.06.2012 /bc }
    procedure DisConnect;
    procedure RunSQL(const aStatement: string); { writing: insert, update & delete }
    function QuerySQL(const aStatement: string; aMemDataSet: TDataset): boolean; { reading: select }
    property DbName: string read get_DbName write set_DbName; { 26.04.2020 /bc }
    property Connected: boolean read get_Connected;
    property Version: string read get_Version;
  { surface the connection details, for blob/parameters use }
    property Connection: TSQLite3Connection read get_Connection; { 26.04.2020 /bc }
    property Transaction: TSQLTransaction read get_Transaction; { 26.04.2020 /bc }
    property Query: TSQLQuery read get_Query;             { 26.04.2020 /bc }
    property LastError: ptruint read get_LastError; { 05.06.2020 /bc }
    property LastErrorDesc: string read get_LastErrorDesc; { 05.06.2020 /bc }
  end;
  { *** TLiteDb *** }

  { TLiteDb }

  TLiteDb = class(TInterfacedObject,ILiteDb)
  {$else}
  TLiteDb = class(TInterfacedObject)
  {$endif}
  private
    fDb: TSQLite3Connection;
    fTrans: TSQLTransaction;
    fQuery: TSQLQuery;
    fDbName: string;
    fLastError: ptruint; { 05.06.2020 /bc }
    function get_Connected: boolean;
    {$ifdef new_blob_code}
      function get_Connection: TSQLite3Connection; { 26.04.2020 /bc }
      function get_DbName: string; { 26.04.2020 /bc }
      function get_Query: TSQLQuery; { 26.04.2020 /bc }
      function get_Transaction: TSQLTransaction; { 26.04.2020 /bc }
      function get_LastError: ptruint;             { private 05.06.2020 /bc }
      function get_LastErrorDesc: string; { 05.06.2020 /bc }
    {$endif}
    function get_Version: string;
    procedure set_DbName(aValue: string);
  public
    constructor Create; overload;
    constructor Create(const aDatabaseName: string); overload;
    destructor Destroy; override;
    function Connect(const aFilename: string): boolean; overload;
    function Connect: boolean; overload; { 08.06.2012 /bc }
    procedure DisConnect;
    procedure RunSQL(const aStatement: string); { writing: insert, update & delete }
    function QuerySQL(const aStatement: string; aMemDataSet: TDataset): boolean; { reading: select }
    property DbName: string read get_DbName write set_DbName;
    property Connected: boolean read get_Connected;
    property Version: string read get_Version;
    { surface the connection details, for blob/parameters use }
    property Connection: TSQLite3Connection read get_Connection; { 26.04.2020 /bc }
    property Transaction: TSQLTransaction read get_Transaction; { 26.04.2020 /bc }
    property Query: TSQLQuery read get_Query;             { 26.04.2020 /bc }
    property LastError: ptruint read get_LastError; { 05.06.2020 /bc }
    property LastErrorDesc: string read get_LastErrorDesc; { 05.06.2020 /bc }
  end; { *** TLiteDb *** }

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

{ *** TLiteDb *** }
function TLiteDb.get_Connected: boolean; {*}
begin
  Result:= fDb.Connected;
end;

function TLiteDb.get_Connection: TSQLite3Connection; {*}
begin
  Result:= fDb;
end;

function TLiteDb.get_DbName: string; {*}
begin
  Result:= fDbName;
end;

function TLiteDb.get_Query: TSQLQuery; {*}
begin
  Result:= fQuery;
end;

function TLiteDb.get_Transaction: TSQLTransaction; {*}
begin
  Result:= fTrans;
end;

function TLiteDb.get_LastError: ptruint; {*}
begin
  Result:= fLastError;
end;

function TLiteDb.get_LastErrorDesc: string; {*}
begin
  Result:= LE_DESC[fLastError]; { flasterror >= 0 }
end;

function TLiteDb.get_Version: string; {*}
begin
  Result:= UnitVersion;
end;

{ setup the name of database file to be used }
procedure TLiteDb.set_DbName(aValue: string); {*}
begin
  if fDbName <> aValue then begin
    fDbName:= aValue;
    fDb.DatabaseName:= aValue;
  end;
end;

{ create litedb object, set dbname later }
constructor TLiteDb.Create; {*}
begin
  fDb:= TSQLite3Connection.Create(nil);
  fTrans:= TSQLTransaction.Create(nil);
  fTrans.DataBase:= fDb;
  fQuery:= TSQLQuery.Create(nil);
  fQuery.DataBase:= fDb;
  fQuery.Transaction:= fTrans;
end;

{ create with database name }
constructor TLiteDb.Create(const aDatabaseName: string); {*}
begin
  Create; { use colleague }
  set_DbName(aDatabaseName);
end;

{ elvis is leaving the building :) }
destructor TLiteDb.Destroy; {*}
begin
  if Connected then DisConnect; { sanity check }
  FreeAndNil(fQuery);
  FreeAndNil(fTrans);
  FreeAndNil(fDb);
  inherited Destroy;
end;

{ connect to a database, specifying the dbname }
function TLiteDb.Connect(const aFilename: string): boolean; {*}
begin
  if Connected then DisConnect; { sanity check }
  set_DbName(aFilename);
  fDb.Open;
  Result:= fDb.Connected;
end;

{ connect to a known database, ie: dbname <> '' }
function TLiteDb.Connect: boolean; {*}
begin
  Result:= false;
  {$ifdef new_blob_code}
    if fDbName = '' then begin
      fLastError:= LE_NODBNAME;   //ææ
      Result:= false;
    end else begin
      fDb.Open;
      Result:= fDb.Connected;
    end;
  {$else}
    if fDbName = '' then raise Exception.Create('Error! TLiteDb.Connect: DbName is empty! Cannot connect!')
    else begin
      fDb.Open;
      Result:= fDb.Connected;
    end;
  {$endif}
end;

{ disconnect query, transaction & connection }
procedure TLiteDb.DisConnect; {*}
begin
  if fQuery.Active then fQuery.Close;
  if fTrans.Active then fTrans.Commit;
  if fDb.Connected then fDb.Close;
end;

{ writing: insert, update & delete, works on simple tables ONLY! }
procedure TLiteDb.RunSQL(const aStatement: string); {*}
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

{ reading: select, doesn't support blobs }
function TLiteDb.QuerySQL(const aStatement: string; aMemDataSet: TDataset): boolean; {*}
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

initialization
  Singleton:= nil;
finalization
  FreeAndNil(Singleton);
end.

