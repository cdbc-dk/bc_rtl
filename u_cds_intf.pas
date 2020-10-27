unit u_cds_intf;
{$mode objfpc}{$H+}
{.$define debugging}
interface
uses
  Classes, SysUtils, db,
  litedb;

type
  { delegate event }
  TExecSqlEvent = procedure(DataSet: TDataSet;var anSQLStatement: string) of object;
  { observer interface }
  IClientDataset = interface; // forward decl.
  IObserver = interface ['{4578802F-6E7E-4C48-90A8-25A12D26F598}']
    procedure Update(aSubject: IClientDataset); // IInterface
  end;
  { clientdataset }
  IClientDataset = interface ['{78315B6E-1705-4E0A-BF5B-337922009382}']
    function get_OnExecInsert: TExecSqlEvent;
    function get_OnExecUpdate: TExecSqlEvent;
    function get_OnExecDelete: TExecSqlEvent;
    function get_LiteDb: TLiteDb;
    function get_SelectSQL: string;
    function get_InsertSQL: string;
    function get_UpdateSQL: string;
    function get_DeleteSQL: string;
    function get_Cds_Version: string;
    function get_Dataset: TDataset;
    procedure set_OnExecInsert(aValue: TExecSqlEvent);
    procedure set_OnExecUpdate(aValue: TExecSqlEvent);
    procedure set_OnExecDelete(aValue: TExecSqlEvent);
    procedure set_LiteDb(aValue: TLiteDb);
    procedure set_SelectSQL(aValue: string);
    procedure set_InsertSQL(aValue: string);
    procedure set_UpdateSQL(aValue: string);
    procedure set_DeleteSQL(aValue: string);
    { observer pattern }
    procedure Register_Observer(anObserver: IObserver);
    procedure UnRegister_Observer(anObserver: IObserver);
    procedure Notify;
    function get_State: integer;
    procedure set_State(const Value: integer);
    { observer pattern }

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
    property Dataset: TDataset read get_Dataset; // read only
    { observer pattern }
    property State: integer read get_State write set_State;
    { observer pattern }
  end; { IClientDataset }

  { class exporting function }
  function cds_CreateClientDataset: IClientDataset;

implementation
uses
  u_clientdataset;

function cds_CreateClientDataset: IClientDataset;
begin { class exporting function }
  Result:= TClientDataset.Create(nil) as IClientDataset; { refcount = 1 }
end;

end.
