
{*******************************************************************************
*        Unit name : bc_bomintf.pas                                            *
*        Copyright : (C)cdbc.dk 2022                                           *
*        Programmer: Benny Christensen                                         *
*        Created   : 19.10.2022 /bc Base interface for business object model   *
*        Updated   : 20.10.2022 /bc Added functions for adding, searching &    *
*                                   enumerating items, basic expected          *
*                                   behaviour of a bom. Data storage agnostic  *
*                                                                              *
*                                                                              *
*                                                                              *
*                                                                              *
*                                                                              *
********************************************************************************
*        Purpose:                                                              *
*        Basic contract, that promises these routines are available in a bom.  *
*        As a base interface the user is expected to do some typecasting or    *
*        create a descendant interface / class, to deal with different item-   *
*        types.                                                                *
*                                                                              *
*                                                                              *
*        TODO:                                                                 *
********************************************************************************
*        License:                                                              *
*        "Beer License" - If you meet me one day, you'll buy me a beer :-)     *
*        I'm NOT liable for anything! Use at your own risk!!!                  *
*******************************************************************************}

unit bc_bomintf;
{$mode objfpc}{$H+}
{$interfaces corba} { NO reference counting! }
{.$define debug}
interface
uses
  Classes, SysUtils;
const
  { GUID for IBom }
  SGUIDIBom  = '{2A1C85B4-163C-42A8-909D-2A2D490DBE06}';
type
  { callback event-method for enumeration }
  TbomEnumerateEvent = procedure(aSender: TObject;anItem: TObject;UserData: pointer;var aCancel: boolean) of object;
  { callback procedure for enumeration (functional) }
  TbomEnumerateCallback = procedure(aSender: TObject;anItem: TObject;UserData: pointer;var aCancel: boolean);

  { base interface for business object model BOM }
  IBom = interface [SGUIDIBom]
    function get_ItemCount: ptruint;
    function get_ItemClass: TClass;
    function get_Modified: boolean;
    { adds an item of type provided in constructor * }
    function AddItem: TObject;
    { clears the bom of any items, owned or not * }
    procedure Clear;
    { returns the item with Item.Id = anId or nil if not found }
    function GetItemObjFromID(const anId: ptruint): TObject;
    { returns -1 on not found else dataset ID index }
    function IndexOfObj(anItem: TObject): ptrint;
    { reads simple table-data, no blobs }
    function ReadData: boolean;              { storage agnostic 2022-08-16 /bc }
    { reads data with blob support }
    function ReadDataWithBlob(const Asc: boolean): boolean; { storage agnostic 2022-08-16 /bc }
    { updates data in the backend, datastorage (storage agnostic) }
    function UpdateData: boolean; overload;
    { enumerator, fires callback on every item, cancel breaks the loop }
    procedure Enumerate(UserData: pointer;aCallback: TbomEnumerateEvent); overload;
    { how many items are there? }
    property ItemCount: ptruint read get_ItemCount;
    { what kind of item are they? }
    property ItemClass: TClass read get_ItemClass;
    { has data changed along the way? }
    property Modified: boolean read get_Modified;
  end; { IBom }


implementation

initialization
//  __Example:= nil;

finalization 
//  FreeAndNil(__Example);
{$interfaces com} { reference counted! }
end.

