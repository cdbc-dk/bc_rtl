unit u_lib_intf;
{$mode objfpc}{$H+}
{.$define debugging}
interface
uses
  Classes, SysUtils;

const
  SRT = 0;    { ~ TSubFormat }
  SUB = 1;
  None = 2;

type
  { TSubFormat }
  TSubFormat = (sfSRT,sfSUB,sfNone);
  { exported interfaces }
  ISubEntry = interface ['{76D1F22A-C6EA-4F7B-B32C-90640182AA08}']
    function get_Id: integer;                         { added 14.01.2011 by bc }
    function get_Separator: ansistring;               { added 14.01.2011 by bc }
    function get_Text: TStrings;                      { added 14.01.2011 by bc }
    function get_AsString: ansistring;
    function get_Done: TDateTime;
    function get_DoneAsString: ansistring;
    function get_Duration: TDateTime;
    function get_Start: TDateTime;
    function get_StartAsMSecs: integer;
    function get_StartAsString: ansistring;
    function get_DurationAsString: ansistring;
    procedure set_Id(const aValue: integer);          { added 14.01.2011 by bc }
    procedure set_Separator(const aValue: ansistring);{ added 14.01.2011 by bc }
    procedure set_Text(const aValue: TStrings);       { added 14.01.2011 by bc }
    procedure set_AsString(const aValue: ansistring);
    procedure set_Done(const aValue: TDateTime);
    procedure set_DoneAsString(const aValue: ansistring);
    procedure set_Duration(const aValue: TDateTime);
    procedure set_Start(const aValue: TDateTime);
    procedure set_StartAsString(const aValue: ansistring);
    function get_IdAsString: string;
    procedure SlideByFactor(const aFactor: integer); // accepts negatives
    procedure SetTextAsString(aText: ansistring); { ex: 'string1|string2|string3' }
    property Id: integer read get_Id write set_Id;
    property IdAsString: string read get_IdAsString;
    property Start: TDateTime read get_Start write set_Start;
    property StartAsMSecs: integer read get_StartAsMSecs;
    property Done: TDateTime read get_Done write set_Done;
    property Duration: TDateTime read get_Duration write set_Duration;
    property Separator: ansistring read get_Separator write set_Separator;
    property Text: TStrings read get_Text write set_Text;
    property StartAsString: ansistring read get_StartAsString write set_StartAsString;
    property DoneAsString: ansistring read get_DoneAsString write set_DoneAsString;
    property DurationAsString: ansistring read get_DurationAsString;
    property AsString: ansistring read get_AsString write set_AsString;
  end; { ISubEntry }

  ISubEntriesEnumerator = interface ['{1FDF0FAD-69EC-419D-8F19-2117C826D44A}']
    procedure Reset;                                  { added 14.01.2011 by bc }
    function GetCurrent: ISubEntry;
    function MoveNext: Boolean;
    property Current: ISubEntry read GetCurrent;
  end; { ISubEntriesEnumerator }

  ISubEntries = interface ['{AF3E0F81-437E-495B-98FB-6E66916D0596}']
    function CheckFilename(const aFilename: ansistring): ansistring;
    function CheckForNullEntries: boolean;
    function get_Merging: boolean;                    { added 14.01.2011 by bc }
    function get_RecreateIds: boolean;                { added 14.01.2011 by bc }
    function get_Modified: boolean;                   { added 14.01.2011 by bc }
    function get_Count: integer;
    function get_Entries(Index: integer): ISubEntry;
    procedure Parse_SRT_Format;
    procedure Parse_SUB_Format;
    procedure set_Merging(const aValue: boolean);     { added 14.01.2011 by bc }
    procedure set_RecreateIds(const aValue: boolean); { added 14.01.2011 by bc }
    procedure set_Modified(const aValue: boolean);    { added 14.01.2011 by bc }
    procedure set_Entries(Index: integer; const AValue: ISubEntry);
    procedure Save_SRT_Format(const aFilename: ansistring);
    procedure Save_SUB_Format(const aFilename: ansistring);
    function get_Enumerator: ISubEntriesEnumerator;
//    function GetEnumerator: TSubEntriesEnumerator; { bds support for the 'for .. in' constuct }
    function GetEntryFromId(anId: integer): ISubEntry; { support for editing entries, returns nil if not found }
    procedure ClearEntries;
    function NewEntry: ISubEntry;
    function AddEntry(anEntry: ISubEntry): boolean;
    function LoadFromFile(const aFilename: ansistring;aFormat: TSubFormat): boolean;
    procedure Resync(const aFactor,IdEntrypoint,IdExitpoint: integer);// positive => forwards, negative => backwards
    procedure SaveToFile(const aFilename: ansistring;aFormat: TSubFormat);
    procedure Merge(const aFilename1,aFilename2: ansistring;aFormat: TSubFormat);
    property Count: integer read get_Count;
    property Entries[Index: integer]: ISubEntry read get_Entries write set_Entries; default;
    property Merging: boolean read get_Merging write set_Merging;
    property RecreateIds: boolean read get_RecreateIds write set_RecreateIds; { ææ 05.10.2010 }
    property Modified: boolean read get_Modified write set_Modified;
    property Enumerator: ISubEntriesEnumerator read get_Enumerator;
  end; { ISubEntries }

  { class exporting function }
  function ec_ExportSubEntries: ISubEntries;

implementation
uses
  usubentries;

function ec_ExportSubEntries: ISubEntries;
begin { class exporting function }
  Result:= TSubEntries.Create as ISubEntries; { refcount = 1 }
end;

end.

