

        {***************************************************************
        * Unit name : bc_types.pas                                     *
        * Copyright : (c) 2020 cdbc.dk                                 *
        * Programmer: Benny Christensen /bc                            *
        * Created   : 2020.04.18 /bc: TVersion created.                *
        * Updated   : 2020.04.20 /bc: Refactored the math in both      *
        *                             TVersion.EncodeFromString and    *
        *                             TVersion.DecodeToString. it      *
        *                             now relies on pointer magic :-)  *
        *                             Implementation moved to include. *
        *                                                              *
        ****************************************************************
        *                                                              *
        * Abstract:                                                    *
        * Provides different types for use in bc-codelibrary,          *
        * sort of a global type-repository.                            *
        *                                                              *
        *                                                              *
        *                                                              *
        ***************************************************************}

unit bc_types;
{$mode objfpc}{$H+}
{.$define use_lcltype}
interface
uses
  Classes, SysUtils;
  {$ifdef use_lcltype}

  {$else}

  {$endif}
const
  UnitVersion = '2.20.04.2020'; { TVersion uses this }

type
  { *** TVersion *** }
  TVersion = class
  private
    fVersionNumber: ptruint;
    function get_AsPtrUint: ptruint;
    function get_AsString: string;
    function EncodeFromString(const aVersionString: string): ptruint;
    function DecodeToString(const aVersionNumber: ptruint): string;
  public
    constructor Create(const aVersionNumber: ptruint); overload;
    constructor Create(const aUnitVersion: string); overload;
    destructor Destroy; override;
    class function VersionNumberToString(const aVersionNumber: ptruint): string;
    class function VersionStringToPtrUint(const aVersionString: string): ptruint;
    property AsPtrUint: ptruint read get_AsPtrUint;
    property AsString: string read get_AsString;
  end; { TVersion }



{ factory provides a global object, singleton on demand }
//Function Factory: TFactory;

implementation

{$i version.inc} { moved the implementation part to an include-file }


(*
var
  Singleton: TSingleton; { a mix of the factory and the singleton patterns }

Function Factory: TFactory;
begin
  if not assigned(Singleton) then Singleton:= TFactory.Create;
  Result:= Singleton;
end; { gets released on progam end }
*)












initialization
//  Singleton:= nil;
finalization
//  FreeAndNil(Singleton);
end.

