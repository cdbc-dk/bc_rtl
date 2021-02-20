
{------------------------------------------------------------------------------|
| Project name: Debug Server                                                   |
| Unit name   : lfm_main.pas                                                   |
| Copyright   : (c) 2021 cdbc.dk                                               |
| Programmer  : Benny Christensen /bc                                          |
| Created     : 2021.01.13 /bc initial design and coding                       |
| Updated     : 2020.01.13 /bc Setting up environment, structure and vision    |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|------------------------------------------------------------------------------|
| Abstract:                                                                    |
|   A debug server to connect to while running a live application.             |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
-------------------------------------------------------------------------------}

unit template_new_unit;
{$mode objfpc}{$H+}
{.$define debug}
interface

uses
  Classes, SysUtils;

//function Example: TObject; { global singleton }   

implementation

(*
var 
  __Example: TObject;

function Example: TObject; { singleton }
begin
  if not assigned(__Example) then __Example:= TObject.Create;
  Result:= __Example;
end; { gets released on progam end }
*)

initialization
//  __Example:= nil;

finalization 
//  FreeAndNil(__Example);
  
end.

