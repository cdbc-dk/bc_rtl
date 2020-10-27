
{------------------------------------------------------------------------------|
| Project name: Daily Diary                                                    |
| Unit name   : lfm_main.pas                                                   |
| Copyright   : (c) 2020 cdbc.dk                                               |
| Programmer  : Benny Christensen /bc                                          |
| Created     : 2020.09.28 /bc initial design and coding,(kind of MVC pattern) |
| Updated     : 2020.09.28 /bc Setting up environment, structure and vision    |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|------------------------------------------------------------------------------|
| Abstract:                                                                    |
|   Make an application to help with remembering what happened that day        |
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

