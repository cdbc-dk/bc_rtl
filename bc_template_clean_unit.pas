
{***************************************************************************
*        Unit name : bc_trvhelp.pas                                        *
*        Copyright : (C)cdbc.dk 2022                                       *
*        Programmer: Benny Christensen                                     *
***************************************************************************}

unit template_clean_unit;
{$mode objfpc}{$H+}
{$interfaces corba}
{$define debug}
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
{$interfaces com}  
end.

