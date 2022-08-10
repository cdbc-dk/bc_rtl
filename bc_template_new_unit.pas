
{***************************************************************************
*        Unit name : bc_trvhelp.pas                                        *
*        Copyright : (C)cdbc.dk 2022                                       *
*        Programmer: Benny Christensen                                     *
*        Created   : 2022.07.31 /bc helper for dealing with ttreeview.     *
*        Updated   : 2022.08.03 /bc added functions for adding, searching  *
*                                   and deleting nodes in a treeview.      *
*                    2022.08.05 /bc updated cleartreeview to cater for all *
*                                   rootnodes...                           *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
****************************************************************************
*        Purpose:                                                          *
*        Helper functions / procedures to aid in manipulating and          *
*        working with TTreeview and TTreeNodes                             *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*        TODO:                                                             *
****************************************************************************
*        License:                                                          *
*        "Beer License" - If you meet me one day, you'll buy me a beer :-) *
*        I'm NOT liable for anything! Use at your own risk!!!              *
***************************************************************************}

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

