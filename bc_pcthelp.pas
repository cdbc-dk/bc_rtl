
{***************************************************************************
*        Unit name : bc_pcthelp.pas                                        *
*        Copyright : (C)cdbc.dk 2022                                       *
*        Programmer: Benny Christensen                                     *
*        Created   : 02.11.2022 /bc helper for dealing with tpagecontrol.  *
*        Updated   : 02.11.2022 /bc added functions for adding, searching  *
*                                   and deleting pages in a pagecontrol.   *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
*                                                                          *
****************************************************************************
*        Purpose:                                                          *
*        Helper functions / procedures to aid in manipulating and          *
*        working with TPageControl and TTabSheet.                          *
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

unit bc_pcthelp;
{$mode objfpc}{$H+}
{.$define debug}
interface

uses
  Classes, SysUtils, Controls, ComCtrls, Forms;

{ bcFindPageByName finds a page or creates a new one. can also set it active,
  returns true in the out param 'aNewPage' if it created one,
  result is a ttabsheet or nil }
function bcFindPageByName(aPageCtrl: TPageControl;      {*}
                          const aName: string;
                          SetActive: boolean;
                          AutoCreate: boolean;
                          out aNewPage: boolean): TTabSheet;
{ bcGetParentForm searches and returns the parent form, for use in sending messages }
function bcGetParentForm(aControl: TWinControl): TForm;

implementation

function bcFindPageByName(aPageCtrl: TPageControl;      {*}
                          const aName: string;
                          SetActive: boolean;
                          AutoCreate: boolean;
                          out aNewPage: boolean): TTabSheet;
var I: SizeInt;
begin
  Result:= nil; aNewPage:= false;
  if ((aPageCtrl = nil) or (aName = '')) then exit;
  for I:= 0 to aPageCtrl.PageCount-1 do begin
    if aPageCtrl.Pages[I].Caption = aName then begin
      Result:= aPageCtrl.Pages[I];
      aNewPage:= false;
      break;
    end;
  end;
  if ((Result = nil) and AutoCreate) then begin
    Result:= aPageCtrl.AddTabSheet;
    Result.Caption:= aName;
    aNewPage:= true;
  end;
  if (Result <> nil) and SetActive then aPageCtrl.ActivePage:= Result;
end; { FindPageByName }

function bcGetParentForm(aControl: TWinControl): TForm; //???
var
  O: TControl;
begin
  Result:= nil;
  if aControl = nil then exit;
  O:= aControl;
  while Result = nil do begin
    if O is TForm then begin
      Result:= TForm(O);
      break;
    end;
    if O.Owner is TApplication then begin
      Result:= TForm(O);
      break;
    end;
    O:= O.parent;
  end;
end;

(*
// TForm(anOwner.Parent.Parent);

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

