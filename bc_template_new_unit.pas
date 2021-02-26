
{**************************************************************************$
$        Unit name : u_treeview.pas                                        $
$        Copyright : (C)cdbc.dk 2021                                       $
$        Programmer: Benny Christensen                                     $
$        Created   : 2021.02.23 /bc helper for dealing with ttreeview      $
$        Updated   : 2021.02.23 /bc                                        $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$                                                                          $
$**************************************************************************$
$        Purpose   :                                                       $
$        One date interface / class, which                                 $
$        implements ISO weeknumber                                         $
$        calculations, as well as some                                     $
$        service functionality.                                            $
$        One time interface / class.                                       $
$        One IISoDateTime interface / class                                $
$        TODO: implement week-stuff                                        $
$**************************************************************************$
$        License   :                                                       $
$        "Beer License" - If you meet me one day, you'll buy me a beer :-) $
$        I'm NOT liable for anything! Use at your own risk!!!              $
$**************************************************************************}

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

