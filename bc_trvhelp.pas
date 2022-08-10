
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

unit bc_trvhelp;
{$mode objfpc}{$H+}
{$define debug}
interface
uses
  bc_errorlog, Classes, SysUtils, LazUTF8, ComCtrls;

{type}

{ helper functions }
{ add a rootnode to treeview and return a reference to it, only 1 root node }
function AddRootNode(aTree: TTreeView;
                     const aCaption: string;
                     aData: pointer): TTreeNode;
{ add a childnode with a reference to data, to the treeview, return the new node }
function AddChildNodeWithData(aTree: TTreeView;
                              aParent: TTreeNode;
                              const aCaption: string;
                              aData: pointer): TTreeNode;
{ add a siblingnode to previoussiblingnode, with a data reference, to tree }
function AddSiblingNodeWithData(aTree: TTreeView;
                                aBrother: TTreeNode;
                                const aCaption: string;
                                aData: pointer): TTreeNode;
{ search the whole tree, for a node with matching text, return nil on not found }
function GetNodeByText(aTree: TTreeView;
                       const aValue: string;
                       aVisible: boolean): TTreeNode;
{ search the whole tree, for a node with matching text,
  but compare only on the same level,return nil on not found }
function GetNodeByTextAtLevel(aTree: TTreeView;
                              const aValue: string;
                              aVisible: boolean;
                              aLevel: integer): TTreeNode;
{ does a node with "text" exist in treeview }
function SearchTreeViewBool(aTree: TTreeView;const aSearchString: string): boolean;
{ delete selected node, if selected node has child nodes, they will be lost! }
procedure DeleteSelectedTreeNode(aTree: TTreeView);
{ clears the treeview, with a choice to keep rootnode or not }
procedure ClearTreeview(aTree: TTreeView;SpareRoot: boolean = true);


//function Example: TObject; { global singleton }   

implementation
{$ifdef debug}
uses lfm_main;
var  Dt: string;
{$endif}

function AddRootNode(aTree: TTreeView;
                     const aCaption: string;
                     aData: pointer): TTreeNode;
begin
  Result:= nil;
  if aTree.Items.Count = 0 then begin         { create only a parent root node }
    Result:= aTree.Items.AddFirst(nil,aCaption);
    Result.Selected:= true;         { make sure that our root-node is selected }
    Result.Data:= aData;                      { assign a reference to our data }
  end;
  {$ifdef debug}
    Dt:= DateTimeToStr(Now);
    lfm_main.frmMain.memDebug.Lines.Add('Root-node added: '+aCaption+' with data');
    ErrorLog.LogLn(Dt+' - Root-node added: '+aCaption+' with data');
  {$endif}
end; { AddRootNode }

function AddChildNodeWithData(aTree: TTreeView;
                              aParent: TTreeNode;
                              const aCaption: string;
                              aData: pointer): TTreeNode;
begin
  if aTree.Items.Count = 0 then exit;
  Result:= aTree.Items.AddChildObject(aParent,aCaption,aData);
end; { AddChildNodeWithData }

function AddSiblingNodeWithData(aTree: TTreeView;
                                aBrother: TTreeNode;
                                const aCaption: string;
                                aData: pointer): TTreeNode;
begin
  if aTree.Items.Count = 0 then exit;
  Result:= aTree.Items.AddObject(aBrother,aCaption,aData);
end; { AddSiblingNodeWithData }

function GetNodeByText(aTree: TTreeView;
                       const aValue: string;
                       aVisible: boolean): TTreeNode;
var
  Node: TTreeNode;
begin
  Result:= nil;
  if aTree.Items.Count = 0 then exit;
  {$ifdef debug}
    Dt:= DateTimeToStr(Now);
    lfm_main.frmMain.memDebug.Lines.Add('*GetNodeByText: '+aValue);
    ErrorLog.LogLn(Dt+' - GetNodeByText: '+aValue);
  {$endif}
  Node:= aTree.Items[0]; { start at rootnode }
  while Node <> nil do begin
    {$ifdef debug}
      lfm_main.frmMain.memDebug.Lines.Add('Visiting: '+Node.Text+' Level: '+Node.Level.ToString);
      ErrorLog.LogLn('Visiting: '+Node.Text+' Level: '+Node.Level.ToString);
    {$endif}
    if UpperCase(Node.Text) = UpperCase(aValue) then begin
      Result:= Node;
      if aVisible then Result.MakeVisible;
      {$ifdef debug}
        lfm_main.frmMain.memDebug.Lines.Add('Found: '+Node.Text+' @ Level: '+Node.Level.ToString);
        ErrorLog.LogLn('Found: '+Node.Text+' @ Level: '+Node.Level.ToString);
      {$endif}
      break;
    end;
    Node:= Node.GetNext;
  end;
end; { GetNodeByText }

function GetNodeByTextAtLevel(aTree: TTreeView;
                              const aValue: string;
                              aVisible: boolean;
                              aLevel: integer): TTreeNode;
var
  Node: TTreeNode;
begin
  Result:= nil;
  if aTree.Items.Count = 0 then exit; { nothing to do }
  Node:= aTree.Items[0]; { ie.: root-node }
  while Node <> nil do begin
    if (UpperCase(Node.Text) = UpperCase(aValue)) and (Node.Level = aLevel) then begin
      Result:= Node;
      if aVisible then Result.MakeVisible;
      break;
    end;
    Node:= Node.GetNext;
  end;
end; { GetNodeByTextAtLevel }

function SearchTreeViewBool(aTree: TTreeView;const aSearchString: string): boolean;
var
  Node: TTreeNode;
begin
  Result:= false;
  Node:= nil;
  Node:= GetNodeByText(aTree,aSearchString,false);
  if Node <> nil then Result:= true;
end; { SearchTreeViewBool }

procedure DeleteSelectedTreeNode(aTree: TTreeView);
var
  RootNode: TTreeNode;
  { a subprocedure to recursively delete nodes }
  procedure DeleteNode(aNode: TTreeNode);
  begin
    while aNode.HasChildren do DeleteNode(aNode.GetLastChild);
    if aNode <> RootNode then aTree.Items.Delete(aNode);
  end;
begin { if selected node has child nodes, they will be lost! }
  if aTree.Selected = nil then exit;                        { nothing to do... }
  RootNode:= aTree.Items[0];     { get a hold of the root node in the treeview }
  DeleteNode(aTree.Selected);   { recursive deleting nodes apart from rootnode }
end; { DeleteSelectedTreeNode }

procedure ClearTreeview(aTree: TTreeView;SpareRoot: boolean = true);
var RootNode: TTreeNode;
begin
  if aTree.Items.Count = 0 then exit;                       { nothing to do... }
  while aTree.Items.Count >0 do begin                   { remove all rootnodes }
    RootNode:= atree.Items[0];      { get a hold of the rootnode in the treeview }
    aTree.Select(RootNode);                 { make sure the rootnode is selected }
    DeleteSelectedTreeNode(aTree);    { recursively remove nodes apart from root }
    if not SpareRoot then aTree.Items.Delete(RootNode);    { finally remove root }
  end;
end; { ClearTreeview }

(*

procedure TForm1.Button1Click(Sender: TObject);
var
  MyTreeNode1, MyTreeNode2: TTreeNode;
begin
  with TreeView1.Items do
  begin
    Clear; { Remove any existing nodes. }
    MyTreeNode1 := Add(nil, 'RootTreeNode1'); { Add a root node. }
    { Add a child node to the node just added. }
    AddChild(MyTreeNode1,'ChildNode1');

    {Add another root node}
    MyTreeNode2 := Add(MyTreeNode1, 'RootTreeNode2');
    {Give MyTreeNode2 to a child. }
    AddChild(MyTreeNode2,'ChildNode2');

    {Change MyTreeNode2 to ChildNode2 }
    { Add a child node to it. }
    MyTreeNode2 := TreeView1.Items[3];
    AddChild(MyTreeNode2,'ChildNode2a');

    { Add another child to ChildNode2, after ChildNode2a. }
    AddChild(MyTreeNode2,'ChildNode2b');

    { Add another root node. }
    Add(MyTreeNode1, 'RootTreeNode3');
  end;
end;

var 
  __Example: TObject;

function Example: TObject; { singleton }
begin
  if not assigned(__Example) then __Example:= TObject.Create;
  Result:= __Example;
end; { gets released on progam end }
*)

initialization
  ErrorLog.Filename:= 'action_history_trv.log';
  {$ifdef debug} Dt:= DateTimeToStr(Now); {$endif}
//  __Example:= nil;

finalization 
//  FreeAndNil(__Example);
  
end.

