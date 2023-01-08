unit tfm_trv_mem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls;

type

  { TtrvmemFrame }

  TtrvmemFrame = class(TFrame)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Memo1: TMemo;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    procedure TreeView1SelectionChanged(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TtrvmemFrame }

procedure TtrvmemFrame.TreeView1SelectionChanged(Sender: TObject);
begin
  Memo1.Lines.Text:= TreeView1.Selected.Text;
end;

end.

