unit lfm_showphoto;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Types;

type
  { TfrmShowPhoto }
  TfrmShowPhoto = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private

  public

  end;

procedure ShowPhoto(const aFilename: string;
                    const aStream: TStream;
                    aWidth,aHeight: ptrint);

implementation
var frmShowPhoto: TfrmShowPhoto;

procedure ShowPhoto(const aFilename: string;
                    const aStream: TStream;
                    aWidth,aHeight: ptrint);
begin
  Application.CreateForm(TfrmShowPhoto, frmShowPhoto);
  try
    frmShowPhoto.Width:= aWidth+32; frmShowPhoto.Height:= aHeight;
    frmShowPhoto.Caption:= 'Showing ['+aFilename+'] ~ '+aWidth.ToString+' x '+aHeight.ToString;
    frmShowPhoto.Image1.Picture.LoadFromStream(aStream);
    frmShowPhoto.ShowModal;
  finally
    frmShowPhoto.Close;
    FreeAndNil(frmShowPhoto);
  end;
end;

{$R *.lfm}

{ TfrmShowPhoto }

procedure TfrmShowPhoto.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //todo: zoom
end;

end.

