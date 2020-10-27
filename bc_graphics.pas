unit bc_graphics;
interface
uses
  Sysutils,
  Graphics,
  ComCtrls,
  classes,
  LCLIntf;

const BackGroundYellow = $0080FFFF;

procedure CustomizeStatusbar(StatusBar: TStatusBar;    // the culprit itself
                             const Rect: TRect;        // rect to draw on
                             const aGlyph: TBitmap;    // optional picture, nil if none
                             const Txt1,               // highlight text
                             Txt2: string;             // normal text
                             const Color1,             // highlight color
                             Color2,                   // normal color
                             ColorBackground: TColor); // background color



implementation

procedure CustomizeStatusbar(StatusBar: TStatusBar;    // the culprit itself
                             const Rect: TRect;        // drawing boundries
                             const aGlyph: TBitmap;    // optional picture, nil if none
                             const Txt1,               // highlight text
                             Txt2: string;             // normal text
                             const Color1,             // highlight color
                             Color2,                   // normal color
                             ColorBackground: TColor); // background color
var
  TxtWidth: integer;    
begin
  Statusbar.Canvas.Brush.Color:= ColorBackground;            // set background
  Statusbar.Canvas.FillRect(Rect);                            // color...
{
  if aGlyph <> nil then begin
    // draw bitmap in front of text TODO!
  end;
}
  Statusbar.Canvas.Font.Color:= Color1;                       // set highlight color
  Statusbar.Canvas.Font.Style:= [fsBold];                     // set highlight style
  Statusbar.Canvas.Textout(Rect.Left,Rect.Top,Txt1);          // draw highlight text
  Txtwidth:= Statusbar.Canvas.Textwidth(Txt1);
  Statusbar.Canvas.Font.Color:= Color2;                       // set normal color
  Statusbar.Canvas.Font.Style:= [];                           // set normal style
  Statusbar.Canvas.Textout(Rect.Left+TxtWidth,Rect.Top,Txt2); // draw normal text
end; { CustomizeStatusbar }


end.
 
