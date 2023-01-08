
{*******************************************************************************
*        Unit name : bc_imghelp.pas                                            *
*        Copyright : (C)cdbc.dk 2022                                           *
*        Programmer: Benny Christensen                                         *
*        Created   : 03.12.2022 /bc image manipulation, single & multi thread  *
*        Updated   : 04.12.2022 /bc refactoring & cleanup                      *
*                                                                              *
********************************************************************************
*        Purpose:                                                              *
*        Helper functions & structures to aid in manipulating                  *
*        and working with Images, Pictures and Photos                          *
*                                                                              *
********************************************************************************
*        License:                                                              *
*        "Beer License" - If you meet me one day, you'll buy me a beer :-)     *
*        I'm NOT liable for anything! Use at your own risk!!!                  *
*******************************************************************************}

unit bc_imghelp;
{$mode objfpc}{$H+}
{.$define debug}
interface
uses
  Classes, SysUtils, Graphics, BGRABitmapTypes, bc_imgqueue, bc_msgqueue, LCLIntf, bc_event,
  bc_semaphore;
const
  bcJpg = TBGRAImageFormat.ifJpeg; { alias }
  bcPng = TBGRAImageFormat.ifPng; { alias for use in form that doesn't use BGRABitmapTypes }
type
  { alias for use in form that doesn't use BGRABitmapTypes }
  TbcImageFormat = BGRABitmapTypes.TBGRAImageFormat;
  { how the image is oriented }
  TPictureOrientation = (poLandscape,poPortrait); { = || }
  TTestFunc = function(aParam: pointer): ptrint;
  { communication record for threading }
  PThrdRec = ^TThrdRec;
  TThrdRec = record
    trFormat: TbcImageFormat;   { desired type of image, ifJpeg, ifPng etc... }
    trHandle: ptrint;           { handle of the calling form }
    trHeight: longint;          { desired height of image keeping aspect ratio }
    trId: ptrint;               { thread id }
    trWidth: longint;           { desired width of image keeping aspect ratio }
    trEvent: IbcEvent;          { event the mainthread waits for, to be signaled }
  end;

{ bcCalcWideRect calculates a rect to convert 4:3 to 16:9, fit for widescreen }
function bcCalcWideRect(aSrc,aDest: TRect): TRect;
{ bcCalculatePictScale calculates the values used in picture resizing/resampling }
function bcCalculatePictScale(aSource,aTarget: TPoint;out aResult: TPoint): TPictureOrientation;
{ LoadImage900x675 picks a filename from nameq and returns
  a png of 900x675 and a png of 200x150 in pictureq }
function LoadImage900x675(aParam: pointer): ptrint;

{ LoadImages loads multiple images rescales and creates thumbnails, then queues them }
procedure LoadImages(aHandle: ptrint;aWidth,aHeight: longint;aFormat: string);

var
  Test: TTestFunc;
  gH: ptrint;
  PictureQ: TbcImgQueue;
  NameQ: TbcMessageQueue;
  List: TbcImgList;
  qEvent: TbcEvent;
  qFirst: boolean;
  Sem: TbcSemaphore;
  ds,dauerload,dauer,ss,sl,sb,sr,sr2,sq,se: PtrUInt;

implementation
uses bc_advstring, BGRABitmap, BGRAResample, bc_debug_simple, FPWriteJPEG, FPWritePNG;

function bcCalcWideRect(aSrc,aDest: TRect): TRect;
var
  Scl: double;
  Half: integer;
begin
  Scl:= aDest.Right / aSrc.Right;
  Half:= round((aSrc.Bottom - (aDest.Bottom / Scl)) / 2);
  with Result do begin
    Left:= aSrc.Left;
    Top:= aSrc.Top + Half;
    Right:= aSrc.Right;
    Bottom:= aSrc.Bottom - Half;
  end;
end; { bcCalcWideRect }

function bcCalculatePictScale(aSource,aTarget: TPoint;out aResult: TPoint): TPictureOrientation;
var
  tmpW,tmpH: double;
begin
  { first figure out our orientation }
  if aSource.X > aSource.Y then Result:= poLandscape
  else Result:= poPortrait;
  { then try to find scale to reach target }
  tmpW:= aTarget.X / aSource.X; tmpH:= aTarget.Y / aSource.Y;
  if tmpW > tmpH then tmpW:= tmpH; { choose the smallest scale }
  { last set the values according to orientation, will fit target }
  aResult.X:= round(aSource.X * tmpW);
  aResult.Y:= round(aSource.Y * tmpW);
end; { bcCalculatePictScale }

{ beware, thread function! }
function privateLoadImage(aParam: pointer): ptrint;
var
  Img,Thm: TBGRABitmap;
  Strm: TMemoryStream;
  S: string;
  Msg: TbcMessage;
  Rec: PImgRec;
  Param: TThrdRec;
  Res: TPoint;
begin
  Param:= PThrdRec(aParam)^;
  if not NameQ.IsEmpty then Msg:= NameQ.DeQueue else exit(-1);
  S:= Msg.SParam;
  Msg.Free;
  Strm:= TMemoryStream.Create;
  try
    Strm.LoadFromFile(S);
    Strm.Position:= 0;
    Img:= TBGRABitmap.Create(Strm);
  finally Strm.Free; end;
  bcCalculatePictScale(Point(Img.Width,Img.Height),Point(200,150),Res); { create thumbnail }
  Thm:= FineResample(Img,Res.X,Res.Y,rfLanczos4) as TBGRABitmap; { rfLanczos4 or rfBestQuality }
  bcCalculatePictScale(Point(Img.Width,Img.Height),Point(Param.trWidth,Param.trHeight),Res);
  BGRAReplace(Img,Img.Resample(Res.X,Res.Y,rmFineResample)); { nice and conveinient }
  Rec:= bcCreateImgRec(S,bcExtractFileExt(S));
  Img.SaveToStreamAs(Rec^.irImage,Param.trFormat);
  Rec^.irImage.Position:= 0;
  Thm.SaveToStreamAs(Rec^.irThumbnail,Param.trFormat);
  Rec^.irThumbnail.Position:= 0;
  Rec^.irWidth:= Img.Width; Rec^.irHeight:= Img.Height;
  PictureQ.EnQueue(Rec);
  Sem.Wait;
  if qFirst then begin
    qFirst:= false;
//    qEvent.Signal;
    Param.trEvent.Signal;
  end;
  Sem.Signal;
  Thm.Free; Img.Free; Result:= 0;
  PostMessage(Param.trHandle,LM_DONE,HR_OK,Param.trId); { notify caller form, that we're done }
  {$ifdef debug} DebugLn(Res.X,Res.Y,S); {$endif debug}
end; { privateLoadImages }

{ beware, thread function! }
function LoadImage900x675(aParam: pointer): ptrint;
var
  Img,Thm: TBGRABitmap;
  Strm: TMemoryStream;
  S: string;
  Msg: TbcMessage;
  Rec: PImgRec;
  Param: TThrdRec;
  Dir: TPictureOrientation;
  Target,Res: TPoint;
begin
  Param:= PThrdRec(aParam)^;
  if not NameQ.IsEmpty then Msg:= NameQ.DeQueue else exit(-1);
  S:= Msg.SParam;
  Msg.Free;

  Strm:= TMemoryStream.Create;
ss:= GetTickCount64; //stream start
  try
    Strm.LoadFromFile(S);
    Strm.Position:= 0;
sl:= GetTickCount64 - ss;
    Img:= TBGRABitmap.Create(Strm);
sb:= (GetTickCount64 - ss) - sl;
  finally Strm.Free; end;
  Dir:= bcCalculatePictScale(Point(Img.Width,Img.Height),Point(200,150),Res);
  Thm:= FineResample(Img,Res.X,Res.Y,rfLanczos4) as TBGRABitmap; //<-- ok  rfLanczos4 rfBestQuality
//  Thm:= FineResample(Img,200,150,rfBestQuality) as TBGRABitmap; //<-- ok  rfLanczos4
sr:= (GetTickCount64 - ss) - sl - sb;
//  Dir:= bcCalculatePictScale(Point(Img.Width,Img.Height),Point(900,675),Res);
//-->
  Dir:= bcCalculatePictScale(Point(Img.Width,Img.Height),Point(Param.trWidth,Param.trHeight),Res);
  BGRAReplace(Img,Img.Resample(Res.X,Res.Y,rmFineResample)); //<-- ok
//<--
//  BGRAReplace(Img,Img.Resample(900,675,rmFineResample)); //<-- ok
sr2:= (GetTickCount64 - ss) - sl - sb - sr;
  Rec:= bcCreateImgRec(S,bcExtractFileExt(S));
//  Img.SaveToStreamAsPng(Rec^.irImage);
  Img.SaveToStreamAs(Rec^.irImage,Param.trFormat);
  Rec^.irImage.Position:= 0;
//  Thm.SaveToStreamAs(Rec^.irThumbnail,ifPng);
  Thm.SaveToStreamAs(Rec^.irThumbnail,Param.trFormat);
  Rec^.irThumbnail.Position:= 0;
  Rec^.irWidth:= Img.Width; Rec^.irHeight:= Img.Height;
  PictureQ.EnQueue(Rec);
sq:= (GetTickCount64 - ss) - sl - sb - sr - sr2;
//  Result.X:= Img.Width; Result.Y:= Img.Height;
  Thm.Free; Img.Free;
se:= (GetTickCount64 - ss);
  PostMessage(Param.trHandle,LM_DONE,HR_OK,Param.trId); { notify caller form, that we're done }
  {$ifdef debug} DebugLn(Res.X,Res.Y,S); {$endif debug}
end; { LoadImage900x675 }

{ beware, thread function! }
function ThreadFunc(aPtr: pointer): ptrint;
var
  S,S1,S2,W: string;
  dP: pchar; { remember to (d)ispose of (P)char }
  wide: TRect;
  img,tmp: TBGRABitmap;
  F: TFileStream;
  J: TFPWriterJPEG;
begin
  dp:= strnew(pchar(aPtr)); // make a copy fast
  S:= string(dP);
  S1:= '/home/bc/Pictures/wide/';  // ExtractFilePath(S);
  S2:= ExtractFileName(S);
  W:= format('%swide75_%s',[S1,S2]);
  img:= TBGRABitmap.Create(S);                // '/home/bc/test.jpg'

  wide:= bcCalcWideRect(rect(0,0,img.Width,img.Height),rect(0,0,1366,768));    // rect(0,376,img.Width,(img.Height-376));
  tmp:= img.GetPart(wide);                                // tmp.SaveToFile(W);

  J:= TFPWriterJPEG.Create; // J.CompressionQuality:= 100;
  F:= TFileStream.Create(W,fmCreate or fmOpenWrite);
  tmp.SaveToStream(F,J);                // TFPWriterJPEG
  F.Free;
  J.Free;
  tmp.Free;
  img.Free;
  StrDispose(dP); { remember to (d)ispose of (P)char }
  Result:= 5000;
//  sleep(Result);
  PostMessage(gH,LM_DONE,HR_OK,0); { notify caller form, that we're done }
end;

procedure LoadImages(aHandle: ptrint;aWidth,aHeight: longint;aFormat: string);
var
  Rec: TThrdRec;
begin
  Rec.trHandle:= aHandle;
  Rec.trWidth:= aWidth;
  Rec.trHeight:= aHeight;
  Rec.trFormat:= TBGRAImageFormat(StrToMsg(aFormat));
  Rec.trEvent:= qEvent;
  Rec.trId:= 0;
  while not NameQ.IsEmpty do begin
    beginthread(@privateLoadImage,@Rec);
//    beginthread(@LoadImage900x675,@Rec);
    sleep(2);
    Rec.trId+= 1;
  end;
  sleep(10);
end;






initialization
  NameQ:= TbcMessageQueue.Create;
  PictureQ:= TbcImgQueue.Create;
  List:= TbcImgList.Create;
  Sem:= TbcSemaphore.Create(1); // binary ~ mutex / crit sect
  Test:= @ThreadFunc;
finalization 
  FreeAndNil(NameQ);
  FreeAndNil(PictureQ);
  FreeAndNil(List);
  if Assigned(Sem) then Sem.Free;
end.

