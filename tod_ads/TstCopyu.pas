(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstCopyu                                                         *)
(* Single producer/single consumer test suite (main form)           *)
(********************************************************************)
                             
unit TstCopyu;

{$I TDDefine.inc}

{$IFNDEF Delphi2Plus}
Error! This test unit is for Win32 Delphi compilers only
{$ENDIF}
                                               
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, 
  Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    edSourceFile: TEdit;
    edDestFile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  TDPCSync;

{$R *.DFM}

const
  BufferSize = 1024;

type
  PBuffer = ^TBuffer;
  TBuffer = packed record
    bCount : longint;
    bBlock : array [0..pred(BufferSize)] of byte;
  end;

  PBufferArray = ^TBufferArray;
  TBufferArray = array [0..1023] of PBuffer;

type
  TQueuedBuffers = class
    private
      FBufCount   : integer;
      FBuffers    : PBufferArray;
      FHead       : integer;
      FTail       : integer;
    protected
      function qbGetHead : PBuffer;
      function qbGetTail : PBuffer;
    public
      constructor Create(aBufferCount : integer);
      destructor Destroy; override;

      procedure AdvanceHead;
      procedure AdvanceTail;

      property Head : PBuffer read qbGetHead;
      property Tail : PBuffer read qbGetTail;
  end;

type
  TProducer = class(TThread)
    private
      FBuffers : TQueuedBuffers;
      FStream  : TStream;
      FSyncObj : TtdProduceConsumeSync;
    protected
      procedure Execute; override;
    public
      constructor Create(aStream  : TStream;
                         aSyncObj : TtdProduceConsumeSync;
                         aBuffers : TQueuedBuffers);
      destructor Destroy; override;
  end;

type
  TConsumer = class(TThread)
    private
      FBuffers : TQueuedBuffers;
      FStream  : TStream;
      FSyncObj : TtdProduceConsumeSync;
    protected
      procedure Execute; override;
    public
      constructor Create(aStream  : TStream;
                         aSyncObj : TtdProduceConsumeSync;
                         aBuffers : TQueuedBuffers);
      destructor Destroy; override;
  end;

{===TQueuedBuffers===================================================}
constructor TQueuedBuffers.Create(aBufferCount : integer);
var
  i : integer;
begin
  inherited Create;
  {allocate the buffers}
  FBuffers := AllocMem(aBufferCount * sizeof(pointer));
  for i := 0 to pred(aBufferCount) do
    GetMem(FBuffers^[i], sizeof(TBuffer));
  FBufCount := aBufferCount;
end;
{--------}
destructor TQueuedBuffers.Destroy;
var
  i : integer;
begin
  {free the buffers}
  if (FBuffers <> nil) then begin
    for i := 0 to pred(FBufCount) do
      if (FBuffers^[i] <> nil) then
        FreeMem(FBuffers^[i], sizeof(TBuffer));
    FreeMem(FBuffers, FBufCount * sizeof(pointer));
  end;
  inherited Destroy;
end;
{--------}
procedure TQueuedBuffers.AdvanceHead;
begin
  inc(FHead);
  if (FHead = FBufCount) then
    FHead := 0;
end;
{--------}
procedure TQueuedBuffers.AdvanceTail;
begin
  inc(FTail);
  if (FTail = FBufCount) then
    FTail := 0;
end;
{--------}
function TQueuedBuffers.qbGetHead : PBuffer;
begin
  Result := FBuffers^[FHead];
end;
{--------}
function TQueuedBuffers.qbGetTail : PBuffer;
begin
  Result := FBuffers^[FTail];
end;
{====================================================================}


{===TProducer========================================================}
constructor TProducer.Create(aStream  : TStream;
                             aSyncObj : TtdProduceConsumeSync;
                             aBuffers : TQueuedBuffers);
begin
  inherited Create(true);
  FStream := aStream;
  FSyncObj := aSyncObj;
  FBuffers := aBuffers;
end;
{--------}
destructor TProducer.Destroy;
begin
  inherited Destroy;
end;
{--------}
procedure TProducer.Execute;
var
  Tail : PBuffer;
begin
  {do until the stream is exhausted...}
  repeat
    {signal that we want to start producing}
    FSyncObj.StartProducing;
    {read a block from the stream into the tail buffer}
    Tail := FBuffers.Tail;
    Tail^.bCount := FStream.Read(Tail^.bBlock, BufferSize);
    {advance the tail pointer}
    FBuffers.AdvanceTail;
    {as we've now written a new buffer, signal that we've produced}
    FSyncObj.StopProducing;
  until (Tail^.bCount = 0);
end;
{====================================================================}


{===TConsumer========================================================}
constructor TConsumer.Create(aStream  : TStream;
                             aSyncObj : TtdProduceConsumeSync;
                             aBuffers : TQueuedBuffers);
begin
  inherited Create(true);
  FStream := aStream;
  FSyncObj := aSyncObj;
  FBuffers := aBuffers;
end;
{--------}
destructor TConsumer.Destroy;
begin
  inherited Destroy;
end;
{--------}
procedure TConsumer.Execute;
var
  Head : PBuffer;
begin
  {signal that we want to start consuming}
  FSyncObj.StartConsuming;
  {get the head buffer}
  Head := FBuffers.Head;
  {while the head buffer is not empty...}
  while (Head^.bCount <> 0) do begin
    {write a block from the head buffer into the stream}
    FStream.Write(Head^.bBlock, Head^.bCount);
    {advance the head pointer}
    FBuffers.AdvanceHead;
    {as we've read and processed a buffer, signal that we've consumed}
    FSyncObj.StopConsuming;
    {signal that we want to start consuming again}
    FSyncObj.StartConsuming;
    {get the head buffer}
    Head := FBuffers.Head;
  end;
end;
{====================================================================}


procedure ThreadedCopyStream(aSrcStream, aDestStream : TStream);
var
  SyncObj  : TtdProduceConsumeSync;
  Buffers  : TQueuedBuffers;
  Producer : TProducer;
  Consumer : TConsumer;
  WaitArray : array [0..1] of THandle;
begin
  SyncObj := nil;
  Buffers := nil;
  Producer := nil;
  Consumer := nil;
  try
    {create the synchronization object, the queued buffer object (with
     20 buffers) and the two threads}
    SyncObj := TtdProduceConsumeSync.Create(20);
    Buffers := TQueuedBuffers.Create(20);
    Producer := TProducer.Create(aSrcStream, SyncObj, Buffers);
    Consumer := TConsumer.Create(aDestStream, SyncObj, Buffers);
    {save the thread handles so we can wait on them}
    WaitArray[0] := Producer.Handle;
    WaitArray[1] := Consumer.Handle;
    {start the threads up}
    Consumer.Resume;
    Producer.Resume;
    {wait for the threads to finish}
    WaitForMultipleObjects(2, @WaitArray, true, INFINITE);
  finally
    Producer.Free;
    Consumer.Free;
    Buffers.Free;
    SyncObj.Free;
  end;
end;




procedure TForm1.Button1Click(Sender: TObject);
var
  File1 : TFileStream;
  File2 : TFileStream;
begin
  File1 := nil;
  File2 := nil;
  try
    Button1.Enabled := false;
    File1 := TFileStream.Create(edSourceFile.Text, fmOpenRead + fmShareDenyNone);
    File2 := TFileStream.Create(edDestFile.Text, fmCreate);
    ThreadedCopyStream(File1, File2);
  finally
    File1.Free;
    File2.Free;
    Button1.Enabled := true;
  end;
end;

end.
