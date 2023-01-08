(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TstNCpyu                                                         *)
(* Single producer/many consumer test suite (main form)             *)
(********************************************************************)
                        
unit TstNCpyu;

{$I TDDefine.inc}

{$IFNDEF Delphi2Plus}
Error! This test unit is for Win32 Delphi compilers only
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
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
  MaxConsumers = 32;
  MaxBuffers = 32;
  BufferSize = 1024;

type
  PStreamArray = ^TStreamArray;
  TStreamArray = array [0..pred(MaxConsumers)] of TStream;

  PBuffer = ^TBuffer;
  TBuffer = packed record
    bCount : longint;
    bBlock : array [0..pred(BufferSize)] of byte;
  end;

  PBufferArray = ^TBufferArray;
  TBufferArray = array [0..pred(MaxBuffers)] of PBuffer;

  TQueuedBuffers = class
    private
      FBufCount      : integer;
      FBuffers       : PBufferArray;
      FConsumerCount : integer;
      FHead          : array [0..pred(MaxConsumers)] of integer;
      FTail          : integer;
    protected
      function qbGetHead(aInx : integer) : PBuffer;
      function qbGetTail : PBuffer;
    public
      constructor Create(aBufferCount   : integer;
                         aConsumerCount : integer);
      destructor Destroy; override;

      procedure AdvanceHead(aConsumerId : integer);
      procedure AdvanceTail;

      property Head[aInx : integer] : PBuffer read qbGetHead;
      property Tail : PBuffer read qbGetTail;

      property ConsumerCount : integer read FConsumerCount;
  end;

type
  TProducer = class(TThread)
    private
      FBuffers : TQueuedBuffers;
      FStream  : TStream;
      FSyncObj : TtdProduceManyConsumeSync;
    protected
      procedure Execute; override;
    public
      constructor Create(aStream  : TStream;
                         aSyncObj : TtdProduceManyConsumeSync;
                         aBuffers : TQueuedBuffers);
      destructor Destroy; override;
  end;

type
  TConsumer = class(TThread)
    private
      FBuffers : TQueuedBuffers;
      FID      : integer;
      FStream  : TStream;
      FSyncObj : TtdProduceManyConsumeSync;
    protected
      procedure Execute; override;
    public
      constructor Create(aStream  : TStream;
                         aSyncObj : TtdProduceManyConsumeSync;
                         aBuffers : TQueuedBuffers;
                         aID      : integer);
      destructor Destroy; override;
  end;

{===TQueuedBuffers===================================================}
constructor TQueuedBuffers.Create(aBufferCount   : integer;
                                  aConsumerCount : integer);
var
  i : integer;
begin
  inherited Create;
  {allocate the buffers}
  FBuffers := AllocMem(aBufferCount * sizeof(pointer));
  for i := 0 to pred(aBufferCount) do
    GetMem(FBuffers^[i], sizeof(TBuffer));
  FBufCount := aBufferCount;
  FConsumerCount := aConsumerCount;
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
procedure TQueuedBuffers.AdvanceHead(aConsumerId : integer);
begin
  inc(FHead[aConsumerId]);
  if (FHead[aConsumerId] = FBufCount) then
    FHead[aConsumerId] := 0;
end;
{--------}
procedure TQueuedBuffers.AdvanceTail;
begin
  inc(FTail);
  if (FTail = FBufCount) then
    FTail := 0;
end;
{--------}
function TQueuedBuffers.qbGetHead(aInx : integer) : PBuffer;
begin
  Result := FBuffers^[FHead[aInx]];
end;
{--------}
function TQueuedBuffers.qbGetTail : PBuffer;
begin
  Result := FBuffers^[FTail];
end;
{====================================================================}


{===TProducer========================================================}
constructor TProducer.Create(aStream  : TStream;
                             aSyncObj : TtdProduceManyConsumeSync;
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
    {signal that we're about to start producing}
    FSyncObj.StartProducing;
    {read a block from the stream into the tail buffer}
    Tail := FBuffers.Tail;
    Tail^.bCount := FStream.Read(Tail^.bBlock, 1024);
    {advance the hail pointer}
    FBuffers.AdvanceTail;
    {signal that we've stopped producing}
    FSyncObj.StopProducing;
  until (Tail^.bCount = 0);
end;
{====================================================================}


{===TConsumer========================================================}
constructor TConsumer.Create(aStream  : TStream;
                             aSyncObj : TtdProduceManyConsumeSync;
                             aBuffers : TQueuedBuffers;
                             aID      : integer);
begin
  inherited Create(true);
  FStream := aStream;
  FSyncObj := aSyncObj;
  FBuffers := aBuffers;
  FID := aID;
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
  FSyncObj.StartConsuming(FID);
  {get our head buffer}
  Head := FBuffers.Head[FID];
  {while the head buffer is not empty...}
  while (Head^.bCount <> 0) do begin
    {write a block from the head buffer into the stream}
    FStream.Write(Head^.bBlock, Head^.bCount);
    {advance our head pointer}
    FBuffers.AdvanceHead(FID);
    {we've now finished with this buffer}
    FSyncObj.StopConsuming(FID);
    {signal that we want to start consuming again}
    FSyncObj.StartConsuming(FID);
    {get our head buffer}
    Head := FBuffers.Head[FID];
  end;
  {we've now finished with the final buffer}
  FSyncObj.StopConsuming(FID);
end;
{====================================================================}


{===Interfaced routine===============================================}
procedure ThreadedMultiCopyStream(aSrcStream   : TStream;
                                  aDestCount   : integer;
                                  aDestStreams : PStreamArray);
var
  i : integer;
  SyncObj   : TtdProduceManyConsumeSync;
  Buffers   : TQueuedBuffers;
  Producer  : TProducer;
  Consumer  : array [0..pred(MaxConsumers)] of TConsumer;
  WaitArray : array [0..MaxConsumers] of THandle;
begin
  SyncObj := nil;
  Buffers := nil;
  Producer := nil;
  for i := 0 to pred(MaxConsumers) do
    Consumer[i] := nil;
  for i := 0 to MaxConsumers do
    WaitArray[i] := 0;
  try
    {create the synchronization object}
    SyncObj := TtdProduceManyConsumeSync.Create(20, aDestCount);
    {create the queued buffer object}
    Buffers := TQueuedBuffers.Create(20, aDestCount);
    {create the producer thread, save its handle}
    Producer := TProducer.Create(aSrcStream, SyncObj, Buffers);
    WaitArray[0] := Producer.Handle;
    {create the consumer threads, save their handles}
    for i := 0 to pred(aDestCount) do begin
      Consumer[i] := TConsumer.Create(aDestStreams^[i], SyncObj, Buffers, i);
      WaitArray[i+1] := Consumer[i].Handle;
    end;
    {start the threads up}
    for i := 0 to pred(aDestCount) do
      Consumer[i].Resume;
    Producer.Resume;
    {wait for the threads to finish}
    WaitForMultipleObjects(1+aDestCount, @WaitArray, true, INFINITE);
  finally
    Producer.Free;
    for i := 0 to pred(aDestCount) do
      Consumer[i].Free;
    Buffers.Free;
    SyncObj.Free;
  end;
end;
{====================================================================}




procedure TForm1.Button1Click(Sender: TObject);
var
  SrcFile : TFileStream;
  DestFile : TStreamArray;
begin
  SrcFile := nil;
  DestFile[0] := nil;
  DestFile[1] := nil;
  DestFile[2] := nil;
  try
    Button1.Enabled := false;
    SrcFile := TFileStream.Create(edSourceFile.Text, fmOpenRead + fmShareDenyNone);
    DestFile[0] := TFileStream.Create(edDestFile.Text, fmCreate);
    DestFile[1] := TFileStream.Create(edDestFile.Text + '1', fmCreate);
    DestFile[2] := TFileStream.Create(edDestFile.Text + '2', fmCreate);
    ThreadedMultiCopyStream(SrcFile, 3, @DestFile);
  finally
    SrcFile.Free;
    DestFile[0].Free;
    DestFile[1].Free;
    DestFile[2].Free;
    Button1.Enabled := true;
  end;
end;

end.
