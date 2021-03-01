{%MainUnit bc_datetime.pas}
{ implementation: (c)cdbc 01.03.2021 /bc }

{ *** TIsoDateTime *** }
function TIsoDateTime.get_Date: IIsoDate;
begin
  Result:= fDate;
end;

function TIsoDateTime.get_Time: IIsoTime;
begin
  Result:= fTime;
end;

function TIsoDateTime.get_AsString: string;
begin
  Result:= fDate.AsString+' - '+fTime.AsString;
end;

constructor TIsoDateTime.Create;
begin
  inherited Create;
  fDate:= TIsoDate.Create(now);
  fTime:= TIsoTime.Create(now);
end;

constructor TIsoDateTime.Create(const aDateTime: TDateTime);
begin
  inherited Create;
  fDate:= TIsoDate.Create(aDateTime);
  fTime:= TIsoTime.Create(aDateTime);
end;

destructor TIsoDateTime.Destroy;
begin
  fDate:= nil;
  fTime:= nil;
  inherited Destroy;
end;

procedure TIsoDateTime.SetDateTime(const aDateTime: TDateTime);
begin
  fDate.AsDate:= aDateTime;
  fTime.AsTime:= aDateTime;
end;

{ included by bc_datetime.pas }
