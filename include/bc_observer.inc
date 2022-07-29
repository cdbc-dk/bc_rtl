{%MainUnit bc_types.pas}
{ implementation: (c)cdbc 26.02.2021 /bc }

{ *** TbcObserver *** }
constructor TbcObserver.Create(const anOwner: TObject);
begin
  inherited Create;
  fOwner:= anOwner; { link }
end;

destructor TbcObserver.Destroy;
begin
  fOwner:= nil; { just unlink }
  inherited Destroy;
end;

procedure TbcObserver.bcSubjectChanged(aSender: TObject;
                                       anOperation: TbcSubjectOperation;
                                       aData: pointer);
begin
  case anOperation of
    ooAddItem: begin
                 // to be overridden
               end;
    ooChange:  begin
                 // to be overridden
               end;
    ooDeleteItem: begin
                    // to be overridden
                  end;
    ooFree:       begin
                    // to be overridden
                  end;
    ooCustom:     begin
                    // to be overridden
                  end;
  end; { case }
end;

{ *** TbcSubject *** }
constructor TbcSubject.Create(anOwner: TObject);
begin
  inherited Create;
  fOwner:= anOwner;
end;

destructor TbcSubject.Destroy;
begin
  if assigned(fObservers) then begin
    bcNotifyObservers(Owner,ooFree,nil);
    FreeAndNil(fObservers);
  end;
  inherited Destroy;
end;

procedure TbcSubject.bcAttachObserver(anObserver: TObject);
var I: IbcObserver;
begin
  if not anObserver.GetInterface(SGUIDIbcObserver,I) then
    raise EObserver.CreateFmt(SErrNotObserver,[anObserver.ClassName]);
  if not assigned(fObservers) then fObservers:= TFPList.Create;
  fObservers.Add(I);
end;

procedure TbcSubject.bcDetachObserver(anObserver: TObject);
var I: IbcObserver;
begin
  if not anObserver.GetInterface(SGUIDIbcObserver,I) then
    raise EObserver.CreateFmt(SErrNotObserver,[anObserver.ClassName]);
  if assigned(fObservers) then begin
    fObservers.Remove(I);
    if (fObservers.Count=0) then FreeAndNil(fObservers);
  end;
end;

procedure TbcSubject.bcNotifyObservers(aSender: TObject;
                                       anOperation: TbcSubjectOperation;
                                       aData: pointer);
var
  Idx: integer;
  Obs: IbcObserver;
begin
  if assigned(fObservers) then for Idx:= fObservers.Count-1 downto 0 do begin
    Obs:= IbcObserver(fObservers[Idx]);
    Obs.bcSubjectChanged(Owner,anOperation,aData);
  end;
end;

{ included by bc_types.pas }
