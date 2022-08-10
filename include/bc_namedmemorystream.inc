{%MainUnit bc_types.pas}
{ implementation: (c)cdbc 20.07.2022 /bc }

{ *** TbcNamedMemoryStream *** }
constructor TbcNamedMemoryStream.Create;
begin
  inherited Create;
  fName:= '';
end;

constructor TbcNamedMemoryStream.Create(const aName: string);
begin
  inherited Create;
  fName:= aName;
end;

destructor TbcNamedMemoryStream.Destroy;
begin
  inherited Destroy;
end;
{ *** TbcNamedMemoryStream *** }
{ included by bc_types.pas }    
