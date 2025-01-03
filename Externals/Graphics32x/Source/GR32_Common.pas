unit GR32_Common;

interface

function InterlockedIncrement(var Addend: Integer): Integer; stdcall; inline;
function InterlockedDecrement(var Addend: Integer): Integer; stdcall; inline;

implementation

function InterlockedIncrement(var Addend: Integer): Integer; stdcall; inline;
begin
  Result := AtomicIncrement(Addend);
end;

function InterlockedDecrement;
begin
  Result := AtomicDecrement(Addend);
end;

end.
