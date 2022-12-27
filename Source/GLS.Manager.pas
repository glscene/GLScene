//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.Manager;

(*
  Managers are used to manage many different kinds of clients in GLScene.
  They are registered so that when clients are loaded, the client can
  look up the manager + register themselves with it.
*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.Types;

procedure RegisterManager(aManager: TComponent);
procedure DeRegisterManager(aManager: TComponent);
function FindManager(classType: TComponentClass; const managerName: String)
  : TComponent;

// ------------------------------------------------------
implementation
// ------------------------------------------------------

var
  vManagers: TList;

procedure RegisterManager(aManager: TComponent);
begin
  if not Assigned(vManagers) then
    vManagers := TList.Create;
  if vManagers.IndexOf(aManager) < 0 then
    vManagers.Add(aManager);
end;

procedure DeRegisterManager(aManager: TComponent);
begin
  if Assigned(vManagers) then
    vManagers.Remove(aManager);
end;

function FindManager(classType: TComponentClass; const managerName: String)
  : TComponent;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(vManagers) then
    for i := 0 to vManagers.Count - 1 do
      with TComponent(vManagers[i]) do
        if InheritsFrom(classType) and (Name = managerName) then
        begin
          Result := TComponent(vManagers[i]);
          Break;
        end;
end;

// ============================================================
initialization
// ============================================================

finalization

vManagers.Free;
vManagers := nil;

end.
