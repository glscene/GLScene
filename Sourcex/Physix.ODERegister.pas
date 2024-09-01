//
// The graphics engine GXScene https://github.com/glscene
//
unit Physix.ODERegister;

(* Design time registration code for the ODE Manager *)

interface

uses
  System.Classes,
  Physix.ODEManager;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterClasses([TgxODEManager, TgxODEJointList, TgxODEJoints, TgxODEElements]);
  RegisterComponents('GXScene',[TgxODEManager,TgxODEJointList]);
end;

end.
