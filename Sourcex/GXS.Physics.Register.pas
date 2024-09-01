//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Physics.Register;

(* Design time registration code for the ODE Manager *)

interface

uses
  System.Classes,
///  GXS.PhysManager todo
  GXS.ODEManager,
  GXS.NGDManager;

procedure Register;

implementation // ---------------------------------------------------------

procedure Register;
begin
  RegisterClasses([TgxODEManager, TgxODEJointList, TgxODEJoints, TgxODEElements,
    TgxNGDManager, TgxNGDDynamic, TgxNGDStatic {, TgxPhysManager}]);
  RegisterComponents('GXScene Physics Managers',[TgxNGDManager, TgxODEManager,
                   TgxODEJointList {, TgxPhysManager}]);
end;

end.
