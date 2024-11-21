//
// The graphics engine GLScene
//
unit GLS.Physics.Register;

(* DesignTime registration code for the Physics Managers *)

interface

uses
  System.Classes,
  GLS.ODEManager,
  GLS.NGDManager,
  GLS.PhysManager;

procedure Register;

implementation // -----------------------------------------------------------

procedure Register;
begin
  RegisterClasses([TGLODEManager, TGLODEJointList, TGLODEJoints, TGLODEElements,
    TGLNGDManager, TGLNGDDynamic, TGLNGDStatic, TGLPhysManager]);
  RegisterComponents('GLScene Physics Managers', [TGLODEManager, TGLODEJointList,
                    TGLNGDManager, TGLPhysManager]);
end;

end.
