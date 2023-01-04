//
// The graphics platform GLScene https://github.com/glscene
//
unit Physics.Register;

(* DesignTime registration code for the Physics Managers *)

interface

uses
  System.Classes,
  Physics.ODEManager,
  Physics.NGDManager,
  Physics.GLxManager;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterClasses([TGLODEManager, TGLODEJointList, TGLODEJoints, TGLODEElements,
    TGLNGDManager, TGLNGDDynamic, TGLNGDStatic, TGLxManager]);
  RegisterComponents('GLScene Physics Managers', [TGLODEManager, TGLODEJointList, TGLNGDManager,
  TGLxManager]);
end;

end.
