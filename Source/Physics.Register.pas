//
// The graphics rendering engine GLScene http://glscene.org
//
unit Physics.Register;

(* DesignTime registration code for the Physics Managers *)

interface

uses
  System.Classes,
  Physics.ODEManager,
  Physics.NGDManager,
  Physics.SPIManager;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterClasses([TGLODEManager, TGLODEJointList,  TGLODEJoints, TGLODEElements,
                   TGLNGDManager, TGLNGDDynamic, TGLNGDStatic,
                   TGLSPIManager]);
  RegisterComponents('GLScene Physics Managers',[TGLODEManager,TGLODEJointList,
                                TGLNGDManager, TGLSPIManager]);
end;

end.
