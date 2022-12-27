//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit Physics.Register;

(* DesignTime registration code for the Physics Managers *)

interface

uses
  System.Classes,
  Physics.ODEManager,
  Physics.SPIManager,
  NGD.Manager;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterClasses([TGLODEManager, TGLODEJointList, TGLODEJoints, TGLODEElements, TGLSPIManager,
    TGLNGDManager, TGLNGDDynamic, TGLNGDStatic]);
  RegisterComponents('GLScene Physics Managers', [TGLODEManager, TGLODEJointList, TGLSPIManager,
    TGLNGDManager]);
end;

end.
