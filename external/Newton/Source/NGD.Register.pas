//
// The graphics rendering engine GLScene http://glscene.org
//
unit NGD.Register;

(* DesignTime registration code for the Physics Managers *)

interface

uses
  System.Classes,
  NGD.Manager;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterClasses([TGLNGDManager, TGLNGDDynamic, TGLNGDStatic]);
  RegisterComponents('GLScene Physics Managers',[TGLNGDManager]);
end;

end.
