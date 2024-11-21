//
// The graphics engine GLScene
//
unit GLS.CgRegister;

(*  Registration unit for Cg shader package *)

interface

{$I Stage.Defines.inc}

uses
  System.Classes,
  DesignIntf,
  DesignEditors,
  VCLEditors,

  GLS.Material,

  Cg.Import,
  Cg.GL,
  GLS.SceneRegister,  // using TGLLibMaterialNameProperty
  GLS.CgShader,
  GLS.CgBombShader;

procedure Register;

implementation //--------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLScene Shaders', [TCgShader, TCgBombShader]);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TCgBombShader, '',
             TGLLibMaterialNameProperty);
end;

end.
