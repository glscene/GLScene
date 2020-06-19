//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLCgRegister;

(*  Registration unit for CG shader *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  DesignIntf,
  DesignEditors,
  VCLEditors,
  GLMaterial,

  GLS.SceneRegister,
  Cg,
  CgGL,
  GLCgShader,
  GLCgBombShader;

procedure Register;

//-----------------------------------------------------
implementation
//-----------------------------------------------------

procedure Register;
begin
  // Register components.
  RegisterComponents('GLScene Shaders', [TCgShader, TCgBombShader]);

  // Register property editors.
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TCgBombShader, '', TGLLibMaterialNameProperty);
end;

end.
