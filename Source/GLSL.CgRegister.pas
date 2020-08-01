//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLSL.CgRegister;

(*  Registration unit for Cg shader package *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  DesignIntf,
  DesignEditors,
  VCLEditors,

  GLMaterial,

  Import.Cg,
  Import.CgGL,
  GLS.SceneRegister,  //?
  GLSL.CgShader,
  GLSL.CgBombShader;

procedure Register;

//-----------------------------------------------------
implementation
//-----------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLScene Shaders', [TCgShader, TCgBombShader]);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TCgBombShader, '', TGLLibMaterialNameProperty);
end;

end.
