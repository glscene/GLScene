//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
   Registration unit for CG shader.
}
unit GLCgRegister;

interface

{$I GLScene.inc}

uses
  System.Classes,
  DesignIntf,
  DesignEditors,
  VCLEditors,
  GLMaterial,
  GLSceneRegister,
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
