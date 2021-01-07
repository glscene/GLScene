//
// The graphics rendering engine GLScene http://glscene.org
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

  GLS.Material,

  Imports.Cg,
  Imports.CgGL,
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
