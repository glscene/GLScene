//
// The graphics rendering engine GLScene http://glscene.org
//
unit Cg.Register;

(*  Registration unit for Cg shader package *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  DesignIntf,
  DesignEditors,
  VCLEditors,

  GLS.Material,

  Cg.Import,
  Cg.GL,
  GLS.SceneRegister,  // TGLLibMaterialNameProperty
  Cg.Shader,
  Cg.BombShader;

procedure Register;

//-----------------------------------------------------
implementation
//-----------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLScene Shaders', [TCgShader, TCgBombShader]);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TCgBombShader, '',
             TGLLibMaterialNameProperty);
end;

end.
