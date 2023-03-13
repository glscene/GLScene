//
// The graphics platform GLScene https://github.com/glscene
//
unit Cg.Register;

(*  Registration unit for Cg shader package *)

interface

{$I Scenario.inc}

uses
  System.Classes,
  DesignIntf,
  DesignEditors,
  VCLEditors,

  GLS.Material,

  Cg.Import,
  Cg.GL,
  GLS.SceneRegister,  // using TGLLibMaterialNameProperty
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
