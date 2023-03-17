//
// The graphics platform GLArena https://github.com/glscene
//
unit CGx.Register;

(*  Registration unit for CGx shader package *)

interface

{$I Scena.inc}

uses
  System.Classes,
//  DesignIntf,
//  DesignEditors,
//  FMXEditors,

  GLX.Material,

  CGx.Import,
  CGx.GL,
  GLX.SceneRegister,  // TgxLibMaterialNameProperty
  CGx.Shader,
  CGx.BombShader;

procedure Register;

//-----------------------------------------------------
implementation
//-----------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLArena Shaders', [TCGxShader, TCGxBombShader]);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TCGxBombShader, '',
             TgxLibMaterialNameProperty);
end;

end.
