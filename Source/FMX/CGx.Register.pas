//
// The graphics rendering engine GLXcene http://glscene.org
//
unit CGx.Register;

(*  Registration unit for CGx shader package *)

interface

{$I GLX.Scene.inc}

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
  RegisterComponents('GLXcene Shaders', [TCGxShader, TCGxBombShader]);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TCGxBombShader, '',
             TgxLibMaterialNameProperty);
end;

end.
