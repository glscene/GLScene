//
// The graphics engine GXScene https://github.com/glscene
//
unit CGx.Register;

(*  Registration unit for CGx shader package *)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
// ToDo
///  DesignIntf,
///  DesignEditors,
///  FMXEditors,

  GXS.Material,

  CGx.Import,
  CGx.GL,
  GXS.SceneRegister,  // TgxLibMaterialNameProperty
  CGx.Shader,
  CGx.BombShader;

procedure Register;

//-----------------------------------------------------
implementation
//-----------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene Shaders', [TCGxShader, TCGxBombShader]);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TCGxBombShader, '',
             TgxLibMaterialNameProperty);
end;

end.
