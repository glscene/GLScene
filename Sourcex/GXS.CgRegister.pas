//
// The graphics engine GLXEngine. The unit of GXScene for Delphi
//
unit GXS.CgRegister;

(*  Registration unit for Cg Shader package *)

interface

uses
  System.Classes,
// ToDo
///  DesignIntf,       not implemented
///  DesignEditors,
///  FMXEditors,

  GXS.Material,

  Cg.Import,
  Cg.GL,
  GXS.SceneRegister,  // for TgxLibMaterialNameProperty
  GXS.CgShader,
  GXS.CgBombShader;

procedure Register;

implementation //-------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene Shaders', [TCGxShader, TCGxBombShader]);
  RegisterPropertyEditor(TypeInfo(TgxLibMaterialName), TCGxBombShader, '',
             TgxLibMaterialNameProperty);
end;

end.
