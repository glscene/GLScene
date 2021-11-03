{:
  GLSL Diffuse Specular Shader Demo

  A demo that shows how to use the TGLSLDiffuseSpecularShader component.

  Version history:
    24/07/09 - DaStr - Added fog support
    02/07/07 - DaStr - Removed old Timer leftovers
                       (GLSimpleNavigation component now does this stuff)
    20/03/07 - DaStr - Initial version


}
program DiffuseShaderD;

uses
  Forms,
  fDiffuseShaderD in 'fDiffuseShaderD.pas' {FormDiffuseShader};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDiffuseShader, FormDiffuseShader);
  Application.Run;
end.
