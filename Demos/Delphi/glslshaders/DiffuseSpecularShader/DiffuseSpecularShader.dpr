{:
  GLSL Diffuse Specular Shader Demo

  A demo that shows how to use the TGLSLDiffuseSpecularShader component.

  Version history:
    24/07/09 - DaStr - Added fog support
    02/07/07 - DaStr - Removed old Timer leftovers
                       (GLSimpleNavigation component now does this stuff)
    20/03/07 - DaStr - Initial version


}
program DiffuseSpecularShader;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {GLSLTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLSLTestForm, GLSLTestForm);
  Application.Run;
end.
