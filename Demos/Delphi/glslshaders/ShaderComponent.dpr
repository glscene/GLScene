{:
  GLSL Shader Component Demo

  A demo that shows how to use the TGLSLShader component.

  Version history:
    30/03/07 - DaStr - Cleaned up "uses" section
    20/03/07 - DaStr - Initial version


}
program ShaderComponent;



uses
  Forms,
  ShaderComponentFm in 'ShaderComponentFm.pas' {GLSLTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLSLTestForm, GLSLTestForm);
  Application.Run;
end.
