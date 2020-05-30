{:
  GLSL Bump Shader Demo

  A demo that shows how to use the TGLSLBumpShader component.

  Note: All unsupported scene objects have only one thing in common -
        they cannot call the BuildTangentSpace() function.

  Version history:
    24/07/09 - DaStr - Added the "Show unsupported scene objects" checkbox
    12/07/07 - DaStr - Bugfixed MultiLight stuff. Other small bigfixes...
    03/04/07 - DaStr - Added more objects
    30/03/07 - DaStr - Initial version
}
program BumpShader;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {GLSLTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLSLTestForm, GLSLTestForm);
  Application.Run;
end.
