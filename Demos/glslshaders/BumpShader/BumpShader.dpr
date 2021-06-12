(*
  GLSL Bump Shader Demo

  A demo that shows how to use the TGLSLBumpShader component.

  Note: All unsupported scene objects have only one thing in common -
        they cannot call the BuildTangentSpace() function.
*)
program BumpShader;

uses
  Forms,
  BumpShaderFm in 'BumpShaderFm.pas' {FormBumpShader};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBumpShader, FormBumpShader);
  Application.Run;
end.
