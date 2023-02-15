{*******************************************************************************
*  Morph GLSL by Dave Gravel 2005
*  lucifers23@hotmail.com
*  dave.gravel@cgocable.ca
*  http://www.dave.serveusers.com
*******************************************************************************}
program MorphD;

uses
  Forms,
  fMorphD in 'fMorphD.pas' {GLSLFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLSLFrm, GLSLFrm);
  Application.Run;
end.
