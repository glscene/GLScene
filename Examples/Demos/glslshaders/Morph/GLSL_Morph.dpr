{*******************************************************************************
*  Morph GLSL by Dave Gravel 2005
*  lucifers23@hotmail.com
*  dave.gravel@cgocable.ca
*  http://www.dave.serveusers.com
*******************************************************************************}
program GLSL_Morph;

uses
  Forms,
  Unit1 in 'Unit1.pas' {GLSLFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLSLFrm, GLSLFrm);
  Application.Run;
end.
