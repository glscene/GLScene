(* Scripting a Shader with DelphiWebScriptII

   A very simple example of how the GLUserShader and scripting
   components can be used to build a scripted material shader.

   The Tdws2OpenGLxUnit requires the Tdws2VectorGeometryUnit to be
   associated with the script.
*)
program DwsShader;

uses
  Vcl.Forms,
  DwsShaderFm in 'DwsShaderFm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
