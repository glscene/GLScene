(*
  Fountain Particles
    2010-05-17 - modified for fired keyboards by Pavel Vassiliev
    2005-06-09 - original code using four textures by Dave Gravel
*)

 program FountainParticlesD;

uses
  Forms,
  fFountainD in 'fFountainD.pas' {Form1},
  uFountainD in 'uFountainD.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
