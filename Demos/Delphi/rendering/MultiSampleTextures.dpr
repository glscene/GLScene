(*
   GLScene Multisample texture demo
*)
program MultiSampleTextures;

uses
  Forms,
  MultiSampleTexturesFm in 'MultiSampleTexturesFm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMultiSampleTextures, FormMultiSampleTextures);
  Application.Run;
end.
