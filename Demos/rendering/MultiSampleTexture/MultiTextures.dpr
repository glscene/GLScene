(*
   GLScene Multisample texture demo
*)
program MultiTextures;

uses
  Forms,
  fMultiTextures in 'fMultiTextures.pas' {FormMultiSampleTextures};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMultiSampleTextures, FormMultiSampleTextures);
  Application.Run;
end.
