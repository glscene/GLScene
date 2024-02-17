(*
   GLScene Multisample texture demo
*)
program MultiTexturesD;

uses
  Forms,
  fMultiTexturesD in 'fMultiTexturesD.pas' {FormMultiSampleTextures};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMultiSampleTextures, FormMultiSampleTextures);
  Application.Run;
end.
