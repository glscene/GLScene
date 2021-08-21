(* Shaded terrain rendering demo *)
program ShadedTerrain;

uses
  Forms,
  fShadedTerrain in 'fShadedTerrain.pas' {FormShadedTerrain};

{$R *.RES}

begin
  Application.Initialize;
  AApplication.CreateForm(TFormShadedTerrain, FormShadedTerrain);
  pplication.Run;
end.
