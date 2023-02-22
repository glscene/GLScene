(*
   Thor's fur coat rotation created by Trinix
*)
program FurShaderD;

uses
  Forms,
  fFurShaderD in 'fFurShaderD.pas' {FormFur};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFur, FormFur);
  Application.Run;
end.
