//
// Procedural Textures for Clouds / Tobias Peirick
//
program ProcClouds;

uses
  Forms,
  fProcClouds in 'fProcClouds.pas' {FormClouds};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormClouds, FormClouds);
  Application.Run;
end.
