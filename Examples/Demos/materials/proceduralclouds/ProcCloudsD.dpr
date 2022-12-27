//
// Procedural Textures for Clouds / Tobias Peirick
//
program ProcCloudsD;

uses
  Forms,
  fProcCloudsD in 'fProcCloudsD.pas' {FormClouds};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormClouds, FormClouds);
  Application.Run;
end.
