program GrassD;

uses
  Forms,
  fGrassD in 'fGrassD.pas' {FormGrass};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGrass, FormGrass);
  Application.Run;
end.
