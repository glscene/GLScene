program Grass;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdGrass in 'fdGrass.pas' {FormGrass};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGrass, FormGrass);
  Application.Run;
end.
