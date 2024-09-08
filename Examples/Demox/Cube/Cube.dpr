program Cube;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdCube in 'fdCube.pas' {FormCube};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCube, FormCube);
  Application.Run;
end.
