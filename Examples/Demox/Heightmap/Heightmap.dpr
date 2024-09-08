program Heightmap;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdHeightmap in 'fdHeightmap.pas' {FormHeightmap};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormHeightmap, FormHeightmap);
  Application.Run;
end.
