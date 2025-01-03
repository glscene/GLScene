program Polygons;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fPolygons in 'fPolygons.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormPolygons, FormPolygons);
  Application.Run;
end.
