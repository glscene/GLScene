program Pathfinder;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdPathfinder in 'fdPathfinder.pas' {FormPathfinder},
  uGBEPathFinder in 'D:\GLScene\Sourcex\uGBEPathFinder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPathfinder, fFormPathfinder);
  Application.Run;
end.
