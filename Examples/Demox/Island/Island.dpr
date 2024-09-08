program Island;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  fdIsland in 'fdIsland.pas' {FormIsland},
  frmSmartphone in 'frmSmartphone.pas' {fSmartphone: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape];
  Application.CreateForm(TFormIsland, FormIsland);
  Application.Run;
end.
