program UtilitiesD;

uses
  Vcl.Forms,
  fUtilitiesD in 'fUtilitiesD.pas' {frmUtilities};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmUtilities, frmUtilities);
  Application.Run;
end.
