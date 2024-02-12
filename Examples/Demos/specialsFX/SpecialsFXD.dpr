program SpecialsFXD;

uses
  Vcl.Forms,
  fSpecialsFXD in 'fSpecialsFXD.pas' {frmSpecialsFX};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSpecialsFX, frmSpecialsFX);
  Application.Run;
end.
