program ComputingD;

uses
  Vcl.Forms,
  fComputingD in 'fComputingD.pas' {frmComputing};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmComputing, frmComputing);
  Application.Run;
end.
