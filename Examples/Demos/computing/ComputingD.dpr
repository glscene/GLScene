program ComputingD;

uses
  Vcl.Forms,
  fdComputing in 'fdComputing.pas' {frmComputing};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmComputing, frmComputing);
  Application.Run;
end.
