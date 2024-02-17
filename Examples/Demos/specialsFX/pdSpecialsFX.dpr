program pdSpecialsFX;

uses
  Vcl.Forms,
  fdSpecialsFX in 'fdSpecialsFX.pas' {frmSpecialsFX};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSpecialsFX, frmSpecialsFX);
  Application.Run;
end.
