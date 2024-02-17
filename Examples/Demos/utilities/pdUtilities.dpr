program pdUtilities;

uses
  Vcl.Forms,
  fdUtilities in 'fdUtilities.pas' {FormUtilities};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormUtilities, FormUtilities);
  Application.Run;
end.
