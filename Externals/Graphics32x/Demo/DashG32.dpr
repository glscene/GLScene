program DashG32;

uses
  System.StartUpCopy,
  FMX.Forms,
  fxDash in 'fxDash.pas' {FormDash};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDash, FormDash);
  Application.Run;
end.
