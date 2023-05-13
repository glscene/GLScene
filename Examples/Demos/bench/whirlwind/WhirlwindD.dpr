program WhirlwindD;

uses
  Vcl.Forms,
  fWhirlD in 'fWhirlD.pas' {FormWhirl};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWhirl, FormWhirl);
  Application.Run;
end.
