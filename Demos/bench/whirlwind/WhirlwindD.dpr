program WhirlwindD;

uses
  Vcl.Forms,
  fWhirlD in 'fWhirlD.pas' {FormWhirlD};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWhirlD, FormWhirlD);
  Application.Run;
end.
