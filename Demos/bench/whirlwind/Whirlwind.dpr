program Whirlwind;

uses
  Vcl.Forms,
  fWhirl in 'fWhirl.pas' {FormWhirlD};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWhirlD, FormWhirlD);
  Application.Run;
end.
