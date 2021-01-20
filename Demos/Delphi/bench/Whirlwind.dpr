program Whirlwind;

uses
  Vcl.Forms,
  WhirlwindFm in 'WhirlwindFm.pas' {FormWhirlwind};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWhirlwind, FormWhirlwind);
  Application.Run;
end.
