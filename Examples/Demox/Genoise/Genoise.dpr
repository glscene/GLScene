program Genoise;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdGenoise in 'fdGenoise.pas' {FormNoise};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormNoise, FormNoise);
  Application.Run;
end.
