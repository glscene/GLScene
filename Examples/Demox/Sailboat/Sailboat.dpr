program Sailboat;
uses
  System.StartUpCopy,
  FMX.Forms,
  fdSailboat in 'fdSailboat.pas' {FormSailboatDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSailboatDemo, FormSailboatDemo);
  Application.Run;
end.
