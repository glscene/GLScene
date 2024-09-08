program SphereExt;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdSphereExt in 'fdSphereExt.pas' {FormSphereExt};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSphereExt, FormSphereExt);
  Application.Run;
end.
