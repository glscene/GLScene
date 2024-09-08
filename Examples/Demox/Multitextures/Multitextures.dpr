program Multitextures;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdMultitextures in 'fdMultitextures.pas' {FormMultitextures};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMultitextures, FormMultitextures);
  Application.Run;
end.
