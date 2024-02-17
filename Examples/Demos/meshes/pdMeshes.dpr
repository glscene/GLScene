program pdMeshes;

uses
  Vcl.Forms,
  fdMeshes in 'fdMeshes.pas' {FormMeshes};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMeshes, FormMeshes);
  Application.Run;
end.
