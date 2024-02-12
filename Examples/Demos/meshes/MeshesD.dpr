program MeshesD;

uses
  Vcl.Forms,
  fMeshesD in 'fMeshesD.pas' {frmMeshes};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMeshes, frmMeshes);
  Application.Run;
end.
