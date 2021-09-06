program MeshGradients;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fMeshGradients in 'fMeshGradients.pas' {FrmMeshGradients};

begin
  Application.Initialize;
  Application.CreateForm(TFrmMeshGradients, FrmMeshGradients);
  Application.Run;
end.

