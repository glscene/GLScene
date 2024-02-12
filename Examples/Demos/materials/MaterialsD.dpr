program MaterialsD;

uses
  Vcl.Forms,
  fMaterialsD in 'fMaterialsD.pas' {frmMaterials};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMaterials, frmMaterials);
  Application.Run;
end.
