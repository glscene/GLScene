program MaterialsD;

uses
  Vcl.Forms,
  fdMaterials in 'fdMaterials.pas' {frmMaterials};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMaterials, frmMaterials);
  Application.Run;
end.
