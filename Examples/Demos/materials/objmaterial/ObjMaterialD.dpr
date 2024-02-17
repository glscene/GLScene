program ObjMaterialD;

uses
  Vcl.Forms,
  fdObjectMats in 'fdObjectMats.pas' {FormMO},
  GLS.Polyhedra in 'GLS.Polyhedra.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMO, FormMO);
  Application.Run;
end.
