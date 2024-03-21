program ObjMaterialD;

uses
  Vcl.Forms,
  fObjMaterialD in 'fObjMaterialD.pas' {FormMO};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMO, FormMO);
  Application.Run;
end.
