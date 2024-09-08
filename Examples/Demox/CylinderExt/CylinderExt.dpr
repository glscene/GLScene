program CylinderExt;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdCylinderExt in 'fdCylinderExt.pas' {FormCylinderExt};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCylinderExt, FormCylinderExt);
  Application.Run;
end.
