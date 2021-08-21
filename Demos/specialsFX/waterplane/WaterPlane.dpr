{: WaterPlane demo

}
program WaterPlane;

uses
  Forms,
  fWaterPlane in 'fWaterPlane.pas' {FormWaterPlane};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormWaterPlane, FormWaterPlane);
  Application.Run;
end.
