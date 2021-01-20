{: WaterPlane demo

}
program WaterPlane;

uses
  Forms,
  WaterPlaneFm in 'WaterPlaneFm.pas' {FormWaterPlane};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormWaterPlane, FormWaterPlane);
  Application.Run;
end.
