program MovementsD;

uses
  Vcl.Forms,
  fdMovements in 'fdMovements.pas' {FormMovements},
  fColumnD in 'column\fColumnD.pas' {FormColumn},
  fEventsD in 'events\fEventsD.pas' {FormEvents},
  fHierarchD in 'hierarch\fHierarchD.pas' {FormHierarchy},
  fManualD in 'manual\fManualD.pas' {FormManual},
  fObjmoveD in 'objmove\fObjmoveD.pas' {FormObjmove},
  fPointtoD in 'pointto\fPointtoD.pas' {FormPointto},
  fPongD in 'pong\fPongD.pas' {FormPong},
  fSmoothNaviD in 'smoothnavi\fSmoothNaviD.pas' {FormSmoothNavigator},
  fTweeningD in 'tweening\fTweeningD.pas' {FormTweening};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMovements, FormMovements);
  Application.Run;
end.
