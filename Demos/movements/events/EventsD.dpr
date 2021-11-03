program EventsD;

uses
  Forms,
  fEventsD in 'fEventsD.pas' {FormEvents};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormEvents, FormEvents);
  Application.Run;
end.
