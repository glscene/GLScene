program Events;

uses
  Forms,
  fEvents in 'fEvents.pas' {FormEvents};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormEvents, FormEvents);
  Application.Run;
end.
