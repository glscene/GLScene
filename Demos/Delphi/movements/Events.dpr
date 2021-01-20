program Events;

uses
  Forms,
  EventsFm in 'EventsFm.pas' {FormEvents};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormEvents, FormEvents);
  Application.Run;
end.
