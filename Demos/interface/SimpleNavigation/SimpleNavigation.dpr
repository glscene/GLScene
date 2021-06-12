// a Demo with absolutely no code :)
program SimpleNavigation;

uses
  Forms,
  SimpleNavigationFm in 'SimpleNavigationFm.pas' {FormSimpleNavigation};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSimpleNavigation, FormSimpleNavigation);
  Application.Run;
end.
