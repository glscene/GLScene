// a Demo with absolutely no code :)
program SimpleNaviD;

uses
  Forms,
  fSimpleNaviD in 'fSimpleNaviD.pas' {FormSimpleNavigation};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSimpleNavigation, FormSimpleNavigation);
  Application.Run;
end.
