// a Demo with absolutely no code :)
program SimpleNavi;

uses
  Forms,
  fSimpleNavi in 'fSimpleNavi.pas' {FormSimpleNavigation};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSimpleNavigation, FormSimpleNavigation);
  Application.Run;
end.
