// a Demo with absolutely no code :)
program SimpleNavigation;

uses
  Forms,
  SimpleNavigationFm in 'SimpleNavigationFm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
