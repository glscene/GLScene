program Image32;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormImage32Example};

begin
  Application.Initialize;
  Application.CreateForm(TFormImage32Example, FormImage32Example);
  Application.CreateForm(TFormImage32Example, FormImage32Example);
  Application.Run;
end.
