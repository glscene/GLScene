program Grow;





{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fGrow in 'fGrow.pas' {FormGrow};

begin
  Application.Initialize;
  Application.CreateForm(TFormGrow, FormGrow);
  Application.Run;
end.
