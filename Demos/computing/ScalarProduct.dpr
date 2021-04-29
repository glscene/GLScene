program ScalarProduct;

uses
  Forms,
  ScalarProductFm in 'ScalarProductFm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
