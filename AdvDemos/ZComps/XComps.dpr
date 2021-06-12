program XComps;

uses
  Vcl.Forms,
  Unit1 in 'C:\Users\Admin\Documents\Embarcadero\Studio\Projects\Unit1.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
