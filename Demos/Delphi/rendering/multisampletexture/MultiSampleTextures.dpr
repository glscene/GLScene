{:
   GLScene Multisample texture demo
}
program MultiSampleTextures;

uses
  Forms,
  uMain in 'uMain.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLDemoForm, GLDemoForm);
  Application.Run;
end.
