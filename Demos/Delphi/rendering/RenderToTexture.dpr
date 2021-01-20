{:
 Rendering to texture demo
}
program RenderToTexture;

uses
  Forms,
  RenderToTextureFm in 'RenderToTextureFm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
