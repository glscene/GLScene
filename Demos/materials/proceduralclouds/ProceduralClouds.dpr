//
{: Procedural Textures for Clouds / Tobias Peirick }
//
program ProceduralClouds;

uses
  Forms,
  ProceduralCloudsFm in 'ProceduralCloudsFm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
