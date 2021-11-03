{
Demo for loading and controlling a Quake 3 model
By Stuart Gooding and Marcus Oblak (aka MRQZZZ)
}

program Q3DemoD;

uses
  Forms,
  fQ3DemoD in 'fQ3DemoD.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
