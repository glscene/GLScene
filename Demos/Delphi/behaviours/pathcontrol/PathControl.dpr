{: This Form demonstrates basic "Pathcontrol" movements.

   You can modified the Looped property of the path to enable the path-looping.
   Set ShowPath property to turn on or turn off the path-displaying
}
program PathControl;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.