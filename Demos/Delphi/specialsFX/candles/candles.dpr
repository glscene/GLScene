{: FireFX and simulated "wind".

   This samples showcases a bare-bones "birthday cake" with three candles,
   you can adjust wind strength with the horizontal slider, but beware, if the
   wind gets too strong, the candles will be blown off! ;)

   The "cake" is a simple revolution solid, the candles are based on a cylinder,
   line, fire FX on the line, and a transparent plane (for the 2cents "shadow").
   The candles are duplicated with a TGLProxyObject each.<br>
   Particles in a FireFX are submitted to a uniform acceleration, specified with
   the "FireDir" property, and the "wind" slider directly adjusts it.
}
program candles;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
