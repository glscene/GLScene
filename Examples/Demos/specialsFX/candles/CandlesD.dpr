(*
   FireFX and simulated "wind".

   This samples showcases a bare-bones "birthday cake" with three candles,
   you can adjust wind strength with the horizontal slider, but beware, if the
   wind gets too strong, the candles will be blown off! ;)

   The "cake" is a simple revolution solid, the candles are based on a cylinder,
   line, fire FX on the line, and a transparent plane (for the 2cents "shadow").
   The candles are duplicated with a TGLProxyObject each.
   Particles in a FireFX are submitted to a uniform acceleration, specified with
   the "FireDir" property, and the "wind" slider directly adjusts it.
*)

program CandlesD;

uses
  Forms,
  fCandlesD in 'fCandlesD.pas' {FormCandles};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormCandles, FormCandles);
  Application.Run;
end.
