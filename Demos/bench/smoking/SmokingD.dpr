{: Benchmark and stress test for PFX.

  Fires are made of additively blended particles, smoke of transparently
  blended ones. Smokes of distinct fires should hide each other, and smoke
  in a particular fire should hide its top flames a bit.

  02/03/2005 - GF3 / AXP 2 GHz - 53 FPS
}
program SmokingD;

uses
  Forms,
  fSmokingD in 'fSmokingD.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
