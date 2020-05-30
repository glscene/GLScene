{: Early mesh subdivision refinement demo.

   Yes, it's slow, the edge data construction is not optimized, and the MD2
   format isn't really suited for refinement (that's approx 200 frames we have
   to subdivide and keep in memory... but it's good for benchmarking!).
}
program subdivide;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
