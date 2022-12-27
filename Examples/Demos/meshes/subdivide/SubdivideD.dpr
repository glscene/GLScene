(*
  Early mesh subdivision refinement demo.
  Yes, it's slow, the edge data construction is not optimized, and the MD2
  format isn't really suited for refinement (that's approx 200 frames we have
  to subdivide and keep in memory... but it's good for benchmarking!).
*)
program SubdivideD;

uses
  Forms,
  fSubdivideD in 'fSubdivideD.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSubdivide, FormSubdivide);
  Application.Run;
end.
