(* 
   Advenced for the TGLHeightField object.

   Check the fxy sample first.

   This sample shows a few more tricks : how to switch formulas at run-time,
   effects of base grid extents and resolution change as well as color and
   lighting options of the TGLHeightField.

   Note that maxed out grid size and minimum step (high resolution) will bring
   most of todays cards to their knees (if they do not just crash, that is).

   Used formulas :
   The Formula1 is of type Sin(d)/(1+d), with d=sqr(x)+sqr(y), you may note
   the interesting sampling-interference effect with big step values (low res)
   and remember your math teacher's warnings on graph-plotting :)

   Formula2 is a more classic sin*cos mix

   Dynamic is the third formula, if you pick it, a small ball will appear and
   move around, the plotted formula being the square distance to the ball.
*)
program HeightField;

uses
  Forms,
  HeightFieldFm in 'HeightFieldFm.pas' {FormHeightField};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormHeightField, FormHeightField);
  Application.Run;
end.
