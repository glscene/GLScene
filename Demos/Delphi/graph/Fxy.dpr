(* Basic demo for the TGLHeightField and TGLXYZGrid objects.

   HeightFields are used to materialize z=f(x, y) surfaces, you can use it to
   render anything from math formulas to statistics. Most important properties
   of an height field are its sampling scales (X & Y) that determine the extents
   and the resolution of the base grid.

   The component will then invoke it OnGetHeight event to retrieve Z values for
   all of the grid points (values are retrieved only once for each point). Each
   point may have an additionnal color and texture coordinate.

   Three XYZ grids are used to materialize planes, and have been colored to match
   the axis colors. Two controls have been provided to move the XY grid (blue)
   up or down, and center or bound-align the XZ and YZ grids.

   The heightfield component takes care of all the tessellation, so there is not
   much in the code of the unit. Check the advanced "heightfield" sample for
   more dynamic uses.
*)
program Fxy;

uses
  Forms,
  FxyFm in 'FxyFm.pas' {FormFxy};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormFxy, FormFxy);
  Application.Run;
end.
