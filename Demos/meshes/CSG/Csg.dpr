{  Demo of Constructive Solid Geometry in GLScene.

   Its kept very simple, you can use mouse to rotate view(drag)
   and mousewheel to zoom/unzoom.

   The CSG system uses BSP to optimize what triangles it considers.
   Its kept on a mesh basis to simplyfy things, it allways generates new BSP's,
   even the meshes allready had BSP optimization.

   The demo uses the polyhedron.3ds, resource from the GLScene pack.

   Author: Joen Joensen.
   Contributed to the GLScene community.

   Features: CSG_Union, CSG_Subtraction, CSG_Intersection.

	 History :
     29/11/03 - JAJ - Created and Submitted to GLScene.
                      Sometimes a single tri is messed up...
                     often(1/3) happends on 2 triangles in this demo when using intersection)
}
program Csg;

uses
  Forms,
  CsgFm in 'CsgFm.pas' {FormCsg};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCsg, FormCsg);
  Application.Run;
end.
