//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;

typedef
  TGLMultiPolygon FPlane[6];

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}

//Delphi - function Vektor(x,y,z:Double):TVektor;
/*
TVektor Vektor(double x,y,z)
{
//  result.x := x;
  return->x = x;
  result.y := y;
  result.z := z;
}
*/

void __fastcall TForm1::ReDraw()
{
  DX = 600;
  DY = 400;
  DZ = 19;
//-  CreatePanel;
//-  MakeHole(0,0.5*DX,0.5*DY,DZ,50,DZ);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormShow(TObject *Sender)
{
  ReDraw();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::SetDX(const double Value)
{
  FDX = Value;
  Container->Position->X = -0.5*Value;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SetDY(const double Value)
{
  FDY = Value;
  Container->Position->Y = -0.5*Value;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SetDZ(const double Value)
{
  FDZ = Value;
  Container->Position->Z = -0.5*Value;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::MakeHole(int Side, double X, double Y, double Z,
							 double D, double T, double Phi = 0, double Rho = 0)
{
/*
  double R;
  TGLDummyCube *Dum;
  TGLCylinder *Cyl;
  bool through;

  Dum := TGLDummyCube.Create(nil);
  Dum.Position.x := X;
  Dum.Position.y := Y;
  Dum.Position.z := Z;
  case Side of
	0 : Dum.PitchAngle := -90;
	1 : Dum.RollAngle  :=  90;
	2 : Dum.RollAngle  := 180;
	3 : Dum.RollAngle  := 270;
	4 : Dum.RollAngle  :=   0;
	5 : Dum.PitchAngle :=  90;
  end;
  Dum.PitchAngle := Dum.PitchAngle + Rho;
  Dum.RollAngle := Dum.RollAngle + Phi;
  R := 0.5*D;
  through := true;
  case Side of
	0 : if (Z-T)<=0 then T := Z else through := false;
	1 : if (X+T)>=DX then T := DX-X else through := false;
	2 : if (Y+T)>=DY then T := DY-Y else through := false;
	3 : if (X-T)<=0 then T := X else through := false;
	4 : if (Y-T)<=0 then T := Y else through := false;
	5 : if (Z+T)>=DZ then T := DZ-Z else through := false;
  end;
  Cyl := TGLCylinder.Create(nil);
  AddMaterial(Cyl);
  Cyl.Position.x := 0;
  Cyl.Position.y := - 0.5*T;
  Cyl.Position.z := 0;
  Cyl.Height := T;
  Cyl.BottomRadius := R;
  Cyl.TopRadius := R;
  Cyl.NormalDirection := ndInside;
  if through then Cyl.Parts := [cySides]
  else Cyl.Parts := [cySides,cyBottom];
  Dum.AddChild(Cyl);
  Container.AddChild(Dum);

  Plane[Side].Contours.Add.Nodes.AddXYArc(R/cos(Phi*c180divPi),R,0,360,16, AffineVectorMake(X,Y,0));

  if through then
	Plane[cOpposite[Side]].Contours.Add.Nodes.AddXYArc(R/cos(Phi*c180divPi),R,0,360,16, AffineVectorMake(X,Y,0));
*/
}



