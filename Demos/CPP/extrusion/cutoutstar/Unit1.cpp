//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLExtrusion"
#pragma link "GLMultiPolygon"
#pragma link "GLNodes"


#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   int i;
   float r, x, y;
   const int
	  cSteps = 16;
   const int
	  c2 = Glvectorgeometry::c2PI;
      new TGLExtrusionSolid(ExtrusionSolid);
   // a small star contour
	  for (i=0; i<cSteps; i++)
	  {
		 r =2+(i && 1)*2; //r :=2+(i and 1)*2;
		 SinCosine(i*(float)c2/cSteps, y, x);
		 ExtrusionSolid->Contours->Add()->Nodes->AddNode(x*r, y*r, 0);
	  }
	  // add an empty contour for the square cutout (see progress event)
	  ExtrusionSolid->Contours->Add();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
   float x, y;

   // Make our Extrusion roll
	ExtrusionSolid->Roll(deltaTime*10);

   // At each frame, we drop the cutout and make a new.
   // Note that we could also have defined it once in the FormCreate and then moved
   // it around with the TGLNodes methods.
	SinCosine(newTime, 2, y, x);
	ExtrusionSolid->Contours->Items[1]->Free();
	ExtrusionSolid->Contours->Add()->Nodes->AddNode(x-1, y-1, 0);
	ExtrusionSolid->Contours->Add()->Nodes->AddNode(x+1, y-1, 0);
	ExtrusionSolid->Contours->Add()->Nodes->AddNode(x+1, y+1, 0);
	ExtrusionSolid->Contours->Add()->Nodes->AddNode(x-1, y+1, 0);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   // Standard FPS counter
   PanelFPS->Caption = Format("%.1f FPS",
	 ARRAYOFCONST((GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
