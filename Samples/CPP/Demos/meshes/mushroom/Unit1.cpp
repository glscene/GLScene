//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFile3DS"

#pragma resource "*.dfm"
TForm1 *Form1;

const float
  cSpread = 90.00;
const int
  cNbMushrooms = 10;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  SetGLSceneMediaDir();
  //   Randomize;
  // Load mushroom mesh
  FreeForm1->LoadFromFile("mushroom.3ds");
  // Load ground texture
  Disk1->Material->Texture->Image->LoadFromFile("clover.jpg");
  // Duplicate our reference mushroom (but not its mesh data !)
  AddMushRooms();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::AddMushRooms()
{
   int i;
   TGLProxyObject *proxy;
   Glvectorgeometry::TVector s;
   float f;

   // spawn some more mushrooms using proxy objects
   for (i=0; i < cNbMushrooms-1; i++)
   {
	  // create a new proxy and set its MasterObject property
	 proxy  = (TGLProxyObject *)(DummyCube1->AddNewChild(__classid(TGLProxyObject)));
	 proxy->MasterObject = FreeForm1;
	 proxy->ProxyOptions = proxy->ProxyOptions << pooObjects;
	 // retrieve reference attitude
	 proxy->Direction = FreeForm1->Direction;
	 proxy->Up = FreeForm1->Up;
	 // randomize scale
	 s = FreeForm1->Scale->AsVector;
	 f = (Random()+0.2);
	 ScaleVector(s, 5*f);
	 proxy->Scale->AsVector = s;
	 // randomize position
	 proxy->Position->SetPoint(Random(cSpread)-(cSpread/2),
					   f*FreeForm1->Position->Y,
					   Random(cSpread)-(cSpread/2));
	 // randomize orientation
	 proxy->RollAngle = Random(360);
	 proxy->PitchAngle = -90;
   }
   MushRoomCounter = MushRoomCounter + cNbMushrooms; // Inc(mushroomCounter, cNbMushrooms);
}


void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
   mx = X; my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
   if (Shift.Contains(ssLeft))
   {
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
	  mx = X; my = Y;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
   AddMushRooms();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   Caption = Format("Mushroom Counter : %d (%.1f FPS)",
			 ARRAYOFCONST (( MushRoomCounter, GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject *Sender)
{
   // adjust focal Length
   GLCamera1->FocalLength = GLSceneViewer1->Width/8;
   // keep "add mushrooms" centered
   Button1->Left = (Width-Button1->Width)/2;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
   // keep it rendering, we want FPS stats !
   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
