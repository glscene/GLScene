//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "math.h"
#include "GLGraphics.hpp"
#include "GLTexture.hpp"
#include "jpeg.hpp"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLGraph"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::HeightFieldGetHeight(const float x, const float y,
      float &z, TVector4f &color, TTexPoint &texPoint)
{
   // Here is the warping function
   // it basicly converts current pixel coords (x, y) to deformed coords (dx, dy)

   float d, dx, dy;
   TAffineVector vec;

   switch (warpEffect)
   {
      case 0 : { // the "zoom" effect
         d = 1.0-exp(-sqrt((x-warpX)*(x-warpX)+(y-warpY)*(y-warpY))/warpRadius);
         dx = x*d+warpX*(1-d);
         dy = y*d+warpY*(1-d);
         break;
      }
      case 1 : { // the "spin" effect
		 vec.X = x-warpX;
		 vec.Y = 0.0;
		 vec.Z = y-warpY;
		 d = VectorNorm(vec);
		 RotateVectorAroundY(vec, (warpRadius*warpRadius)/(d+1.0));
		 dx = warpX+vec.X;
		 dy = warpY+vec.Z;
         break;
      }
      default :
        throw Exception("Unknown warp effect "+IntToStr(warpEffect));
   }

   // apply tex coord
   texPoint.S = dx/HeightField->XSamplingScale->Max;
   texPoint.T = dy/HeightField->YSamplingScale->Max;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::MIOpenImageFileClick(TObject *Sender)
{
  TPicture *picture;

   if (OpenPictureDialog->Execute())
   {
      picture = new TPicture();
      try
      {
         // load picture
         picture->LoadFromFile(OpenPictureDialog->FileName);
         // adjust HeightField
         HeightField->XSamplingScale->Max = picture->Width+0.1;
         HeightField->YSamplingScale->Max = picture->Height+0.1;
         HeightField->Material->Texture->Image->Assign(picture);
         // resize main window
         Width = Width-GLSceneViewer->Width+picture->Width;
         Height = Height-GLSceneViewer->Height+picture->Height;
         // adjust camera
         GLCamera->Position->X = picture->Width/2.0;
         GLCamera->Position->Y = picture->Height/2.0;
         GLCamera->FocalLength = 100.0/picture->Width;
      }
      __finally
      {
         delete picture;
      }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MISaveCurrentImageClick(TObject *Sender)
{
   TGLBitmap32 *bmp32;
   Graphics::TBitmap *bmp;

   bmp32 = GLSceneViewer->Buffer->CreateSnapShot();
   try
   {
      if (SaveDialog->Execute())
      {
         bmp = bmp32->Create32BitsBitmap();
         try
         {
            bmp->SaveToFile(SaveDialog->FileName);
         }
         __finally
         {
            delete bmp;
         }
      }
   }
   __finally
   {
      delete bmp32;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MIExitClick(TObject *Sender)
{
   Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewerMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
   warpX = X;
   warpY = GLSceneViewer->Height-Y;
   HeightField->StructureChanged();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewerMouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
   if (Shift.Contains(ssLeft) || Shift.Contains(ssRight))
   {
      warpX = X;
      warpY = GLSceneViewer->Height-Y;
      HeightField->StructureChanged();
      GLSceneViewer->Refresh();
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   warpX = -1000;
   warpY = -1000;
   warpRadius = 20;        
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MIQualityOptionClick(TObject *Sender)
{
   ((TMenuItem *) Sender)->Checked = true;
   HeightField->XSamplingScale->Step = ((TMenuItem *) Sender)->Tag;
   HeightField->YSamplingScale->Step = ((TMenuItem *) Sender)->Tag;
   HeightField->StructureChanged();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MIRadiusSettingClick(TObject *Sender)
{
   ((TMenuItem *) Sender)->Checked = true;
   warpRadius = ((TMenuItem *) Sender)->Tag;
   HeightField->StructureChanged();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MIZoomEffectClick(TObject *Sender)
{
   ((TMenuItem *) Sender)->Checked = true;
   warpEffect = ((TMenuItem *) Sender)->Tag;
   HeightField->StructureChanged();
}
//---------------------------------------------------------------------------
