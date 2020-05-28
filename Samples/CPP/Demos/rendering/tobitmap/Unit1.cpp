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
#pragma link "GLHUDObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSpaceText"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"

TForm1 *Form1;
TForm2 *f;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
/*
   A utility function, this takes the bitmap and uses Form2 to display it with
   a regular TImage component.
*/
void TForm1::ViewBitmap(TBitmap *aBitmap, String caption)
{
   Application->CreateForm(__classid(TForm2), &f);
   if ((aBitmap->Width<Screen->Width)&&(aBitmap->Height<Screen->Height))
   {
	  f->ClientWidth = aBitmap->Width;
	  f->ClientHeight = aBitmap->Height;
   }
   else
   {
	  f->ClientWidth = Round(Screen->Width*0.75);
	  f->ClientHeight = Round(Screen->Height*0.75);
   }
   f->Image1->Picture->Bitmap = aBitmap;
   f->Caption = caption;
   f->Image1->Width = aBitmap->Width;
   f->Image1->Height = aBitmap->Height;
   f->Show();
}

//---------------------------------------------------------------------------
void TForm1::RenderToBitmap(Single scale)
{
   TBitmap *bmp;
   __int64 pt;
   double delta;
   pt = StartPrecisionTimer();
   // Rendering to a bitmap requires an existing bitmap,
   // so we create and size a new one
   bmp = new TBitmap();
   // Don't forget to specify a PixelFormat, or current screen pixel format
   // will be used, which may not suit your purposes!
   bmp->PixelFormat = pf24bit;
   bmp->Width = Round(GLSceneViewer1->Width*scale);
   bmp->Height = Round(GLSceneViewer1->Height*scale);
   // Here we just request a render
   // The second parameter specifies DPI (Dots Per Inch), which is
   // linked to the bitmap's scaling
   // "96" is the "magic" DPI scale of the screen under windows
   GLSceneViewer1->Buffer->RenderToBitmap(bmp, Round(96*scale));
   delta = StopPrecisionTimer(pt);
   ViewBitmap(bmp, Format("RenderToBitmap %dx%d - %.3f ms",
	   ARRAYOFCONST((bmp->Width, bmp->Height, delta*1000))));
   bmp->Free();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   SetGLSceneMediaDir();
   HUDSprite1->Material->Texture->Image->LoadFromFile("ashwood.jpg");
   Plane1->Material->Texture->Image->LoadFromFile("marbletiles.jpg");
   Sphere1->Material->Texture->Image->LoadFromFile("marbletiles.jpg");

}
//---------------------------------------------------------------------------
void __fastcall TForm1::BUViewerSnapShotClick(TObject *Sender)
{
   int pt;  // Int64 pt;
   TBitmap *bmp;
   double delta;

   pt = StartPrecisionTimer();
   // Create a snapshot directly from the viewer content
   bmp = GLSceneViewer1->CreateSnapShotBitmap();
   delta = StopPrecisionTimer(pt);
   // Display the bitmap for the user to see and gaze in everlasting awe...
   ViewBitmap(bmp, Format("SnapShot %dx%d - %.3f ms",
	   ARRAYOFCONST((bmp->Width, bmp->Height, delta*1000))));
   // Release the bitmap
   bmp->FreeImage();

}
//---------------------------------------------------------------------------
void __fastcall TForm1::BUSnapShotClick(TObject *Sender)
{
   TGLBitmap32 *bmp32;
   TBitmap *bmp;
   int pt; //Int64;
   double delta;

   pt = StartPrecisionTimer();
   // CreateSnapShot returns a TGLBitmap32, which is a low-level data buffer.
   // However TGLBitmap32 can spawn a regular TBitmap, which we use here
   bmp32 = GLSceneViewer1->Buffer->CreateSnapShot();
   bmp = bmp32->Create32BitsBitmap();
   delta = StopPrecisionTimer(pt);
   // Display the bitmap for the user to see and gaze in everlasting awe...
   ViewBitmap(bmp, Format("SnapShot %dx%d - %.3f ms",
	   ARRAYOFCONST((bmp->Width, bmp->Height, delta*1000))));
   // Don't forget to free your TGLBitmap32 and TBitmap!
   bmp->FreeImage();
   bmp32->Free();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BURenderToBitmapClick(TObject *Sender)
{
  // Render at viewer resolution (scale = 1, DPI = 96)
  RenderToBitmap(1);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BUBitmapx2Click(TObject *Sender)
{
   // Render at twice viewer resolution (scale = 2, DPI = 192 = 96x2)
   RenderToBitmap(2);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BUBitmap300Click(TObject *Sender)
{
   // Screen is "magic" 96 dpi, this gives us our scale
   RenderToBitmap(float(300/96));
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BUBitmap600Click(TObject *Sender)
{
   // Screen is "magic" 96 dpi, this gives us our scale
   RenderToBitmap(600/96);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Sphere1Progress(TObject *Sender, const double deltaTime, const double newTime)

{
   Single h;
   h = 2.5+2*Sin(newTime*4);
   Sphere1->Position->Y = h;
   if (h<1)
	  Sphere1->Scale->Y = h;
   else
	  Sphere1->Scale->Y = 1;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject *Sender)
{
  HUDSprite1->Width = GLSceneViewer1->Width;
  HUDSprite1->Position->X = HUDSprite1->Width*0.5;
  HUDSprite1->Height = GLSceneViewer1->Height;
  HUDSprite1->Position->Y = HUDSprite1->Height*0.5;
}
//---------------------------------------------------------------------------
