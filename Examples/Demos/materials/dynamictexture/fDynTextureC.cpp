//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fDynTextureC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
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
  GLSceneViewer1->Align = alClient;
  frame =0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)

{
  TGLTexture *tex;
  TGLDynamicTextureImage *img;

  tex = GLMaterialLibrary1->TextureByName("Anim");
  if (!(dynamic_cast<TGLDynamicTextureImage *>(tex->Image)))
	exit;

  img = (TGLDynamicTextureImage *) tex->Image;
  switch (Key)
  {
  case VK_F2:
	{
	  img->UsePBO = false;
	  GLSceneViewer1->ResetPerformanceMonitor();
	  frame = 0;
	  break;
	}
  case VK_F3:
	{
	  img->UsePBO = true;
	  GLSceneViewer1->ResetPerformanceMonitor();
	  frame = 0;
	  break;
	}
  case VK_F4: partial = !partial; break;
  default:
	  ;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject *Sender)
{
  GLCamera1->SceneScale = GLSceneViewer1->ClientWidth / 400;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLDirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci)

{
  TGLTexture *tex;
  TGLDynamicTextureImage *img;
//  void* p;
  TRGBQuad* p;
  int X, Y;

  tex = GLMaterialLibrary1->TextureByName("Anim");
  if (tex->Disabled)
  {
	tex->ImageClassName = __classid(TGLDynamicTextureImage)->ClassName();
	img = (TGLDynamicTextureImage *) tex->Image;
	img->Width = 256;
	img->Height = 256;

	tex->TextureFormat = tfRGBA;

	tex->TextureMode = tmReplace;
	tex->Disabled = false;
  }

  img = (TGLDynamicTextureImage *)tex->Image;

  img->BeginUpdate();

  // draw some silly stuff
  //
  p = new TRGBQuad();
  p = (TRGBQuad*)img->Data;
  frame++;
  // first frame must always be drawn completely
  if (partial && (frame > 1))
  {
	// do partial update, set the dirty rectangle
	// note that we do NOT offset the p pointer,
	// since it is relative to the dirty rectangle,
	// not the complete texture
	// also note that the right/bottom edge is not included
	// in the upload
	img->DirtyRectangle = Rect(
	  img->Width / 4,
	  img->Height / 4,
	  img->Width * 3 / 4,
	  img->Height * 3 / 4);
  }

  for (Y = img->DirtyRectangle.Top; Y < img->DirtyRectangle.Bottom - 1; Y++)
  {
	for (X = img->DirtyRectangle.Left; X < img->DirtyRectangle.Right - 1; X++)
	{
	  p->rgbRed  = ((X ^ Y) + frame) && 255;
	  p->rgbGreen = ((X + frame) ^ Y) && 255;
	  p->rgbBlue = ((X - frame) ^ (Y + frame)) && 255;
	  p++;
	}
  }
  img->EndUpdate();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
const

//  PBOText: array[Boolean] of string = ("PBO disabled", "PBO enabled");
///  String PBOText[]: array[Boolean] of string = ("PBO disabled", "PBO enabled");

  TGLTexture *tex;
  TGLDynamicTextureImage *img;
  String s;

  tex = GLMaterialLibrary1->TextureByName("Anim");
  if ((dynamic_cast<TGLDynamicTextureImage *>(tex->Image)))
  {
	img = (TGLDynamicTextureImage *) tex->Image;
  s = "PBO";///PBOText[img->UsePBO];
  s = "Dynamic Text";
  }
  Caption = Format("%s - %s", ARRAYOFCONST ((GLSceneViewer1->FramesPerSecondText(), s)));
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
