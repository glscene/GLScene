//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLAsyncTimer"
#pragma link "GLBaseClasses"
#pragma link "GLBehaviours"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLGraph"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTeapot"
#pragma link "GLWin32Viewer"
#pragma link "GLzBuffer"
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
 SetGLSceneMediaDir();
 GLMaterialLibrary1->Materials->Items[2]->Material->Texture->Image->LoadFromFile("marbletiles.jpg");
 GLMaterialLibrary1->Materials->Items[2]->Material->Texture->Disabled = false;

 GLMaterialLibrary1->Materials->Items[3]->Material->Texture->Image->LoadFromFile("beigemarble.jpg");
 GLMaterialLibrary1->Materials->Items[3]->Material->Texture->Disabled = false;

 RotateBoxClick(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
 mx = X; my = Y;
 ActiveControl = DistanceBar;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y)
{
   if (Shift.Contains(ssLeft) || Shift.Contains(ssRight))
	 GLCamera1->MoveAroundTarget(my-Y, mx-X);
   mx = X; my = Y;
   GLCadencer1->Progress();
   Viewer->Refresh();
   Caster->Refresh();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CasterMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
 mx2 = X; my2 = Y;
 ActiveControl = DistanceBar2;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CasterMouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y)
{
   if (Shift.Contains(ssLeft) || Shift.Contains(ssRight))
	 GLCamera2->MoveAroundTarget(my2-Y, mx2-X);
   mx2 = X; my2 = Y;
   if (Shift.Contains(ssLeft) || Shift.Contains(ssRight))
   {
	  Shadows1->CastShadow();
	  GLCadencer1->Progress();
	  Viewer->Refresh();
	  Caster->Refresh();
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DistanceBarChange(TObject *Sender)
{
  float Dist = GLCamera1->DistanceToTarget();
  float  NewDist = ((float)DistanceBar->Position/4)*((float)DistanceBar->Position/4)+1;
  GLCamera1->Position->AsAffineVector = VectorScale(GLCamera1->Position->AsAffineVector, (float)NewDist/Dist);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DistanceBar2Change(TObject *Sender)
{
  float Dist = GLCamera2->DistanceToTarget();
  float  NewDist = ((float)DistanceBar2->Position/4)*((float)DistanceBar2->Position/4)+1;
  GLCamera2->Position->AsAffineVector = VectorScale(GLCamera2->Position->AsAffineVector, (float)NewDist/Dist);
  Shadows1->CastShadow();
  Caster->Refresh();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CastBtnClick(TObject *Sender)
{
 double RefTime = GLCadencer1->GetCurrenttime();
 Shadows1->CastShadow();
 Viewer->Refresh();
 TimeLbl->Caption = IntToStr(Round((GLCadencer1->GetCurrenttime()-(int)RefTime)*100));

}
//---------------------------------------------------------------------------
void __fastcall TForm1::ViewerMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
  Viewer->Visible = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CasterMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
 Shadows1->CastShadow();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FadeBoxClick(TObject *Sender)
{
  Shadows1->DepthFade = FadeBox->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::HeightField1GetHeight(const float x, const float y, float &z,
          TVector4f &Color, TTexPoint &TexPoint)
{
  z = 0;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FrustBoxClick(TObject *Sender)
{
  Shadows1->FrustShadow = FrustBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AsyncTimer1Timer(TObject *Sender)
{
  Caption = "Shadows " + Format("%.2f FPS",
	ARRAYOFCONST ((Viewer->FramesPerSecond())));
  Viewer->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RotateBoxClick(TObject *Sender)
{
//GLAsyncTimer1->Enabled =RotateBox->Checked;
  GLCadencer1->Enabled = RotateBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ShadowOnBoxClick(TObject *Sender)
{
  Shadows1->Visible = ShadowOnBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SoftBoxClick(TObject *Sender)
{
 Shadows1->Soft = SoftBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SkyShadBoxClick(TObject *Sender)
{
 Shadows1->SkyShadow = SkyShadBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FocalChange(TObject *Sender)
{
 GLCamera2->FocalLength = Focal->Position;
 MemView->Render();
 Caster->Refresh();
 Shadows1->CastShadow();
 Viewer->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::dovBarChange(TObject *Sender)
{
 GLCamera2->DepthOfView = dovBar->Position;
 MemView->Render();
 Caster->Refresh();
 Shadows1->CastShadow();
 Viewer->Refresh();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AlphaBarChange(TObject *Sender)
{
  Shadows1->Color->Alpha = (float)AlphaBar->Position/256;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
   Shadows1->CastShadow();
}
//---------------------------------------------------------------------------
