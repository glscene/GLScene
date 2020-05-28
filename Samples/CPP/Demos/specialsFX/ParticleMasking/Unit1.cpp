//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLAsyncTimer"
#pragma link "GLBaseClasses"
#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLEParticleMasksManager"
#pragma link "GLGeomObjects"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLParticleFX"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLWindowsFont"
#pragma link "GLAsyncTimer"
#pragma resource "*.dfm"
TForm1 *Form1;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
	PlaneHeights = 5;
	PlaneWidths  = 5;
	PlaneDepths  = 5;
	PlaneOffSets = 3;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RefreshMask()
{
  TRect Rect;
  TGLLibMaterial *Mat;
  char Letter;
  int Depth;

  Letter = Edit2->Text[1];
  Depth = StrToIntDef(Edit1->Text, 50);

  XPlane->Position->X = -PlaneOffSets;
  XPlane->Height = PlaneHeights;
  XPlane->Width = PlaneWidths;

  YPlane->Position->Y = -PlaneOffSets;
  YPlane->Height = PlaneHeights;
  YPlane->Width = PlaneWidths;

  ZPlane->Position->Z = -PlaneOffSets;
  ZPlane->Height = PlaneHeights;
  ZPlane->Width = PlaneWidths;

  Rect.Left = 0;
  Rect.Top = 0;
  Rect.Bottom = XImage->Height;
  Rect.Right = XImage->Width;

  XImage->Canvas->Font->Name = "Arial";
  XImage->Canvas->Font->Size = 180;
  XImage->Canvas->Font->Color = clWhite;
  XImage->Canvas->Pen->Color = clBlack;
  XImage->Canvas->Pen->Style = psSolid;
  XImage->Canvas->Brush->Color = clBlack;
  XImage->Canvas->Brush->Style = bsSolid;

  XImage->Canvas->FillRect(Rect);

  XImage->Canvas->TextOut(Round((XImage->Width - XImage->Canvas->TextWidth(Letter)) / 2),
	 Round((XImage->Height - XImage->Canvas->TextHeight(Letter)) / 2), Letter);

  Mat = MatLib->LibMaterialByName("XMask");
 ((TGLPersistentImage *)Mat->Material->Texture->Image)->Picture->Bitmap->Height = XImage->Height;
 ((TGLPersistentImage *)Mat->Material->Texture->Image)->Picture->Bitmap->Width = XImage->Width;
 ((TGLPersistentImage *)Mat->Material->Texture->Image)->Picture->Bitmap->Canvas->Draw(0, 0, XImage->Picture->Graphic);

  // this is a very recent implementation, the ability to generate other masks from 1 mask, so it satisfies
  // the requirements for the particle mask manager. useful for text and making basic shapes (cylinders etc)

  GLEParticleMasksManager1->ParticleMaskByName("mask")->GenerateMaskFromProjection(pptXMask, pptYMask, Depth);
  GLEParticleMasksManager1->ParticleMaskByName("mask")->GenerateMaskFromProjection(pptXMask, pptZMask, Depth);

  Mat = MatLib->LibMaterialByName("YMask");
  YImage->Canvas->Draw(0, 0, ((TGLPersistentImage *)Mat->Material->Texture->Image)->Picture->Graphic);

  Mat = MatLib->LibMaterialByName("ZMask");
  ZImage->Canvas->Draw(0, 0, ((TGLPersistentImage *)Mat->Material->Texture->Image)->Picture->Graphic);
}


void __fastcall TForm1::FormCreate(TObject *Sender)
{
  RefreshMask();
  Sphere->Visible = CheckBox1->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencerProgress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  Sphere->TurnAngle = -newTime * 20;
  Sphere->Position->X = Cos(DegToRad(newTime * 20)) * 1.5;
  Sphere->Position->Z = Sin(DegToRad(newTime * 20)) * 1.5;
  SceneViewer->Invalidate();

}
//---------------------------------------------------------------------------

void __fastcall TForm1::SceneViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y)
{
  if (Shift.Contains(ssLeft))
	Camera->MoveAroundTarget(my - Y, mx - X);

  mx = X;
  my = Y;

}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
		  TPoint &MousePos, bool &Handled)
{
  Camera->FocalLength = Camera->FocalLength + (WheelDelta / 150);
  Handled = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::PLManagerCreateParticle(TObject *Sender, TGLParticle *aParticle)

{
  int i;
  TGLParticle *Particle;
  // using point lights just coz it is easier to see the effect
  if (PLManager->Particles->ItemCount() > 0)
	for (i = 0; i < PLManager->Particles->ItemCount(); i++)
	{
	  Particle = PLManager->Particles->Items[i];
	  // first we find the particles that are tagged (since before particle creation, the position is overridden)
	  if (Particle->Tag == 1)
	  {
		if (CheckBox1->Checked)
		  GLEParticleMasksManager1->SetParticlePositionFromMaskTarget(Particle, "mask", Sphere);
		else
		  GLEParticleMasksManager1->SetParticlePositionFromMask(Particle, "mask");
		Particle->Tag = 0;
	  }
	}
  // we tag the new particle for when another particle is made so we know this one needs updating aswell
  aParticle->Tag = 1;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  Sphere->Visible = CheckBox1->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::AsyncTimer1Timer(TObject *Sender)
{
  Caption = FormatFloat("Particle Masking - FPS: 0.0", SceneViewer->FramesPerSecond()) + " Particle Count: " + IntToStr(PLManager->ParticleCount());
  SceneViewer->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject *Sender)
{
  Camera->Position->Z = 0;
  Camera->Position->Y = 0;
  Camera->Position->X = 4;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
  Camera->Position->X = 0;
  Camera->Position->Z = 0.01;
  Camera->Position->Y = 4;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button3Click(TObject *Sender)
{
  Camera->Position->X = 0;
  Camera->Position->Y = 0;
  Camera->Position->Z = 4;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button4Click(TObject *Sender)
{
  FormCreate(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Edit2Change(TObject *Sender)
{
  RefreshMask();
}
//---------------------------------------------------------------------------


void __fastcall TForm1::Edit3Change(TObject *Sender)
{
  GLEParticleMasksManager1->ParticleMaskByName("mask")->PitchAngle = System::Sysutils::StrToFloatDef(Edit3->Text, 0);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Edit4Change(TObject *Sender)
{
  GLEParticleMasksManager1->ParticleMaskByName("mask")->RollAngle = System::Sysutils::StrToFloatDef(Edit4->Text, 0);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Edit5Change(TObject *Sender)
{
  GLEParticleMasksManager1->ParticleMaskByName("mask")->TurnAngle = System::Sysutils::StrToFloatDef(Edit5->Text, 0);
}
//---------------------------------------------------------------------------

