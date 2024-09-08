//---------------------------------------------------------------------------

#include <fmx.h>
#ifdef _WIN32
#include <tchar.h>
#endif

#pragma hdrstop

#include "fcWaves.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GBE.PlaneExtend"
#pragma resource "*.fmx"
TFormWaves *FormWaves;
//---------------------------------------------------------------------------
__fastcall TFormWaves::TFormWaves(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormWaves::FormCreate(TObject *Sender)
{
  GBEPlaneExtend1->Origine = Point3D(-13,-13,0);
  TrackBarAmplitude->Value = GBEPlaneExtend1->Amplitude;
  TrackBarLength->Value = GBEPlaneExtend1->Longueur;
  TrackBarSpeed->Value = GBEPlaneExtend1->Vitesse;
  FloatAnimation1->Start();
}
//---------------------------------------------------------------------------
void __fastcall TFormWaves::FloatAnimation1Process(TObject *Sender)
{
  Viewport3D1->Repaint();
}
//---------------------------------------------------------------------------
void __fastcall TFormWaves::SpinBoxChange(TObject *Sender)
{
  GBEPlaneExtend1->Origine = Point3D(SpinBoxX->Value, SpinBoxY->Value, SpinBoxZ->Value);
}
//---------------------------------------------------------------------------
void __fastcall TFormWaves::SwitchLinesSwitch(TObject *Sender)
{
  GBEPlaneExtend1->ShowLines = SwitchLines->IsChecked;
}
//---------------------------------------------------------------------------
void __fastcall TFormWaves::TrackBarAmplitudeTracking(TObject *Sender)
{
  GBEPlaneExtend1->Amplitude = TrackBarAmplitude->Value;
}
//---------------------------------------------------------------------------
void __fastcall TFormWaves::TrackBarLengthTracking(TObject *Sender)
{
  GBEPlaneExtend1->Longueur = TrackBarLength->Value;
}
//---------------------------------------------------------------------------
void __fastcall TFormWaves::TrackBarSpeedTracking(TObject *Sender)
{
  GBEPlaneExtend1->Vitesse = TrackBarSpeed->Value;
}
//---------------------------------------------------------------------------
void __fastcall TFormWaves::TrackBarOpacityTracking(TObject *Sender)
{
  GBEPlaneExtend1->Opacity = TrackBarOpacity->Value;
}
//---------------------------------------------------------------------------
