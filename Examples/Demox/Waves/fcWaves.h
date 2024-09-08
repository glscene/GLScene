//---------------------------------------------------------------------------

#ifndef fcWavesH
#define fcWavesH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include "GBE.PlaneExtend.hpp"
#include <FMX.Ani.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.Edit.hpp>
#include <FMX.EditBox.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.MaterialSources.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Objects3D.hpp>
#include <FMX.SpinBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.Viewport3D.hpp>
#include <System.Math.Vectors.hpp>
//---------------------------------------------------------------------------
class TFormWaves : public TForm
{
__published:	// IDE-managed Components
	TViewport3D *Viewport3D1;
	TFloatAnimation *FloatAnimation1;
	TLayout *Layout1;
	TRectangle *Rectangle1;
	TGroupBox *GroupBoxOrigin;
	TLabel *Label4;
	TSpinBox *SpinBoxX;
	TLabel *Label5;
	TSpinBox *SpinBoxY;
	TLabel *Label6;
	TSpinBox *SpinBoxZ;
	TLabel *LabelLines;
	TSwitch *SwitchLines;
	TLabel *Label8;
	TTrackBar *TrackBarOpacity;
	TTrackBar *TrackBarAmplitude;
	TLabel *Label1;
	TLabel *Label2;
	TTrackBar *TrackBarLength;
	TLabel *Label3;
	TTrackBar *TrackBarSpeed;
	TLight *Light1;
	TGBEPlaneExtend *GBEPlaneExtend1;
	TColorMaterialSource *ColorMaterialSource1;
	TLightMaterialSource *LightMaterialSource1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FloatAnimation1Process(TObject *Sender);
	void __fastcall TrackBarOpacityTracking(TObject *Sender);
	void __fastcall TrackBarAmplitudeTracking(TObject *Sender);
	void __fastcall TrackBarLengthTracking(TObject *Sender);
	void __fastcall TrackBarSpeedTracking(TObject *Sender);
	void __fastcall SpinBoxChange(TObject *Sender);
	void __fastcall SwitchLinesSwitch(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormWaves(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormWaves *FormWaves;
//---------------------------------------------------------------------------
#endif
