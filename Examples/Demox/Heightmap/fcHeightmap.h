//---------------------------------------------------------------------------

#ifndef fcHeightmapH
#define fcHeightmapH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include "GBE.Heightmap.hpp"
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
class TFormHeightmap : public TForm
{
__published:	// IDE-managed Components
	TViewport3D *Viewport3D1;
	TGBEHeightmap *GBEHeightmap1;
	TFloatAnimation *FloatAnimation2;
	TCylinder *Cylinder1;
	TCylinder *Cylinder2;
	TFloatAnimation *FloatAnimation1;
	TFloatAnimation *FloatAnimation3;
	TLayout *Layout1;
	TRectangle *Rectangle1;
	TLabel *LabelLines;
	TLabel *Label2;
	TSwitch *SwitchToLines;
	TSwitch *SwitchToRamp;
	TLabel *LabelBlur;
	TSpinBox *SpinBoxBlur;
	TImage *Image1;
	TLight *Light1;
	TColorMaterialSource *ColorMaterialSource1;
	TLightMaterialSource *LightMaterialSource1;
	TLightMaterialSource *LightMaterialSource3;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FloatAnimation2Process(TObject *Sender);
	void __fastcall SwitchToLinesSwitch(TObject *Sender);
	void __fastcall SwitchToRampSwitch(TObject *Sender);
	void __fastcall SpinBoxBlurChange(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormHeightmap(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormHeightmap *FormHeightmap;
//---------------------------------------------------------------------------
#endif
