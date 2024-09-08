//---------------------------------------------------------------------------

#ifndef fcCylinderExtH
#define fcCylinderExtH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include "GBE.CylinderExtend.hpp"
#include <FMX.Ani.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.MaterialSources.hpp>
#include <FMX.Objects3D.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.Viewport3D.hpp>
#include <System.Math.Vectors.hpp>
//---------------------------------------------------------------------------
class TFormCylinderExt : public TForm
{
__published:	// IDE-managed Components
	TViewport3D *Viewport3D1;
	TLight *Light1;
	TFloatAnimation *FloatAnimation2;
	TLight *Light2;
	TGBECylinderExtend *GBECylinderExtend1;
	TFloatAnimation *FloatAnimation1;
	TLightMaterialSource *LightMaterialSource3;
	TLightMaterialSource *LightMaterialSource1;
	TLightMaterialSource *LightMaterialSource2;
	TLightMaterialSource *LightMaterialSource4;
private:	// User declarations
public:		// User declarations
	__fastcall TFormCylinderExt(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormCylinderExt *FormCylinderExt;
//---------------------------------------------------------------------------
#endif
