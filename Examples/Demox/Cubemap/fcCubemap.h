//---------------------------------------------------------------------------

#ifndef fcCubemapH
#define fcCubemapH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.MaterialSources.hpp>
#include <FMX.Objects3D.hpp>
#include <FMX.Types.hpp>
#include <FMX.Viewport3D.hpp>
#include <System.Math.Vectors.hpp>
#include "GBE.Cubemap.hpp"
//---------------------------------------------------------------------------
class TFormCubemap : public TForm
{
__published:	// IDE-managed Components
	TViewport3D *Viewport3D1;
	TGBECubemap *GBECubemap1;
	TCamera *Camera1;
	TFloatAnimation *FloatAnimation1;
	TTextureMaterialSource *TextureMaterialSource1;
	void __fastcall FloatAnimation1Process(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormCubemap(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormCubemap *FormCubemap;
//---------------------------------------------------------------------------
#endif
