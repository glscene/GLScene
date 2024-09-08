//---------------------------------------------------------------------------

#ifndef fcGrassH
#define fcGrassH
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
//---------------------------------------------------------------------------
class TFormGrass : public TForm
{
__published:	// IDE-managed Components
	TViewport3D *Viewport3D1;
	TTextureMaterialSource *TextureMaterialSource;
	TTextureMaterialSource *TextureMaterialSource1;
	TTextureMaterialSource *TextureMaterialSource2;
	TDummy *Dummy;
	TFloatAnimation *FloatAnimation2;
	TFloatAnimation *FloatAnimation3;
	TTextureMaterialSource *TextureMaterialSource3;
	TTextureMaterialSource *TextureMaterialSource4;
	TTextureMaterialSource *TextureMaterialSource5;
	TFloatAnimation *FloatAnimation1;
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormGrass(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormGrass *FormGrass;
//---------------------------------------------------------------------------
#endif
