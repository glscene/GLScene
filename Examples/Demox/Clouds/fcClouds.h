//---------------------------------------------------------------------------

#ifndef fcCloudsH
#define fcCloudsH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include "GBE.Clouds.hpp"
#include <FMX.Ani.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.Edit.hpp>
#include <FMX.EditBox.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.MaterialSources.hpp>
#include <FMX.Objects3D.hpp>
#include <FMX.SpinBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Viewport3D.hpp>
#include <System.Math.Vectors.hpp>
//---------------------------------------------------------------------------
class TFormClouds : public TForm
{
__published:	// IDE-managed Components
	TViewport3D *Viewport3D1;
	TGBEClouds *GBEClouds1;
	TTextureMaterialSource *TextureMaterialSource2;
	TTextureMaterialSource *TextureMaterialSource3;
	TLayout *Layout1;
	TArcDial *ArcDial1;
	TSpinBox *SpinBox1;
	TSpinBox *SpinBox2;
	TCamera *Camera1;
	TTextureMaterialSource *TextureMaterialSource1;
	TTextureMaterialSource *TextureMaterialSource4;
	TTextureMaterialSource *TextureMaterialSource5;
	TFloatAnimation *FloatAnimation1;
	void __fastcall FloatAnimation1Process(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ArcDial1Change(TObject *Sender);
	void __fastcall SpinBox1Change(TObject *Sender);
	void __fastcall SpinBox2Change(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormClouds(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormClouds *FormClouds;
//---------------------------------------------------------------------------
#endif
