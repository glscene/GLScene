//---------------------------------------------------------------------------

#ifndef fcCubeH
#define fcCubeH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.MaterialSources.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Objects3D.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.Viewport3D.hpp>
#include <System.Math.Vectors.hpp>
#include "GBE.CubeExtend.hpp"
//---------------------------------------------------------------------------
class TFormCube : public TForm
{
__published:	// IDE-managed Components
	TViewport3D *Viewport3D1;
	TLight *Light1;
	TLight *Light2;
	TLight *Light3;
	TGBECubeExtend *GBECubeExtend1;
	TFloatAnimation *FloatAnimation1;
	TLayout *Layout1;
	TRectangle *Rectangle1;
	TCheckBox *CheckBoxFront;
	TCheckBox *CheckBoxRight;
	TCheckBox *CheckBoxBack;
	TCheckBox *CheckBoxLeft;
	TCheckBox *CheckBoxTop;
	TCheckBox *CheckBoxBottom;
	TLightMaterialSource *LightMaterialSource1;
	TLightMaterialSource *LightMaterialSource2;
	TLightMaterialSource *LightMaterialSource3;
	TLightMaterialSource *LightMaterialSource4;
	TLightMaterialSource *LightMaterialSource5;
	TLightMaterialSource *LightMaterialSource6;
	void __fastcall CheckBoxFrontChange(TObject *Sender);
	void __fastcall CheckBoxRightChange(TObject *Sender);
	void __fastcall CheckBoxBackChange(TObject *Sender);
	void __fastcall CheckBoxLeftChange(TObject *Sender);
	void __fastcall CheckBoxBottomChange(TObject *Sender);
	void __fastcall FloatAnimation1Process(TObject *Sender);
	void __fastcall CheckBoxTopChange(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormCube(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormCube *FormCube;
//---------------------------------------------------------------------------
#endif
