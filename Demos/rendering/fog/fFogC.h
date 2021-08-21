//---------------------------------------------------------------------------

#ifndef fFogCH
#define fFogCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "GLS.Cadencer.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.Texture.hpp"
#include "GLS.SceneViewer.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Material.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// published declarations
		TGLSceneViewer *GLSceneViewer1;
		TGLScene *GLScene1;
		TGLDummyCube *GLDummyCube1;
		TGLLightSource *GLLightSource1;
		TGLCamera *GLCamera1;
		TGLCadencer *GLCadencer1;
		TColorDialog *ColorDialog1;
		TGLMaterialLibrary *GLMaterialLibrary1;
	TPanel *Panel1;
	TCheckBox *CBFogEnable;
	TLabel *LFogStart;
	TEdit *EFogStart;
	TLabel *LFogEnd;
	TEdit *EFogEnd;
	TGroupBox *GBTexture;
	TCheckBox *CBTextureEnabled;
	TCheckBox *CBTextureIgnoreFog;
	TShape *SFogColor;
	TLabel *LFogColor;
	TCheckBox *CBApplyToBackground;
	TLabel *LFogDensity;
	TEdit *EFogDensity;
	TRadioGroup *RGFogDistance;
	TRadioGroup *RGFogMode;
        void __fastcall GLSceneViewer1MouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
		void __fastcall GLSceneViewer1MouseMove(TObject *Sender,
          TShiftState Shift, int X, int Y);
        void __fastcall CBFogEnableClick(TObject *Sender);
        void __fastcall EFogStartChange(TObject *Sender);
        void __fastcall SFogColorMouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall RGFogModeClick(TObject *Sender);
        void __fastcall CBApplyToBackgroundClick(TObject *Sender);
        void __fastcall CBTextureEnabledClick(TObject *Sender);
        void __fastcall CBTextureIgnoreFogClick(TObject *Sender);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
private:	// private declarations
		void ApplyFogSettings(void);
		int mx, my;
public:		// public declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
