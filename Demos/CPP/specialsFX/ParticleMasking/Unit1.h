//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLAsyncTimer.hpp"
#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLEParticleMasksManager.hpp"
#include "GLGeomObjects.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLParticleFX.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"
#include "GLAsyncTimer.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TSplitter *Splitter1;
	TGroupBox *MaskBox;
	TImage *XImage;
	TLabel *XLabel;
	TLabel *YLabel;
	TLabel *ZLabel;
	TImage *YImage;
	TImage *ZImage;
	TButton *Button1;
	TButton *Button2;
	TButton *Button3;
	TGLSceneViewer *SceneViewer;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TEdit *Edit1;
	TEdit *Edit2;
	TEdit *Edit3;
	TEdit *Edit4;
	TEdit *Edit5;
	TCheckBox *CheckBox1;
	TGLScene *GLScene;
	TGLDummyCube *Target;
	TGLPlane *XPlane;
	TGLPlane *YPlane;
	TGLPlane *ZPlane;
	TGLParticleFXRenderer *PFXRenderer;
	TGLSphere *Sphere;
	TGLArrowLine *GLArrowLine1;
	TGLCamera *Camera;
	TGLLightSource *Light;
	TGLCadencer *GLCadencer;
	TGLMaterialLibrary *MatLib;
	TGLWindowsBitmapFont *WinFont;
	TGLPointLightPFXManager *PLManager;
	TGLEParticleMasksManager *GLEParticleMasksManager1;
	TGLAsyncTimer *AsyncTimer1;
	TButton *Button4;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall SceneViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall PLManagerCreateParticle(TObject *Sender, TGLParticle *aParticle);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall Edit2Change(TObject *Sender);
	void __fastcall AsyncTimer1Timer(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall Edit3Change(TObject *Sender);
	void __fastcall Edit4Change(TObject *Sender);
	void __fastcall Edit5Change(TObject *Sender);


private:	// User declarations
	int mx, my;
	int PlaneHeights, PlaneWidths, PlaneDepths, PlaneOffSets;
	void __fastcall RefreshMask();
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
