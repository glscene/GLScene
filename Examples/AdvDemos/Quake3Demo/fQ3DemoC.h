//---------------------------------------------------------------------------

#ifndef fQ3DemoCH
#define fQ3DemoCH
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.Jpeg.hpp>

#include "GLS.Scene.hpp"
#include "GLS.Objects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.ShadowPlane.hpp"
#include "GLS.VectorGeometry.hpp"
#include "GLS.Texture.hpp"
#include "GLS.ParticleFX.hpp"
#include "GLS.Material.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.BaseClasses.hpp"
#include "GLS.FileQ3MD3.hpp"



//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TComboBox *ComboBox1;
	TComboBox *ComboBox2;
	TTrackBar *TrackBar1;
	TTrackBar *TrackBar2;
	TTrackBar *TrackBar3;
	TTrackBar *TrackBar4;
	TComboBox *ComboSkin;
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLDummyCube *ModelCube;
	TGLActor *Legs;
	TGLActor *Torso;
	TGLActor *Head;
	TGLActor *Weapon;
	TGLDummyCube *GunSmoke;
	TGLShadowPlane *GLShadowPlane1;
	TGLParticleFXRenderer *GLParticleFXRenderer1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLMaterialLibrary *MatLib;
	TGLPointLightPFXManager *GLPointLightPFXManager1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ComboBox1Change(TObject *Sender);
	void __fastcall ComboBox2Change(TObject *Sender);
	void __fastcall ComboSkinChange(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormDestroy(TObject *Sender);

private:	// User declarations
	int mx, my;
	int Idx;
	TMD3TagList *LegsTags;
	TMD3TagList *TorsoTags;
	TMD3TagList *WeaponTags;
	void __fastcall BuildModel();
	TGLMatrix __fastcall InterpolateMatrix(
	       TGLMatrix m1, TGLMatrix m2, Single Delta);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
