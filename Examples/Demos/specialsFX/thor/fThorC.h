//---------------------------------------------------------------------------

#ifndef fThorCH
#define fThorCH
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

#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Graph.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLS.SkyDome.hpp"
#include "GLS.ThorFX.hpp"
#include "GLS.SceneViewer.hpp"
#include "Stage.Utils.hpp"
#include "JPeg.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label6;
	TLabel *Label4;
	TLabel *Label3;
	TLabel *Label5;
	TTrackBar *DistanceBar;
	TTrackBar *GSbar;
	TTrackBar *GAbar;
	TTrackBar *WildBar;
	TTrackBar *VibBar;
	TCheckBox *SpinBox;
	TCheckBox *CoreBox;
	TMemo *Memo1;
	TCheckBox *PauseBox;
	TGLScene *GLScene1;
	TGLSkyDome *SkyDome1;
	TGLHeightField *HeightField1;
	TGLDummyCube *Objects;
	TGLCube *TargetCube;
	TGLCube *ThorCube;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TGLThorFXManager *GLThorFXManager1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GSbarChange(TObject *Sender);
	void __fastcall GAbarChange(TObject *Sender);
	void __fastcall WildBarChange(TObject *Sender);
	void __fastcall VibBarChange(TObject *Sender);
	void __fastcall DistanceBarChange(TObject *Sender);
	void __fastcall CoreBoxClick(TObject *Sender);
	void __fastcall GLThorFXManager1CalcPoint(TObject *Sender, int PointNo, float &x,
          float &y, float &z);
	void __fastcall PauseBoxClick(TObject *Sender);
	void __fastcall HeightField1GetHeight(const float x, const float y, float &z, TVector4f &Color,
          TTexPoint &TexPoint);
private:	// User declarations
	int mx, my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
