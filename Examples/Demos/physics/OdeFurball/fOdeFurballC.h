//---------------------------------------------------------------------------

#ifndef fOdeFurballCH
#define fOdeFurballCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>


#include "Stage.Keyboard.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Cadencer.hpp"

#include "ODE.Import.hpp"
#include "GLS.ODEManager.hpp"
#include <GLS.Navigator.hpp>
#include <GLS.ShadowPlane.hpp>
#include <GLS.Extrusion.hpp>
#include "GLS.Texture.hpp"
#include "GLS.Objects.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.VerletTypes.hpp"


//---------------------------------------------------------------------------
class TFormFurBall : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TCheckBox *CheckBox_LockBall;
	TCheckBox *CheckBox_Inertia;
	TCheckBox *CheckBox_FurGravity;
	TCheckBox *CheckBox_WindResistence;
	TTrackBar *TrackBar_WindForce;
	TCheckBox *CheckBox_Bald;
	TCheckBox *CheckBox_Shadows;
	TGLCadencer *GLCadencer1;
	TGLScene *GLScene1;
	TGLDummyCube *DC_LightHolder;
	TGLLightSource *GLLightSource1;
	TGLSphere *Sphere1;
	TGLDummyCube *DCShadowCaster;
	TGLSphere *FurBall;
	TGLShadowPlane *GLShadowPlane_Floor;
	TGLShadowPlane *GLShadowPlane_Wall;
	TGLShadowPlane *GLShadowPlane_Floor2;
	TGLLines *GLLines1;
	TGLShadowPlane *GLShadowPlane_Wall2;
	TGLShadowPlane *GLShadowPlane_Wall3;
	TGLCamera *GLCamera1;
	TTimer *Timer1;
	TLabel *Label_FPS;
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall CheckBox_FurGravityClick(TObject *Sender);
	void __fastcall CheckBox_WindResistenceClick(TObject *Sender);
	void __fastcall CheckBox_BaldClick(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall CheckBox_ShadowsClick(TObject *Sender);
	void __fastcall TrackBar_WindForceChange(TObject *Sender);
	void __fastcall CheckBox_InertiaClick(TObject *Sender);
	void __fastcall DC_LightHolderProgress(TObject *Sender, const double deltaTime,
          const double newTime);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall FormCreate(TObject *Sender);

private:	// User declarations
  int mX, mY;
public:		// User declarations
	__fastcall TFormFurBall(TComponent* Owner);

  PdxBody odeFurBallBody;
  PdxGeom odeFurBallGeom;

  PdxWorld world;
  PdxSpace space;
  TdJointGroupID contactgroup;

  TGLVerletWorld *VerletWorld;
  TList *HairList;
  TGLVerletFrictionSphere *VCSphere;
  float PhysicsTime;

  TGLVerletGravity *Gravity;
  TGLVerletAirResistance *AirResistance;

  void __fastcall CreateBall();
  void __fastcall CreateFur();
  void __fastcall CreateRandomHair();

};
//---------------------------------------------------------------------------
extern PACKAGE TFormFurBall *FormFurBall;
//---------------------------------------------------------------------------
#endif
