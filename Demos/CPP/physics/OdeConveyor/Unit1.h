//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLObjects.hpp"
#include "GLODEManager.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLWin32Viewer.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *FrictionFeedback;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label6;
	TLabel *NormZ;
	TLabel *NormY;
	TLabel *NormX;
	TTrackBar *TrackBarMotionSpeed;
	TEdit *Friction;
	TEdit *FDirX;
	TEdit *FDirY;
	TEdit *FDirZ;
	TButton *AddODECube;
	TGLScene *GLScene1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLCube *ConveyorBelt1;
	TGLCube *GLCube1;
	TGLRenderPoint *GLRenderPoint1;
	TGLDummyCube *SpawnPoint;
	TGLCadencer *GLCadencer1;
	TGLODEManager *GLODEManager1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLODEManager1Collision(TObject *Sender, TObject *Object1, TObject *Object2,
          TdContact &Contact, bool &HandleCollision);
	void __fastcall TrackBarMotionSpeedChange(TObject *Sender);
	void __fastcall AddODECubeClick(TObject *Sender);
	void __fastcall FrictionChange(TObject *Sender);

private:	// User declarations
	Glvectorgeometry::TVector FUserDirection;
	Glvectorgeometry::TVector FDirectionVector;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
