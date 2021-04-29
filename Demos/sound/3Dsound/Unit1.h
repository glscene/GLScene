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

#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.GeomObjects.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "Sounds.BASS.hpp"
#include "Sounds.FMOD.hpp"
#include "Sounds.OpenAL.hpp"
#include "GLS.Sound.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Utils.hpp"
#include "GLS.FileWAV.hpp"
#include "GLS.FileMP3.hpp"
#include "Imports.BASS.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer;
	TTrackBar *TrackBar;
	TTrackBar *TrackBar1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *LabelFPS;
	TRadioButton *RBBass;
	TRadioButton *RBFMOD;
	TButton *Button1;
	TButton *btnHowl;
	TRadioButton *RBOpenAL;
	TGLSMFMOD *GLSMFMOD;
	TGLSMBASS *GLSMBASS;
	TGLSMOpenAL *GLSMOpenAL;
	TGLSoundLibrary *GLSoundLibrary;
	TGLScene *GLScene;
	TGLDummyCube *DummyCube;
	TGLTorus *Torus1;
	TGLSphere *Mickey;
	TGLSphere *Sphere2;
	TGLSphere *Sphere3;
	TGLCone *Cone1;
	TGLPlane *Plane1;
	TGLSphere *Sphere;
	TGLDisk *Disk1;
	TGLLightSource *GLLightSource;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall TrackBar1Change(TObject *Sender);
	void __fastcall SphereProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall TrackBarChange(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall btnHowlClick(TObject *Sender);
	void __fastcall TimerTimer(TObject *Sender);
	void __fastcall RBFMODClick(TObject *Sender);
	void __fastcall GLSceneViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLSceneViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);

private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
