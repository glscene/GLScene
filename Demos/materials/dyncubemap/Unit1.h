//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H

//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#include <gl.h>
#include <glext.h>

#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.GeomObjects.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SkyDome.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Texture.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *LabelFPS;
	TCheckBox *CBDynamic;
	TGLScene *GLScene1;
	TGLSkyDome *SkyDome1;
	TGLLightSource *GLLightSource1;
	TGLTorus *Torus1;
	TGLCylinder *Cylinder1;
	TGLSphere *Sphere1;
	TGLCube *Cube1;
	TGLTeapot *Teapot1;
	TGLCamera *CubeMapCamera;
	TGLCamera *GLCamera1;
	TGLMemoryViewer *GLMemoryViewer1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	void __fastcall GLSceneViewer1BeforeRender(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);

public:		// Public declarations
	__fastcall TForm1(TComponent* Owner);
private:	// User declarations
	int mx, my;
	bool CubmapSupported;
	bool CubeMapWarnDone;
	void __fastcall GenerateCubeMap();

};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
