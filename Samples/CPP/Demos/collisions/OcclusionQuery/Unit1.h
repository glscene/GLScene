//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "OpenGL1x.hpp"
#include "GLContext.hpp"
#include "GLRenderContextInfo.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *LabelFPS;
	TCheckBox *CheckBox1;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLCube *GLCube1;
	TGLCylinder *GLCylinder1;
	TGLDummyCube *GLDummyCube2;
	TGLCube *GLCube2;
	TGLDirectOpenGL *OGLBeginQueries;
	TGLDummyCube *dcTestObjects;
	TGLTorus *GLTorus1;
	TGLCone *GLCone1;
	TGLDirectOpenGL *OGLEndQueries;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1BeforeRender(TObject *Sender);
	void __fastcall OGLBeginQueriesRender(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall OGLEndQueriesRender(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall Timer1Timer(TObject *Sender);


private:	// User declarations
   TGLTimerQueryHandle *TimerQuery;
   TGLOcclusionQueryHandle *OcclusionQuery;
   TGLBooleanOcclusionQueryHandle *bOcclusionQuery;

   bool queriesCreated;
   bool timerQuerySupported;
   int timeTaken; // in nanoseconds
   int samplesPassed;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
