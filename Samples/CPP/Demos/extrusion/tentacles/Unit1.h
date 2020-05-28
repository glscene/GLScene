//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLScene.hpp"
#include "GLObjects.hpp"
#include "GLExtrusion.hpp"
#include "GLCadencer.hpp"
#include "GLVectorGeometry.hpp"
#include "GLTexture.hpp"
#include "GLWin32Viewer.hpp"
#include "GLColor.hpp"
#include "GLCrossPlatform.hpp"
#include "GLCoordinates.hpp"
#include "GLBaseClasses.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *PanelFPS;
	TGLScene *GLScene1;
	TGLDummyCube *DCBase;
	TGLSphere *Sphere1;
	TGLPipe *Pipe1;
	TGLPipe *Pipe2;
	TGLPipe *Pipe3;
	TGLPipe *Pipe4;
	TGLPipe *Pipe5;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *DCTarget;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
