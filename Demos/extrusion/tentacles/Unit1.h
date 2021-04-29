//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLS.Scene.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Extrusion.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.VectorGeometry.hpp"
#include "GLS.Texture.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Color.hpp"

#include "GLS.Coordinates.hpp"
#include "GLS.BaseClasses.hpp"
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
