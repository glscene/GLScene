//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLFeedback.hpp"
#include "GLObjects.hpp"
#include "GLPolyhedron.hpp"
#include "GLScene.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLMesh.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TButton *Button1;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLFreeForm *GLFreeForm1;
	TGLFeedback *GLFeedback1;
	TGLDummyCube *MeshObject1;
	TGLCube *GLCube1;
	TGLDodecahedron *GLDodecahedron1;
	TGLDummyCube *MeshObject2;
	TGLSphere *GLSphere1;
	TGLMesh *GLMesh1;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y);
private:	// User declarations
    int mx, my;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
