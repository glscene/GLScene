//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <tchar.h>

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
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSLProjectedTextures.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLSLProjectedTextures *GLSLProjectedTextures1;
	TGLFreeForm *GLFreeForm1;
	TGLCube *GLCube1;
	TGLDummyCube *GLDummyCube3;
	TGLSLTextureEmitter *GLSLTextureEmitter2;
	TGLArrowLine *GLArrowLine1;
	TGLSLTextureEmitter *GLSLTextureEmitter1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource2;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLCamera1CustomPerspective(const TRectangle &viewport, int width,
          int height, int DPI, float &viewPortRadius);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
		  TPoint &MousePos, bool &Handled);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);

private:	// User declarations
	int mx, my;
	int sdir;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
