//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Imaging.Jpeg.hpp>

#include "GLScene.hpp"
#include "GLObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLTexture.hpp"
#include "GLVectorGeometry.hpp"
#include "GLCadencer.hpp"
#include "GLMultiMaterialShader.hpp"
#include "GLTexCombineShader.hpp"
#include "GLMaterial.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLBaseClasses.hpp"
#include "GLUtils.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLCube *GLCube1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLMaterialLibrary *GLMaterialLibrary2;
	TGLMultiMaterialShader *GLMultiMaterialShader1;
	TGLCadencer *GLCadencer1;
	TGLTexCombineShader *GLTexCombineShader1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);

private:	// User declarations
public:		// User declarations
   int mx,my;
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
