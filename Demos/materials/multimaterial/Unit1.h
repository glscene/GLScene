//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Imaging.Jpeg.hpp>

#include "GLS.Scene.hpp"
#include "GLS.Objects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Texture.hpp"
#include "GLS.VectorGeometry.hpp"
#include "GLS.Cadencer.hpp"
#include "GLSL.MultiMaterialShader.hpp"
#include "GLSL.TextureShaders.hpp"
#include "GLS.Material.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.BaseClasses.hpp"
#include "GLS.Utils.hpp"
#include "GLSL.MultiMaterialShader.hpp"
#include "GLSL.TextureShaders.hpp"

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
