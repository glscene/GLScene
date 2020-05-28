//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLScene.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLObjects.hpp"
#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLHUDObjects.hpp"
#include "GLMaterial.hpp"
#include "GLUserShader.hpp"
#include "GLWindowsFont.hpp"
#include "GLRenderContextInfo.hpp"
#include "GLGraphics.hpp"
#include "GLState.hpp"
#include "GLTextureFormat.hpp"

//---------------------------------------------------------------------------

typedef
   // Structures used in our binary file
   // The structure is quite simplified here, original data came from a FEM
   // package and was in (huge) text files, and parsing text files is not the
   // purpose of this demo, so data was simplified ;)

struct TDataNode
{
  float X;
  float Y;
  float Z;
  float Intensity;
} * DataNodes;

struct TDataPrimitive {
  Word Node1;
  Word Node2;
  Word Node3;
  Word Node4;  // if Node4 is OxFFFF, codes a triangle
} * DataPrimitives;
//---------------------------------------------------------------------------

class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TCheckBox *CBWireFrame;
	TCheckBox *CBSmooth;
	TTrackBar *TBScale;
	TGLScene *GLScene1;
	TGLFreeForm *GLFreeForm;
	TGLDummyCube *DCTarget;
	TGLCamera *GLCamera;
	TGLHUDSprite *HSPalette;
	TGLHUDText *HTPaletteLeft;
	TGLHUDText *HTPaletteRight;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLUserShader *GLUserShader;
	TGLWindowsBitmapFont *GLWindowsBitmapFont;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall TBScaleChange(TObject *Sender);
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
