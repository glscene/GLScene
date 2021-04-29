//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.GeomObjects.hpp"
#include "GLS.Graph.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.RenderContextInfo.hpp"
#include <gl\gl.h>



//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *SceneViewer;
	TGLScene *GLScene1;
	TGLCamera *GLCamera;
	TGLDummyCube *GLDummyCube;
	TGLArrowLine *GLArrowLine1;
	TGLLightSource *GLLightSource1;
	TGLDirectOpenGL *DirectOpenGL;
	TGLPoints *GLPoints;
	TGLPlane *GLPlane;
	TGLXYZGrid *GLXYZGrid1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall DirectOpenGLRender(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall SceneViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y);
	void __fastcall SceneViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
		  TPoint &MousePos, bool &Handled);
private:	// User declarations
    int mx, my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
