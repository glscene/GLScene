//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "GLGeomObjects.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLTexture.hpp"
#include "GLWin32Viewer.hpp"
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TGLSceneViewer *GLSceneViewer1;
        TButton *BUBind;
        TGLScene *GLScene1;
        TGLLightSource *GLLightSource1;
        TGLTorus *Torus1;
        TGLSphere *Sphere1;
        TGLAnnulus *GLAnnulus1;
        TGLAnnulus *GLAnnulus2;
        TGLCube *GLCube1;
        TGLSphere *GLSphere1;
        TGLCamera *GLCamera1;
        TGLMaterialLibrary *GLMaterialLibrary1;
        void __fastcall BUBindClick(TObject *Sender);
        void __fastcall GLSceneViewer1MouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall GLSceneViewer1MouseMove(TObject *Sender,
          TShiftState Shift, int X, int Y);
private:	// User declarations
        int mx, my;
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
class THiddenLineShader : public TGLShader
{
private:

public:
	__fastcall virtual THiddenLineShader(TComponent* AOwner);
	__fastcall virtual ~THiddenLineShader(void);

        TColorVector BackgroundColor, LineColor;
        int PassCount;

	void __fastcall DoApply(TGLRenderContextInfo &rci, System::TObject* Sender);
	bool __fastcall DoUnApply(TGLRenderContextInfo &rci);
};

class TOutLineShader : public TGLShader
{
private:

public:
	__fastcall virtual TOutLineShader(TComponent* AOwner);
	__fastcall virtual ~TOutLineShader(void);
        
        TColorVector BackgroundColor, LineColor;
        bool OutlineSmooth, Lighting;
        float OutlineWidth, OldlineWidth;
        int PassCount;

	void __fastcall DoApply(TGLRenderContextInfo &rci, System::TObject* Sender);
	bool __fastcall DoUnApply(TGLRenderContextInfo &rci);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
