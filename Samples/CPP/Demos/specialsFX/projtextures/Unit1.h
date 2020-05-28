//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Forms.hpp>

#include "GLCadencer.hpp"
#include "GLObjects.hpp"
#include "GLProjectedTextures.hpp"
#include "GLScene.hpp"
#include "GLTexture.hpp"
#include "GLWin32Viewer.hpp"
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLFileTGA.hpp"



//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TGLSceneViewer *viewer;
        TGLScene *scene;
        TGLProjectedTextures *ProjLight;
        TGLDummyCube *scenery;
        TGLCube *GLCube2;
        TGLDummyCube *light2;
        TGLSphere *GLSphere3;
        TGLTextureEmitter *emitter2;
        TGLDummyCube *Light;
        TGLLightSource *GLLightSource1;
        TGLSphere *GLSphere2;
        TGLTextureEmitter *emitter1;
        TGLCube *GLCube1;
        TGLPlane *GLPlane1;
        TGLPlane *GLPlane2;
		TGLPlane *GLPlane3;
        TGLSphere *GLSphere1;
	TGLCamera *GLCamera1;
		TGLCadencer *GLCadencer1;
        TGLMaterialLibrary *matLib;
        TTimer *Timer1;
        void __fastcall GLCadencer1Progress(TObject *Sender,
          const double deltaTime, const double newTime);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall Timer1Timer(TObject *Sender);
        void __fastcall viewerMouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall viewerMouseUp(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
        void __fastcall viewerMouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y);
        void __fastcall FormKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
