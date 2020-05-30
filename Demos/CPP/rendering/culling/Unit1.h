//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>

#include "GLCadencer.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLTexture.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLFileMD2.hpp"
#include <Jpeg.hpp>
#include "GLFileMD2.hpp"
#include "GLUtils.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:
        TGLSceneViewer *Viewer;
        TGLScene *GLScene;
        TGLLightSource *GLLightSource1;
        TGLDummyCube *DCTarget;
        TGLCamera *GLCamera1;
        TGLDummyCube *DCSpheres;
        TGLDummyCube *DCActors;
        TGLActor *ACReference;
        TGLCadencer *GLCadencer;
        TTimer *Timer1;
        TGLMaterialLibrary *GLMaterialLibrary;
	TPanel *Panel2;
	TLabel *Label1;
	TRadioButton *RBNone;
	TRadioButton *RBObject;
	TRadioButton *RBHierarchical;
	TLabel *Label2;
	TRadioButton *RBSpheres;
	TRadioButton *RBActors;
	TLabel *LabelFPS;
        void __fastcall RBSpheresClick(TObject *Sender);
        void __fastcall GLCadencerProgress(TObject *Sender,
          const double deltaTime, const double newTime);
        void __fastcall Timer1Timer(TObject *Sender);
        void __fastcall RBNoneClick(TObject *Sender);
private:	// Déclarations de l'utilisateur
public:		// Déclarations de l'utilisateur
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
