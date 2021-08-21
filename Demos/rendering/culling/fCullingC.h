//---------------------------------------------------------------------------

#ifndef fCullingCH
#define fCullingCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>

#include "GLS.Cadencer.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.Texture.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Material.hpp"
#include "GLS.FileMD2.hpp"
#include <Jpeg.hpp>
#include "GLS.FileMD2.hpp"
#include "GLS.Utils.hpp"

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
