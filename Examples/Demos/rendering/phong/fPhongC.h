//---------------------------------------------------------------------------

#ifndef fPhongCH
#define fPhongCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "GLS.AsyncTimer.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Texture.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLSL.CustomShader.hpp"
#include "GLS.Material.hpp"
#include "GLSL.AsmShader.hpp"
#include "GLSL.PhongShader.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// Composants gérés par l'EDI
        TGLSceneViewer *GLSceneViewer1;
        TCheckBox *CheckBox1;
        TGLScene *GLScene1;
        TGLDummyCube *GLDummyCube1;
        TGLCamera *GLCamera1;
        TGLLightSource *GLLightSource1;
        TGLTeapot *GLTeapot1;
        TGLMaterialLibrary *GLMaterialLibrary1;
        TGLPhongShader *GLPhongShader1;
        TGLCadencer *GLCadencer1;
		TGLAsyncTimer *AsyncTimer1;
        void __fastcall GLSceneViewer1MouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall GLSceneViewer1MouseMove(TObject *Sender,
          TShiftState Shift, int X, int Y);
        void __fastcall AsyncTimer1Timer(TObject *Sender);
        void __fastcall CheckBox1Click(TObject *Sender);
        void __fastcall GLCadencer1Progress(TObject *Sender,
          const double deltaTime, const double newTime);
private:	// Déclarations de l'utilisateur
public:		// Déclarations de l'utilisateur
        __fastcall TForm1(TComponent* Owner);
        int mx, my;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
