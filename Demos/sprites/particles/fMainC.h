//---------------------------------------------------------------------------

#ifndef fMainCH
#define fMainCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Behaviours.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.Particles.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Scene.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.SceneViewer.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.SceneViewer.hpp"
#include <System.Classes.hpp>
//---------------------------------------------------------------------------
class TFormStars:public TForm
{
__published:                   // IDE-managed Components
  TGLSceneViewer * GLSceneViewer1;
  TGLScene *GLScene1;
  TGLCamera *GLCamera1;
  TGLParticles *GLParticles1;
  TGLSprite *Sprite1;
  TGLCadencer *GLCadencer1;
  TTimer *Timer1;
  void __fastcall GLParticles1ActivateParticle(TObject * Sender,
                                               TGLBaseSceneObject * particle);
  void __fastcall Sprite1Progress(TObject * Sender, const double deltaTime,
								  const double newTime);
  void __fastcall Timer1Timer(TObject * Sender);
  void __fastcall FormResize(TObject * Sender);
private:                       // User declarations
public:                        // User declarations
    __fastcall TFormStars(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormStars *FormStars;
//---------------------------------------------------------------------------
#endif
 
