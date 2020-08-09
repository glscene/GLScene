//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <GLS.SceneViewer.hpp>    // Pascal unit
#include <GLS.VectorGeometry.hpp>   // Pascal unit
#include <GLS.Behaviours.hpp>     // Pascal unit
#include <GLS.Cadencer.hpp>       // Pascal unit
#include <GLS.Particles.hpp>      // Pascal unit
#include <GLS.Objects.hpp>        // Pascal unit
#include <GLS.Scene.hpp>
#include <ExtCtrls.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.SceneViewer.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.SceneViewer.hpp"         // Pascal unit
//---------------------------------------------------------------------------
class TForm1:public TForm
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
    __fastcall TForm1(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 
