//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <GLS.GeomObjects.hpp>    // Pascal unit
#include <GLS.Cadencer.hpp>       // Pascal unit
#include <GLS.SceneViewer.hpp>    // Pascal unit
#include <GLS.Objects.hpp>        // Pascal unit
#include <GLS.VectorFileObjects.hpp>      // Pascal unit
#include <GLS.Scene.hpp>          // Pascal unit
#include <ExtCtrls.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"
         // Pascal unit
#include "GLS.Utils.hp
#include "GLS.Scene.hpp"p"         // Pascal unit

//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLScene * GLScene1;
  TGLLightSource *GLLightSource1;
  TGLDummyCube *DummyCube1;
  TGLFreeForm *FreeForm1;
  TGLSphere *Sphere1;
  TGLArrowLine *ArrowLine1;
  TGLSceneViewer *GLSceneViewer2;
  TGLCamera *GLCamera2;
  TGLCadencer *GLCadencer1;
  TTimer *Timer1;
  TPanel *Panel1;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label5;
  TLabel *LABuild;
  TCheckBox *CheckBox1;
  TCheckBox *CBOctree;
  TLabel *Label4;
	TLabel *LabelFPS;
  void __fastcall GLSceneViewer2MouseDown(TObject * Sender, TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer2MouseMove(TObject * Sender, TShiftState Shift,
                                          int X, int Y);
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
                                      const double newTime);
  void __fastcall Timer1Timer(TObject * Sender);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);

  int mousex, mousey;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 
