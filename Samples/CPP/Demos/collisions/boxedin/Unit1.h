//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include "GLVectorGeometry.hpp"
#include "GLScene.hpp"
#include "GLBaseClasses.hpp"
#include <GLGeomObjects.hpp>
#include <GLNavigator.hpp>
#include <GLCadencer.hpp>
#include <GLWin32Viewer.hpp>
#include <GLObjects.hpp>
#include <GLVectorFileObjects.hpp>
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLUtils.hpp"

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
  TGLDummyCube *DummyCube2;
  TGLSphere *Sphere2;
  TGLLightSource *GLLightSource2;
  TPanel *Panel1;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TTrackBar *TrackBar1;
  TButton *Button1;
  TGLLines *Lines1;
	TLabel *LabelFPS;
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
                                      const double newTime);
  void __fastcall Timer1Timer(TObject * Sender);
  void __fastcall Button1Click(TObject * Sender);
private:                       // User declarations
  float colTotalTime;           // for timing collision detection
  int colCount;
  void AddToTrail(const Glvectorgeometry::TVector & p);
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);
  int mousex, mousey;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 
