//---------------------------------------------------------------------------

#ifndef fDuckyCH
#define fDuckyCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <ComCtrls.hpp>
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Objects.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.FileNURBS.hpp"

//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TPanel * Panel1;
  TTrackBar *TrackBar1;
  TLabel *Label1;
  TGLSceneViewer *GLSceneViewer1;
  TGLScene *GLScene1;
  TGLDummyCube *GLDummyCube1;
  TGLCamera *GLCamera1;
  TGLLightSource *GLLightSource1;
  TGLActor *GLActor1;
  TCheckBox *CheckBox1;
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender, TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall TrackBar1Change(TObject * Sender);
  void __fastcall CheckBox1Click(TObject * Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

