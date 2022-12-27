//---------------------------------------------------------------------------

#ifndef fPanoViewerCH
#define fPanoViewerCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ExtDlgs.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <Forms.hpp>
#include <Vcl.Imaging.jpeg.hpp>

#include "GLS.Cadencer.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.Texture.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.Material.hpp"

#include "GLS.KeyBoard.hpp"
#include "GLS.FileJPEG.hpp"
#include "GLS.Utils.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
  __published: // IDE-managed Components
    TGLSceneViewer* GLSceneViewer1;
    TPanel* Panel1;
    TLabel* LabelYaw;
    TLabel* LabelPitch;
    TLabel* Label1;
    TLabel* Label2;
    TButton* BtnLoad;
    TTrackBar* TrackBar1;
    TGLScene* GLScene1;
    TGLSphere* Sphere1;
    TGLCamera* GLCamera1;
    TOpenPictureDialog* OpenPictureDialog1;
    TGLMaterialLibrary* GLMaterialLibrary1;
    TGLCadencer* GLCadencer1;
    void __fastcall GLSceneViewer1MouseDown(
        TObject* Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
    void __fastcall GLSceneViewer1MouseMove(
        TObject* Sender, TShiftState Shift, int X, int Y);
    void __fastcall BtnLoadClick(TObject* Sender);
    void __fastcall TrackBar1Change(TObject* Sender);
    void __fastcall GLCadencer1Progress(
        TObject* Sender, const double deltaTime, const double newTime);
    void __fastcall FormKeyDown(TObject* Sender, WORD &Key, TShiftState Shift);
    void __fastcall FormMouseWheel(TObject* Sender, TShiftState Shift,
        int WheelDelta, TPoint &MousePos, bool &Handled);
  private: // User declarations
    int mx, my;
    float pitch, yaw; // in degree
    void PanCameraAround(float dx, float dy);
  public: // User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1* Form1;
//---------------------------------------------------------------------------
#endif

