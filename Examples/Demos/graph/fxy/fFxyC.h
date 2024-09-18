//---------------------------------------------------------------------------

#ifndef fFxyCH
#define fFxyCH
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Graph.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <System.ImageList.hpp>
#include <Vcl.ImgList.hpp>

//---------------------------------------------------------------------------
class TFormFxy : public TForm
{
  __published: // IDE-managed Components
    TGLScene* GLScene1;
    TGLSceneViewer* Viewer;
    TGLCamera* Camera;
	TGLLightSource *LightSource;
	TGLHeightField *HeightField;
    TGLXYZGrid* XYGrid;
    TGLXYZGrid* XZGrid;
    TGLXYZGrid* YZGrid;
    TPanel* Panel1;
	TTrackBar *TrackBarY;
	TTrackBar *TrackBarX;
	TTrackBar *TrackBarZ;
	TLabel *LabelX;
	TCheckBox *chbCenter;
	TRadioGroup *rgFormula;
	TRadioGroup *rgPolygonMode;
	TLabel *LabelY;
	TLabel *LabelZ;
    void __fastcall chbCenterClick(TObject* Sender);
    void __fastcall TrackBarYChange(TObject* Sender);
    void __fastcall ViewerMouseDown(
        TObject* Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
    void __fastcall ViewerMouseMove(
        TObject* Sender, TShiftState Shift, int X, int Y);
    void __fastcall TrackBarXChange(TObject* Sender);
    void __fastcall TrackBarZChange(TObject* Sender);
    void __fastcall FormMouseWheel(TObject* Sender, TShiftState Shift,
        int WheelDelta, TPoint &MousePos, bool &Handled);
    void __fastcall rgFormulaClick(TObject* Sender);
    void __fastcall FormCreate(TObject* Sender);
	void __fastcall rgPolygonModeClick(TObject *Sender);
  private: // User declarations
    int mx, my;
    void __fastcall Formula0(const float x, const float y, float &z,
        TVector4f &color, TTexPoint &texPoint);
    void __fastcall Formula1(const float x, const float y, float &z,
        TVector4f &color, TTexPoint &texPoint);
    void __fastcall Formula2(const float x, const float y, float &z,
        TVector4f &color, TTexPoint &texPoint);
	void __fastcall Formula3(const float x, const float y, float &z,
        TVector4f &color, TTexPoint &texPoint);
  public: // User declarations
    __fastcall TFormFxy(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormFxy* FormFxy;
//---------------------------------------------------------------------------
#endif

