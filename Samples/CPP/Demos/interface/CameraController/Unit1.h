//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCameraController.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLGraph.hpp"
#include "GLMaterial.hpp"
#include "GLNavigator.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSmoothNavigator.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TPanel *Panel2;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label5;
	TEdit *eDestX;
	TEdit *eDestY;
	TEdit *eDestZ;
	TPanel *Panel3;
	TLabel *Label6;
	TLabel *Label9;
	TEdit *eDistance;
	TPanel *Panel4;
	TLabel *Label7;
	TLabel *Label8;
	TLabel *Label10;
	TLabel *Label11;
	TLabel *Label12;
	TEdit *eSafeDistance;
	TEdit *eTimeToSafePlacement;
	TEdit *eTimeToOrbit;
	TEdit *eTimeToZoomBackIn;
	TPanel *Panel5;
	TLabel *Label13;
	TLabel *Label14;
	TEdit *eTime;
	TButton *btnMoveToPos;
	TButton *btnZoomToDistance;
	TButton *btnOrbitToPos;
	TButton *btnSafeOrbitAndZoomToPos;
	TButton *btnOrbitToPosAdv;
	TPanel *Panel8;
	TLabel *Label20;
	TPanel *Panel7;
	TLabel *Label16;
	TLabel *Label17;
	TLabel *Label18;
	TLabel *Label19;
	TEdit *camDirY;
	TEdit *camDirZ;
	TPanel *Panel9;
	TLabel *Label21;
	TLabel *Label22;
	TLabel *Label23;
	TLabel *Label24;
	TEdit *camUpX;
	TEdit *camUpY;
	TEdit *camUpZ;
	TCheckBox *UpAxis;
	TCheckBox *cbMoveParent;
	TButton *btSmoothOrbit;
	TButton *btSmoothOrbitToPosAdv;
	TButton *btSmoothOrbitAndZoom;
	TGLSceneViewer *GLSceneViewer1;
	TPanel *pImg;
	TImage *Image1;
	TLabel *Label4;
	TPanel *Panel6;
	TLabel *Label15;
	TGLScene *GLScene1;
	TGLDummyCube *dcMovingParent;
	TGLCamera *GLCamera;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *dcSphere;
	TGLSphere *GLSphere1;
	TGLCylinder *GLCylinder1;
	TGLSphere *GLSphere2;
	TGLSphere *GLSphere3;
	TGLDummyCube *dcDebugGUI;
	TGLArrowLine *ArrowLine;
	TGLXYZGrid *XYZGrid;
	TGLPlane *GLPlane1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLCameraController *GLCameraController1;
	TGLSmoothNavigator *GLSmoothNavigator;
	TEdit *camDirX;
	void __fastcall btnMoveToPosClick(TObject *Sender);
	void __fastcall btnZoomToDistanceClick(TObject *Sender);
	void __fastcall btnOrbitToPosClick(TObject *Sender);
	void __fastcall btnSafeOrbitAndZoomToPosClick(TObject *Sender);
	void __fastcall btSmoothOrbitClick(TObject *Sender);
	void __fastcall btSmoothOrbitAndZoomClick(TObject *Sender);
	void __fastcall btSmoothOrbitToPosAdvClick(TObject *Sender);
	void __fastcall btnOrbitToPosAdvClick(TObject *Sender);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
		  TPoint &MousePos, bool &Handled);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y);
	void __fastcall GLSceneViewer1MouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall FormCreate(TObject *Sender);

private:	// User declarations
	int mx, my;
	double DextX, DextY, DextZ, Time, ZoomDistance;
	TGLCameraController *FGLCameraController;
	TGLNavigatorSmoothChangeVector *FCameraSmoothAnimator_AbsPos;
	TGLNavigatorSmoothChangeVector *FCameraSmoothAnimator_RelPos;
	void __fastcall GetInput(TButton *Sender);
	Glvectortypes::TVector4f __fastcall (__closure *TGLNavigatorSmoothChangeVectorGetEvent)
		(TGLNavigatorSmoothChangeVector* const ASender);
	Glvectorgeometry::TVector __fastcall OnGetCameraPosition(
					   TGLNavigatorSmoothChangeVector* const ASender);
	void __fastcall OnSetCameraPosition(TGLNavigatorSmoothChangeVector* const ASender,
					   const Glvectortypes::TVector4f &AValue);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
