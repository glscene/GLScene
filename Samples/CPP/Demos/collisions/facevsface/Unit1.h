//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Grids.hpp>
#include "GLBaseClasses.hpp"
#include "GLCollision.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSpaceText.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLUtils.hpp"
#include "GLFile3DS.hpp"


//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TSplitter *Splitter1;
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TShape *Shape1;
	TLabel *Label1;
	TLabel *LATime;
	TLabel *Label2;
	TRadioGroup *cbCollisionMode;
	TPanel *Panel2;
	TStringGrid *StringGrid1;
	TMemo *Memo1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLLightSource *GLLightSource2;
	TGLDummyCube *DummyCube1;
	TGLCamera *GLCamera2;
	TGLSpaceText *txtX;
	TGLSpaceText *txtY;
	TGLSpaceText *txtZ;
	TGLFreeForm *TeaPot1;
	TGLFreeForm *TeaPot2;
	TGLCube *CubePoint1;
	TGLCube *CubePoint2;
	TGLCube *Cube2;
	TGLCube *Bar;
	TGLSphere *GLSphere1;
	TGLSphere *GLSphere2;
	TGLSphere *GLSphereEllipsoid1;
	TGLSphere *GLSphereEllipsoid2;
	TGLCube *GLCube1;
	TGLCamera *GLCamera1;
	TGLCamera *GLCamera3;
	TTimer *Timer1;
	TGLCollisionManager *CollisionManager1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall cbCollisionModeClick(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall CollisionManager1Collision(TObject *Sender, TGLBaseSceneObject *object1,
          TGLBaseSceneObject *object2);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
private:	// User declarations
	int mdx, mdy;
	bool CollisionDetected;
	TGLCustomSceneObject *CurrSO;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
