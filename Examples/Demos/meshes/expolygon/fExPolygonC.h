//---------------------------------------------------------------------------

#ifndef fExPolygonCH
#define fExPolygonCH
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>

#include "GLS.Scene.hpp"
#include "GLS.Objects.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Texture.hpp"
#include "GLS.MultiPolygon.hpp"
#include "GLS.VectorGeometry.hpp"
#include "GLS.SceneViewer.hpp"

#include "GLS.Material.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.BaseClasses.hpp"


//---------------------------------------------------------------------------

struct TVektor
{
 double x;
 double y;
 double z;
};

class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLLightSource *GLLightSource2;
	TGLDummyCube *Container;
	TGLDummyCube *CameraTarget;
	TGLCamera *Camera;
	TGLMaterialLibrary *GLMaterialLibrary1;
	void __fastcall FormShow(TObject *Sender);
private:	// User declarations
	int mx,my;
	double FDY;
	double FDX;
	double FDZ;
	TGLMultiPolygon __fastcall GetPlane(int Side);
	void __fastcall SetDX(const double Value);
	void __fastcall SetDY(const double Value);
	void __fastcall SetDZ(const double Value);
	void __fastcall CreatePanel();
	void __fastcall AddMaterial(TGLSceneObject *Obj);
	void __fastcall ReDraw();
	TVektor __fastcall TransformToPlane(int Side, double x, double y, double z); //overload;
	TVektor __fastcall TransformToPlane(int Side, TVektor v); // overload;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);

	void __fastcall MakeHole(int Side, double X, double Y, double Z,
							 double D, double T, double Phi, double Rho);
///	property Plane[Side:Integer]:TGLMultiPolygon read GetPlane;
	__property double DX = {read=FDX, write=SetDX};
	__property double DY = {read=FDY, write=SetDY};
	__property double DZ = {read=FDZ, write=SetDZ};
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
