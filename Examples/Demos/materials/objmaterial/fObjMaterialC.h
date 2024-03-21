//---------------------------------------------------------------------------

#ifndef fObjMaterialCH
#define fObjMaterialCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLS.VectorFileObjects.hpp"
#include <System.ImageList.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLS.Material.hpp"
#include "GLS.Mesh.hpp"
//---------------------------------------------------------------------------
class TFormObjMaterial : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLCamera *Camera;
	TGLLightSource *LightSource;
	TGLDummyCube *dcPolyhedra;
	TGLDodecahedron *Dodecahedron;
	TGLSimpleNavigation *GLSimpleNavigation1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLMaterialLibrary *GLMatLibTextures;
	TPanel *PanelBottom;
	TRadioGroup *rgPolyhedra;
	TGLTetrahedron *Tetrahedron;
	TGLOctahedron *Octahedron;
	TGLMaterialLibrary *GLMatLibColors;
	TGLDummyCube *dcPolyHex;
	TGLPolygon *Quadrangle0;
	TGLPolygon *Quadrangle1;
	TGLPolygon *Quadrangle2;
	TGLPolygon *Quadrangle3;
	TGLPolygon *Quadrangle4;
	TGLPolygon *Quadrangle5;
	TGLHexahedron *Hexahedron;
	TGLIcosahedron *Icosahedron;
	TGLDummyCube *dcPolyTet;
	TGLPolygon *TetTriangle0;
	TGLPolygon *TetTriangle1;
	TGLPolygon *TetTriangle2;
	TGLPolygon *TetTriangle3;
	TGLDummyCube *dcPolyOct;
	TGLPolygon *OctTriangle0;
	TGLPolygon *OctTriangle1;
	TGLPolygon *OctTriangle2;
	TGLPolygon *OctTriangle3;
	TGLPolygon *OctTriangle4;
	TGLPolygon *OctTriangle5;
	TGLPolygon *OctTriangle6;
	TGLPolygon *OctTriangle7;
	TGLDummyCube *dcPolyIco;
	TGLPolygon *IcoTriangle0;
	TGLPolygon *IcoTriangle1;
	TGLPolygon *IcoTriangle2;
	TGLPolygon *IcoTriangle3;
	TGLPolygon *IcoTriangle4;
	TGLPolygon *IcoTriangle5;
	TGLPolygon *IcoTriangle6;
	TGLPolygon *IcoTriangle7;
	TGLPolygon *IcoTriangle8;
	TGLPolygon *IcoTriangle9;
	TGLPolygon *IcoTriangle10;
	TGLPolygon *IcoTriangle11;
	TGLPolygon *IcoTriangle12;
	TGLPolygon *IcoTriangle13;
	TGLPolygon *IcoTriangle14;
	TGLPolygon *IcoTriangle15;
	TGLPolygon *IcoTriangle16;
	TGLPolygon *IcoTriangle17;
	TGLPolygon *IcoTriangle18;
	TGLPolygon *IcoTriangle19;
	TGLDummyCube *dcPolyDod;
	TGLPolygon *Pentagon0;
	TGLPolygon *Pentagon1;
	TGLPolygon *Pentagon2;
	TGLPolygon *Pentagon3;
	TGLPolygon *Pentagon4;
	TGLPolygon *Pentagon5;
	TGLPolygon *Pentagon6;
	TGLPolygon *Pentagon7;
	TGLPolygon *Pentagon8;
	TGLPolygon *Pentagon9;
	TGLPolygon *Pentagon10;
	TGLPolygon *Pentagon11;
	TCheckBox *chbRotation;
	TGLMesh *meshConvexhull;
	TGLDummyCube *dcConvexhull;
	void __fastcall GLCadencer1Progress(TObject *Sender, const double DeltaTime, const double NewTime);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall rgPolyhedraClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);

private:	// User declarations
   // describe convexhull
public:		// User declarations
	__fastcall TFormObjMaterial(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormObjMaterial *FormObjMaterial;
//---------------------------------------------------------------------------
#endif
