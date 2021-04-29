//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <GLS.GeomObjects.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Objects.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"
        // Pascal unit
//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLSceneViewer * GLSceneViewer1;
  TGLScene *GLScene1;
  TGLCamera *GLCamera1;
  TGLDummyCube *DCSphere;
  TGLArrowLine *ArrowLine;
  TGLLightSource *GLLightSource1;
  TGLSphere *Sphere;
  TGLDummyCube *DCArrow;
  TGLCadencer *GLCadencer1;
  TGLDisk *Disk1;
  TGLDisk *Disk2;
  TGLLines *Lines1;
  TGLPlane *Plane1;
  TGLLines *Lines2;
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
                                      const double newTime);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 
