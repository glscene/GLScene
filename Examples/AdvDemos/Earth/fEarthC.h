//---------------------------------------------------------------------------

#ifndef fEarthCH
#define fEarthCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.LensFlare.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.SkyDome.hpp"
#include "GLSL.TextureShaders.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmEarth : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer;
	TGLScene *Scene;
	TGLSkyDome *SkyDome;
	TGLLines *ConstellationLines;
	TGLLines *ConstellationBorders;
	TGLDummyCube *dcEarth;
	TGLCamera *CameraController;
	TGLCamera *Camera;
	TGLDummyCube *dcMoon;
	TGLSphere *SphereMoon;
	TGLSphere *SphereEarth;
	TGLDirectOpenGL *DirectOpenGL;
	TGLLightSource *LightSourceSun;
	TGLLensFlare *GLLensFlare1;
	TGLCadencer *Cadencer;
	TTimer *Timer1;
	TGLMaterialLibrary *MatLib;
	TGLTexCombineShader *EarthCombiner;
private:	// User declarations
public:		// User declarations
	__fastcall TfrmEarth(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmEarth *frmEarth;
//---------------------------------------------------------------------------
#endif
