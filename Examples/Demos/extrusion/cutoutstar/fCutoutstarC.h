//---------------------------------------------------------------------------

#ifndef fCutoutstarCH
#define fCutoutstarCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Extrusion.hpp"
#include "GLS.MultiPolygon.hpp"
#include "GLS.Nodes.hpp"

//---------------------------------------------------------------------------
class TFormCutoutstar : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *PanelFPS;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLExtrusionSolid *ExtrusionSolid;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TFormCutoutstar(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormCutoutstar *FormCutoutstar;
//---------------------------------------------------------------------------
#endif
