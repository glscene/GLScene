// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FRMaterialPreview.pas' rev: 35.00 (Windows)

#ifndef FrmaterialpreviewHPP
#define FrmaterialpreviewHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Texture.hpp>
#include <GLS.HUDObjects.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.GeomObjects.hpp>
#include <GLS.Color.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Material.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Frmaterialpreview
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TRMaterialPreview;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TRMaterialPreview : public Vcl::Forms::TFrame
{
	typedef Vcl::Forms::TFrame inherited;
	
__published:
	Gls::Scene::TGLScene* GLScene;
	Gls::Sceneviewer::TGLSceneViewer* GLSceneViewer;
	Vcl::Stdctrls::TComboBox* CBObject;
	Gls::Scene::TGLCamera* Camera;
	Gls::Objects::TGLCube* Cube;
	Gls::Objects::TGLSphere* Sphere;
	Gls::Scene::TGLLightSource* LightSource;
	Vcl::Stdctrls::TComboBox* CBBackground;
	Gls::Hudobjects::TGLHUDSprite* BackGroundSprite;
	Gls::Geomobjects::TGLTeapot* Teapot;
	Gls::Objects::TGLDummyCube* World;
	Gls::Objects::TGLDummyCube* Light;
	Gls::Objects::TGLSphere* FireSphere;
	Gls::Material::TGLMaterialLibrary* GLMaterialLibrary;
	void __fastcall CBObjectChange(System::TObject* Sender);
	void __fastcall CBBackgroundChange(System::TObject* Sender);
	void __fastcall SceneViewerMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SceneViewerMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SceneViewerMouseWheel(System::TObject* Sender, System::Classes::TShiftState Shift, int WheelDelta, const System::Types::TPoint &MousePos, bool &Handled);
	
private:
	Gls::Material::TGLAbstractLibMaterial* FLibMaterial;
	Gls::Material::TGLMaterial* __fastcall GetMaterial();
	void __fastcall SetMaterial(Gls::Material::TGLMaterial* const Value);
	Gls::Material::TGLAbstractLibMaterial* __fastcall GetLibMaterial();
	void __fastcall SetLibMaterial(Gls::Material::TGLAbstractLibMaterial* const Value);
	
public:
	__fastcall virtual TRMaterialPreview(System::Classes::TComponent* AOwner);
	__property Gls::Material::TGLMaterial* Material = {read=GetMaterial, write=SetMaterial};
	__property Gls::Material::TGLAbstractLibMaterial* LibMaterial = {read=GetLibMaterial, write=SetLibMaterial};
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TRMaterialPreview() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TRMaterialPreview(HWND ParentWindow) : Vcl::Forms::TFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Frmaterialpreview */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRMATERIALPREVIEW)
using namespace Frmaterialpreview;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrmaterialpreviewHPP
