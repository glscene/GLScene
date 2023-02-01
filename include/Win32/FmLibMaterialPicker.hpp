// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FmLibMaterialPicker.pas' rev: 35.00 (Windows)

#ifndef FmlibmaterialpickerHPP
#define FmlibmaterialpickerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Objects.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.Material.hpp>
#include <GLS.Color.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.HUDObjects.hpp>
#include <GLS.GeomObjects.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmlibmaterialpicker
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLLibMaterialPickerForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLLibMaterialPickerForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TListBox* ListBoxMaterials;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Buttons::TBitBtn* BitBtnOk;
	Vcl::Buttons::TBitBtn* BitBtnCancel;
	Gls::Scene::TGLScene* GLScene1;
	Gls::Sceneviewer::TGLSceneViewer* GLSceneViewer1;
	Gls::Scene::TGLLightSource* LightSource1;
	Gls::Objects::TGLDummyCube* dcLight1;
	Gls::Objects::TGLDummyCube* dcWorld1;
	Gls::Scene::TGLCamera* Camera1;
	Gls::Objects::TGLCube* Cube1;
	Gls::Objects::TGLSphere* Sphere1;
	Gls::Objects::TGLSphere* FireSphere1;
	Gls::Geomobjects::TGLTeapot* Teapot1;
	Gls::Material::TGLMaterialLibrary* GLMaterialLibrary1;
	Vcl::Stdctrls::TComboBox* ComboBoxObject1;
	Vcl::Stdctrls::TComboBox* ComboBoxBackGround1;
	Gls::Hudobjects::TGLHUDSprite* BackGroundSprite1;
	Gls::Scene::TGLLightSource* LightSource2;
	void __fastcall ListBoxMaterialsClick(System::TObject* Sender);
	void __fastcall ListBoxMaterialsKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall ListBoxMaterialsDblClick(System::TObject* Sender);
	void __fastcall ComboBoxObject1Change(System::TObject* Sender);
	void __fastcall ComboBoxBackGround1Change(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall GLSceneViewer1MouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall GLSceneViewer1MouseWheel(System::TObject* Sender, System::Classes::TShiftState Shift, int WheelDelta, const System::Types::TPoint &MousePos, bool &Handled);
	void __fastcall GLSceneViewer1MouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	
private:
	int mx;
	int my;
	Gls::Material::TGLAbstractLibMaterial* FLibMaterial1;
	Gls::Material::TGLMaterial* __fastcall GetMaterial1();
	void __fastcall SetMaterial1(Gls::Material::TGLMaterial* const Value);
	Gls::Material::TGLAbstractLibMaterial* __fastcall GetLibMaterial1();
	void __fastcall SetLibMaterial1(Gls::Material::TGLAbstractLibMaterial* const Value);
	
public:
	bool __fastcall Execute(System::UnicodeString &MaterialName, Gls::Material::TGLAbstractMaterialLibrary* MaterialLibrary);
	__property Gls::Material::TGLMaterial* Material1 = {read=GetMaterial1, write=SetMaterial1};
	__property Gls::Material::TGLAbstractLibMaterial* LibMaterial1 = {read=GetLibMaterial1, write=SetLibMaterial1};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLLibMaterialPickerForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLLibMaterialPickerForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLLibMaterialPickerForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLLibMaterialPickerForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLLibMaterialPickerForm* __fastcall GLLibMaterialPickerForm(void);
extern DELPHI_PACKAGE void __fastcall ReleaseLibMaterialPickerForm(void);
}	/* namespace Fmlibmaterialpicker */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMLIBMATERIALPICKER)
using namespace Fmlibmaterialpicker;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmlibmaterialpickerHPP
