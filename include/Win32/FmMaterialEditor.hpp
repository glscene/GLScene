// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FmMaterialEditor.pas' rev: 35.00 (Windows)

#ifndef FmmaterialeditorHPP
#define FmmaterialeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Buttons.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.State.hpp>
#include <GLS.Material.hpp>
#include <GLS.Texture.hpp>
#include <FRTrackBarEdit.hpp>
#include <FRMaterialPreview.hpp>
#include <FRColorEditor.hpp>
#include <FRFaceEditor.hpp>
#include <FRTextureEdit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmmaterialeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMaterialEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLMaterialEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Comctrls::TPageControl* PageControl1;
	Vcl::Comctrls::TTabSheet* TSFront;
	Vcl::Comctrls::TTabSheet* TSBack;
	Vcl::Comctrls::TTabSheet* TSTexture;
	Frfaceeditor::TRFaceEditor* FEFront;
	Frfaceeditor::TRFaceEditor* FEBack;
	Vcl::Stdctrls::TGroupBox* GroupBox1;
	Frmaterialpreview::TRMaterialPreview* MPPreview;
	Vcl::Buttons::TBitBtn* BBOk;
	Vcl::Buttons::TBitBtn* BBCancel;
	Frtextureedit::TRTextureEdit* RTextureEdit;
	Vcl::Stdctrls::TComboBox* CBBlending;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TComboBox* CBPolygonMode;
	void __fastcall OnMaterialChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TGLMaterialEditorForm(System::Classes::TComponent* AOwner);
	bool __fastcall Execute(Gls::Material::TGLMaterial* AMaterial);
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLMaterialEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLMaterialEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLMaterialEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLMaterialEditorForm* __fastcall GLMaterialEditorForm(void);
extern DELPHI_PACKAGE void __fastcall ReleaseMaterialEditorForm(void);
}	/* namespace Fmmaterialeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMMATERIALEDITOR)
using namespace Fmmaterialeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmmaterialeditorHPP
