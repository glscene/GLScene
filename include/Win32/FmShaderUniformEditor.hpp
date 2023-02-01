// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FmShaderUniformEditor.pas' rev: 35.00 (Windows)

#ifndef FmshaderuniformeditorHPP
#define FmshaderuniformeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <GLS.Strings.hpp>
#include <GLSL.ShaderParameter.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.VectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmshaderuniformeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLShaderUniformEditor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLShaderUniformEditor : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
	
private:
	typedef System::DynamicArray<Glsl::Shaderparameter::_di_IShaderParameter> _TGLShaderUniformEditor__1;
	
	
__published:
	Vcl::Stdctrls::TListBox* LBUniforms;
	Vcl::Stdctrls::TLabel* Labe1;
	Vcl::Stdctrls::TComboBox* AutoSetBox;
	Vcl::Stdctrls::TComboBox* SamplerBox;
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Extctrls::TRadioGroup* RedGroup;
	Vcl::Extctrls::TRadioGroup* GreenGroup;
	Vcl::Extctrls::TRadioGroup* BlueGroup;
	Vcl::Extctrls::TRadioGroup* AlphaGroup;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TComboBox* TextureBox;
	Vcl::Stdctrls::TButton* Button1;
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall LBUniformsClick(System::TObject* Sender);
	void __fastcall ColorGroupClick(System::TObject* Sender);
	void __fastcall AutoSetBoxChange(System::TObject* Sender);
	void __fastcall TextureBoxChange(System::TObject* Sender);
	void __fastcall SamplerBoxChange(System::TObject* Sender);
	void __fastcall LBUniformsKeyPress(System::TObject* Sender, System::WideChar &Key);
	
private:
	_TGLShaderUniformEditor__1 FUniformList;
	
public:
	void __fastcall Clear();
	void __fastcall AddTextureName(const System::UnicodeString S);
	void __fastcall AddSamplerName(const System::UnicodeString S);
	void __fastcall AddUniform(Glsl::Shaderparameter::_di_IShaderParameter AValue);
	void __fastcall Execute();
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLShaderUniformEditor(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLShaderUniformEditor(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLShaderUniformEditor() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLShaderUniformEditor(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLShaderUniformEditor* __fastcall GLShaderUniformEditor(void);
extern DELPHI_PACKAGE void __fastcall ReleaseShaderUniformEditor(void);
}	/* namespace Fmshaderuniformeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMSHADERUNIFORMEDITOR)
using namespace Fmshaderuniformeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmshaderuniformeditorHPP
