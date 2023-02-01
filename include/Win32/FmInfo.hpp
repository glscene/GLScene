// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FmInfo.pas' rev: 35.00 (Windows)

#ifndef FminfoHPP
#define FminfoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.OpenGLAdapter.hpp>
#include <GLS.Scene.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.Context.hpp>
#include <GLS.Utils.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fminfo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLInfoForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLInfoForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TLabel* AccLabel;
	Vcl::Stdctrls::TLabel* AccumLabel;
	Vcl::Stdctrls::TLabel* AuxLabel;
	Vcl::Stdctrls::TLabel* ClipLabel;
	Vcl::Stdctrls::TLabel* ColorLabel;
	Vcl::Stdctrls::TLabel* CopyLabel;
	Vcl::Stdctrls::TLabel* DepthLabel;
	Vcl::Stdctrls::TLabel* DoubleLabel;
	Vcl::Stdctrls::TLabel* EvalLabel;
	Vcl::Extctrls::TImage* Image;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label10;
	Vcl::Stdctrls::TLabel* Label11;
	Vcl::Stdctrls::TLabel* Label12;
	Vcl::Stdctrls::TLabel* Label13;
	Vcl::Stdctrls::TLabel* Label14;
	Vcl::Stdctrls::TLabel* Label15;
	Vcl::Stdctrls::TLabel* Label16;
	Vcl::Stdctrls::TLabel* Label17;
	Vcl::Stdctrls::TLabel* Label18;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label20;
	Vcl::Stdctrls::TLabel* Label23;
	Vcl::Stdctrls::TLabel* Label25;
	Vcl::Stdctrls::TLabel* Label26;
	Vcl::Stdctrls::TLabel* Label27;
	Vcl::Stdctrls::TLabel* Label28;
	Vcl::Stdctrls::TLabel* Label29;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label30;
	Vcl::Stdctrls::TLabel* LabelCommon;
	Vcl::Stdctrls::TLabel* LabelDepths;
	Vcl::Stdctrls::TLabel* LabelMaxValues;
	Vcl::Stdctrls::TLabel* Label34;
	Vcl::Stdctrls::TLabel* Label35;
	Vcl::Stdctrls::TLabel* Label37;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TLabel* Label6;
	Vcl::Stdctrls::TLabel* Label7;
	Vcl::Stdctrls::TLabel* Label8;
	Vcl::Stdctrls::TLabel* Label9;
	Vcl::Stdctrls::TLabel* LightLabel;
	Vcl::Stdctrls::TLabel* ListLabel;
	Vcl::Stdctrls::TMemo* MemoAbout;
	Vcl::Stdctrls::TMemo* MemoContributors;
	Vcl::Stdctrls::TLabel* ModelLabel;
	Vcl::Stdctrls::TLabel* NameLabel;
	Vcl::Stdctrls::TLabel* OverlayLabel;
	Vcl::Comctrls::TPageControl* PageControl;
	Vcl::Stdctrls::TLabel* PixelLabel;
	Vcl::Stdctrls::TLabel* ProjLabel;
	Vcl::Stdctrls::TLabel* RendererLabel;
	Vcl::Forms::TScrollBox* ScrollBoxInfo;
	Vcl::Comctrls::TTabSheet* TabSheetInformation;
	Vcl::Stdctrls::TLabel* StencilLabel;
	Vcl::Stdctrls::TLabel* StereoLabel;
	Vcl::Stdctrls::TLabel* SubLabel;
	Vcl::Comctrls::TTabSheet* TabSheetAbout;
	Vcl::Comctrls::TTabSheet* TabSheetContributors;
	Vcl::Stdctrls::TLabel* TexSizeLabel;
	Vcl::Stdctrls::TLabel* TexStackLabel;
	Vcl::Stdctrls::TLabel* TexUnitsLabel;
	Vcl::Stdctrls::TLabel* UnderlayLabel;
	Vcl::Stdctrls::TLabel* VendorLabel;
	Vcl::Stdctrls::TLabel* VersionLabel;
	Vcl::Comctrls::TTabSheet* TabSheetExtensions;
	Vcl::Stdctrls::TListBox* ListBoxExtensions;
	Vcl::Menus::TPopupMenu* PMWebLink;
	Vcl::Menus::TMenuItem* MIRegistryLink;
	Vcl::Menus::TMenuItem* MIDelphi3D;
	Vcl::Comctrls::TTabSheet* TabSheetGLScene;
	Vcl::Stdctrls::TButton* CloseButton;
	Vcl::Stdctrls::TLabel* VersionLbl;
	Vcl::Stdctrls::TLabel* ViewLabel;
	Vcl::Stdctrls::TLabel* lblSfGLScene;
	Vcl::Stdctrls::TLabel* lblGithubGLScene;
	void __fastcall CloseButtonClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall FormClose(System::TObject* Sender, System::Uitypes::TCloseAction &Action);
	void __fastcall ListBoxExtensionsDblClick(System::TObject* Sender);
	void __fastcall ListBoxExtensionsClick(System::TObject* Sender);
	void __fastcall ListBoxExtensionsKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall MIDelphi3DClick(System::TObject* Sender);
	void __fastcall lblSfGLSceneClick(System::TObject* Sender);
	void __fastcall lblGithubGLSceneClick(System::TObject* Sender);
	
protected:
	System::UnicodeString __fastcall GetSceneVersion();
	
public:
	void __fastcall GetInfoFrom(Gls::Scene::TGLSceneBuffer* aSceneBuffer);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLInfoForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLInfoForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLInfoForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLInfoForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Fminfo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMINFO)
using namespace Fminfo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FminfoHPP
