// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CUDA.EditorFm.pas' rev: 35.00 (Windows)

#ifndef Cuda_EditorfmHPP
#define Cuda_EditorfmHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <System.Classes.hpp>
#include <System.Win.Registry.hpp>
#include <System.ImageList.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ToolWin.hpp>
#include <DesignIntf.hpp>
#include <VCLEditors.hpp>
#include <GLS.Strings.hpp>
#include <CUDA.APIComps.hpp>
#include <CUDA.FFTPlan.hpp>
#include <CUDA.Graphics.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cuda
{
namespace Editorfm
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCUDAEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLCUDAEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Comctrls::TToolBar* ToolBar1;
	Vcl::Comctrls::TToolButton* AddModuleButton;
	Vcl::Comctrls::TToolButton* DeleteButton;
	Vcl::Stdctrls::TListBox* ListBox1;
	Vcl::Controls::TImageList* ImageList1;
	Vcl::Comctrls::TToolButton* AddMemDataButton;
	Vcl::Comctrls::TToolButton* AddFFTPlanButton;
	Vcl::Comctrls::TToolButton* AddGeometryResButton;
	Vcl::Comctrls::TToolButton* AddImageResButton;
	void __fastcall AddItemButtonClick(System::TObject* Sender);
	void __fastcall DeleteButtonClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall ListBox1Click(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	
private:
	System::Classes::TList* FClassList;
	Cuda::Apicomps::TGLCUDA* FCUDA;
	Designintf::_di_IDesigner FCurrentDesigner;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall OnCUDAComponentNameChanged(System::TObject* Sender);
	
public:
	void __fastcall SetCUDAEditorClient(Cuda::Apicomps::TGLCUDA* Client, Designintf::_di_IDesigner Designer);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLCUDAEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLCUDAEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLCUDAEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLCUDAEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLCUDAEditorForm* __fastcall GLCUDAEditorForm(void);
extern DELPHI_PACKAGE void __fastcall ReleaseGLCUDAEditorForm(void);
}	/* namespace Editorfm */
}	/* namespace Cuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA_EDITORFM)
using namespace Cuda::Editorfm;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA)
using namespace Cuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cuda_EditorfmHPP
