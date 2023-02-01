// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FmGuiSkinEditor.pas' rev: 35.00 (Windows)

#ifndef FmguiskineditorHPP
#define FmguiskineditorHPP

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
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Windows.hpp>
#include <GLS.HUDObjects.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.Gui.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Utils.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Material.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmguiskineditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSkinEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLSkinEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Gls::Scene::TGLScene* GLScene1;
	Gls::Scene::TGLCamera* GLCamera1;
	Gls::Windows::TGLPanel* GLPanel1;
	Gls::Hudobjects::TGLHUDSprite* HUDSprite1;
	Gls::Scene::TGLMemoryViewer* GLMemoryViewer1;
	Gls::Scene::TGLLightSource* GLLightSource1;
	Vcl::Comctrls::TStatusBar* StatusBar;
	Vcl::Extctrls::TPanel* panBottom;
	Vcl::Extctrls::TPanel* panZoomImage;
	Vcl::Extctrls::TImage* imgFull;
	Vcl::Stdctrls::TScrollBar* sbarHorizontal;
	Vcl::Stdctrls::TScrollBar* sbarVertical;
	Vcl::Stdctrls::TButton* Button5;
	Vcl::Stdctrls::TButton* Button6;
	Vcl::Extctrls::TPanel* panImageProperties;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TLabel* Label6;
	Vcl::Extctrls::TPanel* Panel2;
	Vcl::Extctrls::TImage* imgPreview;
	Vcl::Extctrls::TPanel* Panel3;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TButton* Button3;
	Vcl::Stdctrls::TButton* Button4;
	Vcl::Stdctrls::TCheckBox* CheckBox1;
	Vcl::Stdctrls::TEdit* WidthEdit;
	Vcl::Stdctrls::TEdit* HeightEdit;
	Vcl::Extctrls::TPanel* panElements;
	Vcl::Extctrls::TBevel* Bevel2;
	Vcl::Extctrls::TBevel* Bevel1;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TLabel* Label11;
	Vcl::Stdctrls::TLabel* Label12;
	Vcl::Stdctrls::TLabel* Label13;
	Vcl::Stdctrls::TLabel* Label9;
	Vcl::Stdctrls::TLabel* Label10;
	Vcl::Stdctrls::TLabel* Label14;
	Vcl::Stdctrls::TListBox* lbElements;
	Vcl::Stdctrls::TButton* btnAdd;
	Vcl::Stdctrls::TButton* btnDelete;
	Vcl::Stdctrls::TComboBox* ComboBox1;
	Vcl::Stdctrls::TEdit* LeftEdit;
	Vcl::Stdctrls::TEdit* TopEdit;
	Vcl::Stdctrls::TEdit* RightEdit;
	Vcl::Stdctrls::TEdit* BottomEdit;
	Vcl::Stdctrls::TEdit* ScaleXEdit;
	Vcl::Stdctrls::TEdit* ScaleYEdit;
	Vcl::Menus::TPopupMenu* popElements;
	Vcl::Menus::TMenuItem* mnuTopLeft;
	Vcl::Menus::TMenuItem* mnuTop;
	Vcl::Menus::TMenuItem* mnuTopRight;
	Vcl::Menus::TMenuItem* mnuLeft;
	Vcl::Menus::TMenuItem* mnuCenter;
	Vcl::Menus::TMenuItem* mnuRight;
	Vcl::Menus::TMenuItem* mnuBottomLeft;
	Vcl::Menus::TMenuItem* mnuBottom;
	Vcl::Menus::TMenuItem* mnuBottomRight;
	Vcl::Menus::TMenuItem* N1;
	Vcl::Menus::TMenuItem* mnuAddAll;
	Vcl::Menus::TMenuItem* N2;
	Vcl::Menus::TMenuItem* mnuAllTop;
	Vcl::Menus::TMenuItem* mnuAllMiddle;
	Vcl::Menus::TMenuItem* mnuAllBottom;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall Button3Click(System::TObject* Sender);
	void __fastcall Button4Click(System::TObject* Sender);
	void __fastcall ScrollBarScroll(System::TObject* Sender, System::Uitypes::TScrollCode ScrollCode, int &ScrollPos);
	void __fastcall ScrollbarChange(System::TObject* Sender);
	void __fastcall WidthEditChange(System::TObject* Sender);
	void __fastcall HeightEditChange(System::TObject* Sender);
	void __fastcall btnAddClick(System::TObject* Sender);
	void __fastcall lbElementsClick(System::TObject* Sender);
	void __fastcall ComboBox1Change(System::TObject* Sender);
	void __fastcall btnDeleteClick(System::TObject* Sender);
	void __fastcall imgFullMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall imgFullMouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall imgFullMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall lbElementsKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall CheckBox1Click(System::TObject* Sender);
	void __fastcall ScaleXEditChange(System::TObject* Sender);
	void __fastcall ScaleYEditChange(System::TObject* Sender);
	void __fastcall LeftEditChange(System::TObject* Sender);
	void __fastcall TopEditChange(System::TObject* Sender);
	void __fastcall RightEditChange(System::TObject* Sender);
	void __fastcall BottomEditChange(System::TObject* Sender);
	void __fastcall EditKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall FormResize(System::TObject* Sender);
	void __fastcall MenuItemClick(System::TObject* Sender);
	void __fastcall mnuAddAllClick(System::TObject* Sender);
	void __fastcall mnuAllTopClick(System::TObject* Sender);
	void __fastcall mnuAllMiddleClick(System::TObject* Sender);
	void __fastcall mnuAllBottomClick(System::TObject* Sender);
	void __fastcall imgPreviewMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall imgPreviewMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	
private:
	System::Classes::TWndMethod FOriginalWndProc;
	System::Types::TRect FFocusRect;
	System::Types::TRect VisibleRect;
	System::Types::TPoint PreviewMousePoint;
	int PreviewWidth;
	int PreviewHeight;
	System::Types::TPoint FullMousePoint;
	bool MouseDown;
	void __fastcall ImageWndProc(Winapi::Messages::TMessage &Message);
	void __fastcall DrawImageFocusRect(const System::Types::TRect &ARect);
	void __fastcall AlignZoomPanel();
	void __fastcall UpdateRegionEdits();
	void __fastcall SetEditState(Vcl::Controls::TControl* Parent, bool Enabled);
	void __fastcall AddElement(int Index);
	void __fastcall DrawCrossair(const System::Types::TPoint &Point);
	
public:
	Gls::Gui::TGLGuiElementList* TheGuiComponent;
	Gls::Gui::TGLGuiElement* SelectedElement;
	Gls::Texture::TGLTexture* Tex;
	float Zoom;
	int Width;
	int Height;
	bool __fastcall Edit(Gls::Gui::TGLGuiElementList* GuiComponent);
	void __fastcall Render();
	void __fastcall SetMax(Vcl::Stdctrls::TScrollBar* Scrollbar, int Val);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLSkinEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLSkinEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLSkinEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSkinEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLSkinEditorForm* GLSkinEditorForm;
extern DELPHI_PACKAGE bool __fastcall GUIComponentDialog(Gls::Gui::TGLGuiElementList* GuiComponent);
}	/* namespace Fmguiskineditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMGUISKINEDITOR)
using namespace Fmguiskineditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmguiskineditorHPP
