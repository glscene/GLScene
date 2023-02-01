// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Memo.pas' rev: 35.00 (Windows)

#ifndef Gls_MemoHPP
#define Gls_MemoHPP

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
#include <System.UITypes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Clipbrd.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.Types.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Memo
{
//-- forward type declarations -----------------------------------------------
struct TCellSize;
struct TCellPos;
struct TFullPos;
class DELPHICLASS TLineProp;
class DELPHICLASS TCharStyle;
class DELPHICLASS TStyleList;
class DELPHICLASS TGLAbstractMemoObject;
class DELPHICLASS TGLSMemoAbstractScrollableObject;
class DELPHICLASS TGLSMemoScrollBar;
class DELPHICLASS TGLSMemoStrings;
class DELPHICLASS TGLSMemoGutter;
class DELPHICLASS TGLSMemoUndo;
class DELPHICLASS TGLSMemoInsCharUndo;
class DELPHICLASS TGLSMemoDelCharUndo;
class DELPHICLASS TGLSMEmoDelLineUndo;
class DELPHICLASS TGLSMemoSelUndo;
class DELPHICLASS TGLSMemoDeleteBufUndo;
class DELPHICLASS TGLSMemoPasteUndo;
class DELPHICLASS TGLSMemoUndoList;
class DELPHICLASS TGLSCustomMemo;
class DELPHICLASS TGLSMemo;
class DELPHICLASS TGLSMemoStringList;
class DELPHICLASS TGLSSynHiMemo;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TBorderType : unsigned char { btRaised, btLowered, btFlatRaised, btFlatLowered };

typedef int TCommand;

struct DECLSPEC_DRECORD TCellSize
{
public:
	int W;
	int H;
};


struct DECLSPEC_DRECORD TCellPos
{
public:
	int X;
	int Y;
};


struct DECLSPEC_DRECORD TFullPos
{
public:
	int LineNo;
	int Pos;
};


class PASCALIMPLEMENTATION TLineProp : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::TObject* FObject;
	int FStyleNo;
	bool FInComment;
	int FInBrackets;
	bool FValidAttrs;
	System::UnicodeString FCharAttrs;
public:
	/* TObject.Create */ inline __fastcall TLineProp() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TLineProp() { }
	
};


class PASCALIMPLEMENTATION TCharStyle : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Uitypes::TColor FTextColor;
	System::Uitypes::TColor FBkColor;
	System::Uitypes::TFontStyles FStyle;
	
__published:
	__property System::Uitypes::TColor TextColor = {read=FTextColor, write=FTextColor, nodefault};
	__property System::Uitypes::TColor BkColor = {read=FBkColor, write=FBkColor, nodefault};
	__property System::Uitypes::TFontStyles Style = {read=FStyle, write=FStyle, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCharStyle() { }
	
public:
	/* TObject.Create */ inline __fastcall TCharStyle() : System::Classes::TPersistent() { }
	
};


class PASCALIMPLEMENTATION TStyleList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
private:
	void __fastcall CheckRange(int Index);
	System::Uitypes::TColor __fastcall GetTextColor(int Index);
	void __fastcall SetTextColor(int Index, System::Uitypes::TColor Value);
	System::Uitypes::TColor __fastcall GetBkColor(int Index);
	void __fastcall SetBkColor(int Index, System::Uitypes::TColor Value);
	System::Uitypes::TFontStyles __fastcall GetStyle(int Index);
	void __fastcall SetStyle(int Index, System::Uitypes::TFontStyles Value);
	
protected:
	__property System::Uitypes::TColor TextColor[int Index] = {read=GetTextColor, write=SetTextColor};
	__property System::Uitypes::TColor BkColor[int Index] = {read=GetBkColor, write=SetBkColor};
	__property System::Uitypes::TFontStyles Style[int Index] = {read=GetStyle, write=SetStyle};
	
public:
	__fastcall virtual ~TStyleList();
	virtual void __fastcall Clear();
	HIDESBASE void __fastcall Delete(int Index);
	HIDESBASE int __fastcall Add(System::Uitypes::TColor ATextColor, System::Uitypes::TColor ABkCOlor, System::Uitypes::TFontStyles AStyle);
	void __fastcall Change(int Index, System::Uitypes::TColor ATextColor, System::Uitypes::TColor ABkColor, System::Uitypes::TFontStyles AStyle);
public:
	/* TObject.Create */ inline __fastcall TStyleList() : System::Classes::TList() { }
	
};


class PASCALIMPLEMENTATION TGLAbstractMemoObject : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	virtual bool __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y) = 0 ;
	virtual bool __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y) = 0 ;
	virtual bool __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y) = 0 ;
public:
	/* TObject.Create */ inline __fastcall TGLAbstractMemoObject() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLAbstractMemoObject() { }
	
};


class PASCALIMPLEMENTATION TGLSMemoAbstractScrollableObject : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
protected:
	virtual void __fastcall DoScroll(TGLSMemoScrollBar* Sender, int ByValue) = 0 ;
	virtual void __fastcall DoScrollPage(TGLSMemoScrollBar* Sender, int ByValue) = 0 ;
public:
	/* TCustomControl.Create */ inline __fastcall virtual TGLSMemoAbstractScrollableObject(System::Classes::TComponent* AOwner) : Vcl::Controls::TCustomControl(AOwner) { }
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TGLSMemoAbstractScrollableObject() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSMemoAbstractScrollableObject(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TsbState : unsigned char { sbsWait, sbsBack, sbsForward, sbsPageBack, sbsPageForward, sbsDragging };

class PASCALIMPLEMENTATION TGLSMemoScrollBar : public TGLAbstractMemoObject
{
	typedef TGLAbstractMemoObject inherited;
	
private:
	Vcl::Forms::TScrollBarKind FKind;
	TGLSMemoAbstractScrollableObject* FParent;
	int FLeft;
	int FTop;
	int FWidth;
	int FHeight;
	int FTotal;
	int FMaxPosition;
	int FPosition;
	int FButtonLength;
	TsbState FState;
	int FXOffset;
	int FYOffset;
	void __fastcall SetParams(int Index, int Value);
	void __fastcall SetState(TsbState Value);
	System::Types::TRect __fastcall GetRect();
	System::Types::TRect __fastcall GetThumbRect();
	System::Types::TRect __fastcall GetBackRect();
	System::Types::TRect __fastcall GetMiddleRect();
	System::Types::TRect __fastcall GetForwardRect();
	System::Types::TRect __fastcall GetPgBackRect();
	System::Types::TRect __fastcall GetPgForwardRect();
	
public:
	__fastcall TGLSMemoScrollBar(TGLSMemoAbstractScrollableObject* AParent, Vcl::Forms::TScrollBarKind AKind);
	void __fastcall PaintTo(Vcl::Graphics::TCanvas* ACanvas);
	virtual bool __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual bool __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual bool __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	int __fastcall MoveThumbTo(int X, int Y);
	__property TGLSMemoAbstractScrollableObject* Parent = {read=FParent};
	__property Vcl::Forms::TScrollBarKind Kind = {read=FKind, write=FKind, nodefault};
	__property TsbState State = {read=FState, write=SetState, nodefault};
	__property int Left = {read=FLeft, write=SetParams, index=0, nodefault};
	__property int Top = {read=FTop, write=SetParams, index=1, nodefault};
	__property int Width = {read=FWidth, write=SetParams, index=2, nodefault};
	__property int Height = {read=FHeight, write=SetParams, index=3, nodefault};
	__property int Total = {read=FTotal, write=SetParams, index=4, nodefault};
	__property int MaxPosition = {read=FMaxPosition, write=SetParams, index=5, nodefault};
	__property int Position = {read=FPosition, write=SetParams, index=6, nodefault};
	__property System::Types::TRect FullRect = {read=GetRect};
	__property System::Types::TRect ThumbRect = {read=GetThumbRect};
	__property System::Types::TRect BackRect = {read=GetBackRect};
	__property System::Types::TRect MiddleRect = {read=GetMiddleRect};
	__property System::Types::TRect ForwardRect = {read=GetForwardRect};
	__property System::Types::TRect PageForwardRect = {read=GetPgForwardRect};
	__property System::Types::TRect PageBackRect = {read=GetPgBackRect};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSMemoScrollBar() { }
	
};


class PASCALIMPLEMENTATION TGLSMemoStrings : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
private:
	TGLSCustomMemo* FMemo;
	int FLockCount;
	bool FDeleting;
	void __fastcall CheckRange(int Index);
	TLineProp* __fastcall GetLineProp(int Index);
	void __fastcall SetLineStyle(int Index, int Value);
	int __fastcall GetLineStyle(int Index);
	bool __fastcall GetInComment(int Index);
	void __fastcall SetInComment(int Index, bool Value);
	int __fastcall GetInBrackets(int Index);
	void __fastcall SetInBrackets(int Index, int Value);
	bool __fastcall GetValidAttrs(int Index);
	void __fastcall SetValidAttrs(int Index, bool Value);
	System::UnicodeString __fastcall GetCharAttrs(int Index);
	void __fastcall SetCharAttrs(int Index, const System::UnicodeString Value);
	
protected:
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetUpdateState(bool Updating);
	TLineProp* __fastcall CreateProp(int Index);
	__property TLineProp* LineProp[int Index] = {read=GetLineProp};
	__property int Style[int Index] = {read=GetLineStyle, write=SetLineStyle};
	__property bool InComment[int Index] = {read=GetInComment, write=SetInComment};
	__property int InBrackets[int Index] = {read=GetInBrackets, write=SetInBrackets};
	__property bool ValidAttrs[int Index] = {read=GetValidAttrs, write=SetValidAttrs};
	__property System::UnicodeString CharAttrs[int Index] = {read=GetCharAttrs, write=SetCharAttrs};
	
public:
	__fastcall virtual ~TGLSMemoStrings();
	virtual void __fastcall Clear();
	int __fastcall DoAdd(const System::UnicodeString S);
	virtual int __fastcall Add(const System::UnicodeString S);
	virtual int __fastcall AddObject(const System::UnicodeString S, System::TObject* AObject);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Insert(int Index, const System::UnicodeString S);
	void __fastcall DoInsert(int Index, const System::UnicodeString S);
	virtual void __fastcall InsertObject(int Index, const System::UnicodeString S, System::TObject* AObject);
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall LoadFromFile(const System::UnicodeString FileName)/* overload */;
public:
	/* TStringList.Create */ inline __fastcall TGLSMemoStrings()/* overload */ : System::Classes::TStringList() { }
	/* TStringList.Create */ inline __fastcall TGLSMemoStrings(bool OwnsObjects)/* overload */ : System::Classes::TStringList(OwnsObjects) { }
	/* TStringList.Create */ inline __fastcall TGLSMemoStrings(System::WideChar QuoteChar, System::WideChar Delimiter)/* overload */ : System::Classes::TStringList(QuoteChar, Delimiter) { }
	/* TStringList.Create */ inline __fastcall TGLSMemoStrings(System::WideChar QuoteChar, System::WideChar Delimiter, System::Classes::TStringsOptions Options)/* overload */ : System::Classes::TStringList(QuoteChar, Delimiter, Options) { }
	/* TStringList.Create */ inline __fastcall TGLSMemoStrings(System::Types::TDuplicates Duplicates, bool Sorted, bool CaseSensitive)/* overload */ : System::Classes::TStringList(Duplicates, Sorted, CaseSensitive) { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  LoadFromFile(const System::UnicodeString FileName, System::Sysutils::TEncoding* Encoding){ System::Classes::TStrings::LoadFromFile(FileName, Encoding); }
	
};


class PASCALIMPLEMENTATION TGLSMemoGutter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLSCustomMemo* FMemo;
	int FLeft;
	int FTop;
	int FWidth;
	int FHeight;
	System::Uitypes::TColor FColor;
	void __fastcall SetParams(int Index, int Value);
	System::Types::TRect __fastcall GetRect();
	
protected:
	void __fastcall PaintTo(Vcl::Graphics::TCanvas* ACanvas);
	void __fastcall Invalidate();
	
public:
	__property int Left = {read=FLeft, write=SetParams, index=0, nodefault};
	__property int Top = {read=FTop, write=SetParams, index=1, nodefault};
	__property int Width = {read=FWidth, write=SetParams, index=2, nodefault};
	__property int Height = {read=FHeight, write=SetParams, index=3, nodefault};
	__property System::Types::TRect FullRect = {read=GetRect};
public:
	/* TObject.Create */ inline __fastcall TGLSMemoGutter() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSMemoGutter() { }
	
};


class PASCALIMPLEMENTATION TGLSMemoUndo : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLSCustomMemo* FMemo;
	int FUndoCurX0;
	int FUndoCurY0;
	int FUndoCurX;
	int FUndoCurY;
	System::UnicodeString FUndoText;
	
public:
	__fastcall TGLSMemoUndo(int ACurX0, int ACurY0, int ACurX, int ACurY, const System::UnicodeString AText);
	virtual bool __fastcall Append(TGLSMemoUndo* NewUndo);
	void __fastcall Undo();
	void __fastcall Redo();
	virtual void __fastcall PerformUndo() = 0 ;
	virtual void __fastcall PerformRedo() = 0 ;
	__property int UndoCurX0 = {read=FUndoCurX0, write=FUndoCurX0, nodefault};
	__property int UndoCurY0 = {read=FUndoCurY0, write=FUndoCurY0, nodefault};
	__property int UndoCurX = {read=FUndoCurX, write=FUndoCurX, nodefault};
	__property int UndoCurY = {read=FUndoCurY, write=FUndoCurY, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSMemoUndo() { }
	
};


class PASCALIMPLEMENTATION TGLSMemoInsCharUndo : public TGLSMemoUndo
{
	typedef TGLSMemoUndo inherited;
	
public:
	virtual bool __fastcall Append(TGLSMemoUndo* NewUndo);
	virtual void __fastcall PerformUndo();
	virtual void __fastcall PerformRedo();
public:
	/* TGLSMemoUndo.Create */ inline __fastcall TGLSMemoInsCharUndo(int ACurX0, int ACurY0, int ACurX, int ACurY, const System::UnicodeString AText) : TGLSMemoUndo(ACurX0, ACurY0, ACurX, ACurY, AText) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSMemoInsCharUndo() { }
	
};


class PASCALIMPLEMENTATION TGLSMemoDelCharUndo : public TGLSMemoUndo
{
	typedef TGLSMemoUndo inherited;
	
private:
	bool FIsBackspace;
	
public:
	virtual bool __fastcall Append(TGLSMemoUndo* NewUndo);
	virtual void __fastcall PerformUndo();
	virtual void __fastcall PerformRedo();
	__property bool IsBackspace = {read=FIsBackspace, write=FIsBackspace, nodefault};
public:
	/* TGLSMemoUndo.Create */ inline __fastcall TGLSMemoDelCharUndo(int ACurX0, int ACurY0, int ACurX, int ACurY, const System::UnicodeString AText) : TGLSMemoUndo(ACurX0, ACurY0, ACurX, ACurY, AText) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSMemoDelCharUndo() { }
	
};


class PASCALIMPLEMENTATION TGLSMEmoDelLineUndo : public TGLSMemoUndo
{
	typedef TGLSMemoUndo inherited;
	
private:
	int FIndex;
	
public:
	__fastcall TGLSMEmoDelLineUndo(int AIndex, int ACurX0, int ACurY0, int ACurX, int ACurY, const System::UnicodeString AText);
	virtual void __fastcall PerformUndo();
	virtual void __fastcall PerformRedo();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSMEmoDelLineUndo() { }
	
};


class PASCALIMPLEMENTATION TGLSMemoSelUndo : public TGLSMemoUndo
{
	typedef TGLSMemoUndo inherited;
	
private:
	int FUndoSelStartX;
	int FUndoSelStartY;
	int FUndoSelEndX;
	int FUndoSelEndY;
	
public:
	__property int UndoSelStartX = {read=FUndoSelStartX, write=FUndoSelStartX, nodefault};
	__property int UndoSelStartY = {read=FUndoSelStartY, write=FUndoSelStartY, nodefault};
	__property int UndoSelEndX = {read=FUndoSelEndX, write=FUndoSelEndX, nodefault};
	__property int UndoSelEndY = {read=FUndoSelEndY, write=FUndoSelEndY, nodefault};
public:
	/* TGLSMemoUndo.Create */ inline __fastcall TGLSMemoSelUndo(int ACurX0, int ACurY0, int ACurX, int ACurY, const System::UnicodeString AText) : TGLSMemoUndo(ACurX0, ACurY0, ACurX, ACurY, AText) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSMemoSelUndo() { }
	
};


class PASCALIMPLEMENTATION TGLSMemoDeleteBufUndo : public TGLSMemoSelUndo
{
	typedef TGLSMemoSelUndo inherited;
	
public:
	virtual void __fastcall PerformUndo();
	virtual void __fastcall PerformRedo();
public:
	/* TGLSMemoUndo.Create */ inline __fastcall TGLSMemoDeleteBufUndo(int ACurX0, int ACurY0, int ACurX, int ACurY, const System::UnicodeString AText) : TGLSMemoSelUndo(ACurX0, ACurY0, ACurX, ACurY, AText) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSMemoDeleteBufUndo() { }
	
};


class PASCALIMPLEMENTATION TGLSMemoPasteUndo : public TGLSMemoUndo
{
	typedef TGLSMemoUndo inherited;
	
public:
	virtual void __fastcall PerformUndo();
	virtual void __fastcall PerformRedo();
public:
	/* TGLSMemoUndo.Create */ inline __fastcall TGLSMemoPasteUndo(int ACurX0, int ACurY0, int ACurX, int ACurY, const System::UnicodeString AText) : TGLSMemoUndo(ACurX0, ACurY0, ACurX, ACurY, AText) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSMemoPasteUndo() { }
	
};


class PASCALIMPLEMENTATION TGLSMemoUndoList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TGLSMemoUndo* operator[](int Index) { return this->Items[Index]; }
	
private:
	int FPos;
	TGLSCustomMemo* FMemo;
	bool FIsPerforming;
	int FLimit;
	
protected:
	HIDESBASE TGLSMemoUndo* __fastcall Get(int Index);
	void __fastcall SetLimit(int Value);
	
public:
	__fastcall TGLSMemoUndoList();
	__fastcall virtual ~TGLSMemoUndoList();
	HIDESBASE int __fastcall Add(void * Item);
	virtual void __fastcall Clear();
	HIDESBASE void __fastcall Delete(int Index);
	void __fastcall Undo();
	void __fastcall Redo();
	__property TGLSMemoUndo* Items[int Index] = {read=Get/*, default*/};
	__property bool IsPerforming = {read=FIsPerforming, write=FIsPerforming, nodefault};
	__property TGLSCustomMemo* Memo = {read=FMemo, write=FMemo};
	__property int Pos = {read=FPos, write=FPos, nodefault};
	__property int Limit = {read=FLimit, write=SetLimit, nodefault};
};


typedef void __fastcall (__closure *TGutterClickEvent)(System::TObject* Sender, int LineNo);

typedef void __fastcall (__closure *TGutterDrawEvent)(System::TObject* Sender, Vcl::Graphics::TCanvas* ACanvas, int LineNo, const System::Types::TRect &rct);

typedef void __fastcall (__closure *TGetLineAttrsEvent)(System::TObject* Sender, int LineNo, System::UnicodeString &Attrs);

typedef void __fastcall (__closure *TUndoChangeEvent)(System::TObject* Sender, bool CanUndo, bool CanRedo);

enum DECLSPEC_DENUM TScrollMode : unsigned char { smAuto, smStrict };

class PASCALIMPLEMENTATION TGLSCustomMemo : public TGLSMemoAbstractScrollableObject
{
	typedef TGLSMemoAbstractScrollableObject inherited;
	
private:
	bool FAutoIndent;
	int FMargin;
	bool FHiddenCaret;
	bool FCaretVisible;
	TCellSize FCellSize;
	int FCurX;
	int FCurY;
	int FLeftCol;
	int FTopLine;
	int FTabSize;
	Vcl::Graphics::TFont* FFont;
	System::Uitypes::TColor FBkColor;
	System::Uitypes::TColor FSelColor;
	System::Uitypes::TColor FSelBkColor;
	bool FReadOnly;
	bool FDelErase;
	System::Classes::TStrings* FLines;
	int FSelStartX;
	int FSelStartY;
	int FSelEndX;
	int FSelEndY;
	int FPrevSelX;
	int FPrevSelY;
	System::Uitypes::TScrollStyle FScrollBars;
	int FScrollBarWidth;
	TGLSMemoGutter* FGutter;
	int FGutterWidth;
	TGLSMemoScrollBar* sbVert;
	TGLSMemoScrollBar* sbHorz;
	TStyleList* FStyles;
	Vcl::Graphics::TBitmap* FLineBitmap;
	TFullPos FSelCharPos;
	int FSelCharStyle;
	bool FLeftButtonDown;
	TScrollMode FScrollMode;
	TGLSMemoUndoList* FUndoList;
	TGLSMemoUndoList* FFirstUndoList;
	int FUndoLimit;
	int FLastMouseUpX;
	int FLastMouseUpY;
	bool FAfterDoubleClick;
	System::Classes::TNotifyEvent FOnMoveCursor;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnAttrChange;
	System::Classes::TNotifyEvent FOnStatusChange;
	System::Classes::TNotifyEvent FOnSelectionChange;
	TGutterDrawEvent FOnGutterDraw;
	TGutterClickEvent FOnGutterClick;
	TGetLineAttrsEvent FOnGetLineAttrs;
	TUndoChangeEvent FOnUndoChange;
	bool FHideCursor;
	void __fastcall SetHiddenCaret(bool Value);
	void __fastcall SetScrollBars(System::Uitypes::TScrollStyle Value);
	void __fastcall SetGutterWidth(int Value);
	void __fastcall SetGutterColor(System::Uitypes::TColor Value);
	System::Uitypes::TColor __fastcall GetGutterColor();
	void __fastcall SetCurX(int Value);
	void __fastcall SetCurY(int Value);
	HIDESBASE void __fastcall SetFont(Vcl::Graphics::TFont* Value);
	HIDESBASE void __fastcall SetColor(int Index, System::Uitypes::TColor Value);
	System::Types::TPoint __fastcall GetSelStart();
	System::Types::TPoint __fastcall GetSelEnd();
	void __fastcall SetLines(System::Classes::TStrings* ALines);
	void __fastcall SetLineStyle(int Index, int Value);
	int __fastcall GetLineStyle(int Index);
	bool __fastcall GetInComment(int Index);
	void __fastcall SetInComment(int Index, bool Value);
	int __fastcall GetInBrackets(int Index);
	void __fastcall SetInBrackets(int Index, int Value);
	bool __fastcall GetValidAttrs(int Index);
	void __fastcall SetValidAttrs(int Index, bool Value);
	System::UnicodeString __fastcall GetCharAttrs(int Index);
	void __fastcall SetCharAttrs(int Index, const System::UnicodeString Value);
	void __fastcall ExpandSelection();
	System::UnicodeString __fastcall GetSelText();
	void __fastcall SetSelText(const System::UnicodeString AValue);
	int __fastcall GetSelLength();
	void __fastcall MovePage(int dP, System::Classes::TShiftState Shift);
	void __fastcall ShowCaret(bool State);
	void __fastcall MakeVisible();
	int __fastcall GetVisible(int Index);
	int __fastcall MaxLength();
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Winapi::Messages::TWMSetCursor &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TMessage &Message);
	void __fastcall MoveCursor(int dX, int dY, System::Classes::TShiftState Shift);
	void __fastcall ResizeEditor();
	void __fastcall ResizeScrollBars();
	void __fastcall ResizeGutter();
	void __fastcall DoCommand(int cmd, const System::Classes::TShiftState AShift);
	void __fastcall DrawLine(int LineNo);
	bool __fastcall IsLineVisible(int LineNo);
	void __fastcall FreshLineBitmap();
	void __fastcall SetUndoLimit(int Value);
	
protected:
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	System::Types::TRect __fastcall EditorRect();
	System::Types::TRect __fastcall LineRangeRect(int FromLine, int ToLine);
	System::Types::TRect __fastcall ColRangeRect(int FromCol, int ToCol);
	void __fastcall InvalidateLineRange(int FromLine, int ToLine);
	int __fastcall AddString(const System::UnicodeString S);
	void __fastcall InsertString(int Index, System::UnicodeString S);
	void __fastcall GoHome(System::Classes::TShiftState Shift);
	void __fastcall GoEnd(System::Classes::TShiftState Shift);
	void __fastcall InsertChar(System::WideChar C);
	void __fastcall DeleteChar(int OldX, int OldY);
	void __fastcall DeleteLine(int Index, int OldX, int OldY, int NewX, int NewY, bool FixUndo);
	void __fastcall BackSpace();
	void __fastcall BackSpaceWord();
	System::UnicodeString __fastcall IndentCurrLine();
	void __fastcall NewLine();
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall Paint();
	void __fastcall DrawMargin();
	void __fastcall DrawGutter();
	void __fastcall DrawScrollBars();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall DblClick();
	virtual void __fastcall DoScroll(TGLSMemoScrollBar* Sender, int ByValue);
	virtual void __fastcall DoScrollPage(TGLSMemoScrollBar* Sender, int ByValue);
	__property int VisiblePosCount = {read=GetVisible, index=0, nodefault};
	__property int VisibleLineCount = {read=GetVisible, index=1, nodefault};
	__property int LastVisiblePos = {read=GetVisible, index=2, nodefault};
	__property int LastVisibleLine = {read=GetVisible, index=3, nodefault};
	void __fastcall DeleteSelection(bool bRepaint);
	HIDESBASE virtual void __fastcall Changed(int FromLine, int ToLine);
	virtual void __fastcall AttrChanged(int LineNo);
	virtual void __fastcall SelectionChanged();
	virtual void __fastcall StatusChanged();
	void __fastcall ClearUndoList();
	void __fastcall UndoChange();
	__property bool AutoIndent = {read=FAutoIndent, write=FAutoIndent, nodefault};
	__property int GutterWidth = {read=FGutterWidth, write=SetGutterWidth, nodefault};
	__property System::Uitypes::TColor GutterColor = {read=GetGutterColor, write=SetGutterColor, nodefault};
	__property System::Uitypes::TScrollStyle ScrollBars = {read=FScrollBars, write=SetScrollBars, default=3};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property bool ReadOnly = {read=FReadOnly, write=FReadOnly, nodefault};
	__property System::Classes::TStrings* Lines = {read=FLines, write=SetLines};
	__property System::Uitypes::TColor BkColor = {read=FBkColor, write=SetColor, index=0, nodefault};
	__property System::Uitypes::TColor SelColor = {read=FSelColor, write=SetColor, index=1, nodefault};
	__property System::Uitypes::TColor SelBkColor = {read=FSelBkColor, write=SetColor, index=2, nodefault};
	__property bool HiddenCaret = {read=FHiddenCaret, write=SetHiddenCaret, nodefault};
	__property int TabSize = {read=FTabSize, write=FTabSize, nodefault};
	__property TScrollMode ScrollMode = {read=FScrollMode, write=FScrollMode, default=0};
	__property int UndoLimit = {read=FUndoLimit, write=SetUndoLimit, nodefault};
	__property bool HideCursor = {read=FHideCursor, write=FHideCursor, nodefault};
	__property bool InComment[int Index] = {read=GetInComment, write=SetInComment};
	__property int InBrackets[int Index] = {read=GetInBrackets, write=SetInBrackets};
	__property bool ValidAttrs[int Index] = {read=GetValidAttrs, write=SetValidAttrs};
	__property System::UnicodeString CharAttrs[int Index] = {read=GetCharAttrs, write=SetCharAttrs};
	__property TGutterClickEvent OnGutterClick = {read=FOnGutterClick, write=FOnGutterClick};
	__property TGutterDrawEvent OnGutterDraw = {read=FOnGutterDraw, write=FOnGutterDraw};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnMoveCursor = {read=FOnMoveCursor, write=FOnMoveCursor};
	__property System::Classes::TNotifyEvent OnAttrChange = {read=FOnAttrChange, write=FOnAttrChange};
	__property System::Classes::TNotifyEvent OnSelectionChange = {read=FOnSelectionChange, write=FOnSelectionChange};
	__property System::Classes::TNotifyEvent OnStatusChange = {read=FOnStatusChange, write=FOnStatusChange};
	__property TGetLineAttrsEvent OnGetLineAttrs = {read=FOnGetLineAttrs, write=FOnGetLineAttrs};
	__property TUndoChangeEvent OnUndoChange = {read=FOnUndoChange, write=FOnUndoChange};
	
public:
	__fastcall virtual TGLSCustomMemo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSCustomMemo();
	void __fastcall CopyToClipBoard();
	void __fastcall PasteFromClipBoard();
	void __fastcall CutToClipBoard();
	void __fastcall SelectLines(int StartLine, int EndLine);
	void __fastcall SelectAll();
	__property System::Types::TPoint SelStart = {read=GetSelStart};
	__property System::Types::TPoint SelEnd = {read=GetSelEnd};
	__property System::UnicodeString Selection = {read=GetSelText, write=SetSelText};
	__property int SelLength = {read=GetSelLength, nodefault};
	void __fastcall ClearSelection();
	void __fastcall Clear();
	HIDESBASE void __fastcall SetCursor(int ACurX, int ACurY);
	int __fastcall SelectLine(int LineNo, int StyleNo);
	void __fastcall SelectChar(int LineNo, int Pos, int StyleNo);
	TCellPos __fastcall CellFromPos(int X, int Y);
	TFullPos __fastcall CharFromPos(int X, int Y);
	System::Types::TRect __fastcall CellRect(int ACol, int ARow);
	System::Types::TRect __fastcall LineRect(int ARow);
	System::Types::TRect __fastcall ColRect(int ACol);
	int __fastcall CharStyleNo(int LineNo, int Pos);
	void __fastcall InsertTemplate(System::UnicodeString AText);
	void __fastcall UnSelectChar();
	void __fastcall Undo();
	void __fastcall Redo();
	bool __fastcall CanUndo();
	bool __fastcall CanRedo();
	bool __fastcall FindText(System::UnicodeString Text, Vcl::Dialogs::TFindOptions Options, bool Select);
	__property int CurX = {read=FCurX, write=SetCurX, nodefault};
	__property int CurY = {read=FCurY, write=SetCurY, nodefault};
	__property bool DelErase = {read=FDelErase, write=FDelErase, nodefault};
	__property int LineStyle[int Index] = {read=GetLineStyle, write=SetLineStyle};
	__property TStyleList* Styles = {read=FStyles};
	__property TGLSMemoUndoList* UndoList = {read=FUndoList, write=FUndoList};
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSCustomMemo(HWND ParentWindow) : TGLSMemoAbstractScrollableObject(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TGLSMemo : public TGLSCustomMemo
{
	typedef TGLSCustomMemo inherited;
	
__published:
	__property PopupMenu;
	__property Align = {default=0};
	__property Enabled = {default=1};
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=0};
	__property Visible = {default=1};
	__property ReadOnly;
	__property AutoIndent;
	__property GutterColor;
	__property GutterWidth;
	__property ScrollBars = {default=3};
	__property Font;
	__property BkColor;
	__property Selection = {default=0};
	__property SelColor;
	__property SelBkColor;
	__property Lines;
	__property HiddenCaret;
	__property TabSize;
	__property ScrollMode = {default=0};
	__property UndoLimit;
	__property DelErase;
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnStartDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnGutterDraw;
	__property OnGutterClick;
	__property OnChange;
	__property OnMoveCursor;
	__property OnAttrChange;
	__property OnSelectionChange;
	__property OnStatusChange;
	__property OnGetLineAttrs;
	__property OnUndoChange;
public:
	/* TGLSCustomMemo.Create */ inline __fastcall virtual TGLSMemo(System::Classes::TComponent* AOwner) : TGLSCustomMemo(AOwner) { }
	/* TGLSCustomMemo.Destroy */ inline __fastcall virtual ~TGLSMemo() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSMemo(HWND ParentWindow) : TGLSCustomMemo(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TGLSMemoStringList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
private:
	void __fastcall ReadStrings(System::Classes::TReader* Reader);
	void __fastcall WriteStrings(System::Classes::TWriter* Writer);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
public:
	/* TStringList.Create */ inline __fastcall TGLSMemoStringList()/* overload */ : System::Classes::TStringList() { }
	/* TStringList.Create */ inline __fastcall TGLSMemoStringList(bool OwnsObjects)/* overload */ : System::Classes::TStringList(OwnsObjects) { }
	/* TStringList.Create */ inline __fastcall TGLSMemoStringList(System::WideChar QuoteChar, System::WideChar Delimiter)/* overload */ : System::Classes::TStringList(QuoteChar, Delimiter) { }
	/* TStringList.Create */ inline __fastcall TGLSMemoStringList(System::WideChar QuoteChar, System::WideChar Delimiter, System::Classes::TStringsOptions Options)/* overload */ : System::Classes::TStringList(QuoteChar, Delimiter, Options) { }
	/* TStringList.Create */ inline __fastcall TGLSMemoStringList(System::Types::TDuplicates Duplicates, bool Sorted, bool CaseSensitive)/* overload */ : System::Classes::TStringList(Duplicates, Sorted, CaseSensitive) { }
	/* TStringList.Destroy */ inline __fastcall virtual ~TGLSMemoStringList() { }
	
};


typedef System::Sysutils::TSysCharSet TDelimiters;

enum DECLSPEC_DENUM TTokenType : unsigned char { ttWord, ttBracket, ttSpecial, ttDelimiter, ttSpace, ttEOL, ttInteger, ttFloat, ttComment, ttOther, ttWrongNumber };

class PASCALIMPLEMENTATION TGLSSynHiMemo : public TGLSCustomMemo
{
	typedef TGLSCustomMemo inherited;
	
private:
	bool FIsPainting;
	bool FInComment;
	TGLSMemoStringList* FWordList;
	TGLSMemoStringList* FSpecialList;
	TGLSMemoStringList* FBracketList;
	System::Sysutils::TSysCharSet FDelimiters;
	int FInBrackets;
	System::UnicodeString FLineComment;
	System::UnicodeString FMultiCommentLeft;
	System::UnicodeString FMultiCommentRight;
	TCharStyle* FDelimiterStyle;
	TCharStyle* FCommentStyle;
	TCharStyle* FNumberStyle;
	int FDelimiterStyleNo;
	int FCommentStyleNo;
	int FNumberStyleNo;
	bool FCaseSensitive;
	System::UnicodeString __fastcall GetToken(const System::UnicodeString S, int &From, /* out */ TTokenType &TokenType, /* out */ int &StyleNo);
	void __fastcall SetWordList(TGLSMemoStringList* Value);
	void __fastcall SetSpecialList(TGLSMemoStringList* Value);
	void __fastcall SetBracketList(TGLSMemoStringList* Value);
	void __fastcall FindLineAttrs(System::TObject* Sender, int LineNo, System::UnicodeString &Attrs);
	void __fastcall SetStyle(int Index, TCharStyle* Value);
	void __fastcall SetCaseSensitive(bool Value);
	
protected:
	virtual void __fastcall Paint();
	
public:
	__fastcall virtual TGLSSynHiMemo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSSynHiMemo();
	void __fastcall AddWord(int StyleNo, const System::UnicodeString *ArrS, const int ArrS_High);
	void __fastcall AddSpecial(int StyleNo, const System::UnicodeString *ArrS, const int ArrS_High);
	void __fastcall AddBrackets(int StyleNo, const System::UnicodeString *ArrS, const int ArrS_High);
	__property System::Sysutils::TSysCharSet Delimiters = {read=FDelimiters, write=FDelimiters};
	
__published:
	__property PopupMenu;
	__property Align = {default=0};
	__property Enabled = {default=1};
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=0};
	__property Visible = {default=1};
	__property ReadOnly;
	__property AutoIndent;
	__property GutterColor;
	__property GutterWidth;
	__property ScrollBars = {default=3};
	__property Font;
	__property BkColor;
	__property SelColor;
	__property SelBkColor;
	__property Lines;
	__property HiddenCaret;
	__property TabSize;
	__property ScrollMode = {default=0};
	__property UndoLimit;
	__property DelErase;
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnStartDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnGutterClick;
	__property OnGutterDraw;
	__property OnChange;
	__property OnMoveCursor;
	__property OnSelectionChange;
	__property OnStatusChange;
	__property OnUndoChange;
	__property System::UnicodeString LineComment = {read=FLineComment, write=FLineComment};
	__property System::UnicodeString MultiCommentLeft = {read=FMultiCommentLeft, write=FMultiCommentLeft};
	__property System::UnicodeString MultiCommentRight = {read=FMultiCommentRight, write=FMultiCommentRight};
	__property TGLSMemoStringList* WordList = {read=FWordList, write=SetWordList};
	__property TGLSMemoStringList* SpecialList = {read=FSpecialList, write=SetSpecialList};
	__property TGLSMemoStringList* BracketList = {read=FBracketList, write=SetBracketList};
	__property TCharStyle* DelimiterStyle = {read=FDelimiterStyle, write=SetStyle, index=0};
	__property TCharStyle* CommentStyle = {read=FCommentStyle, write=SetStyle, index=1};
	__property TCharStyle* NumberStyle = {read=FNumberStyle, write=SetStyle, index=2};
	__property bool CaseSensitive = {read=FCaseSensitive, write=SetCaseSensitive, nodefault};
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSSynHiMemo(HWND ParentWindow) : TGLSCustomMemo(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Border(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &rct, TBorderType BorderType);
}	/* namespace Memo */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MEMO)
using namespace Gls::Memo;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_MemoHPP
