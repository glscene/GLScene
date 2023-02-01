// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Console.pas' rev: 35.00 (Windows)

#ifndef Gls_ConsoleHPP
#define Gls_ConsoleHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.TypInfo.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.Objects.hpp>
#include <GLS.HUDObjects.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.BitmapFont.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.Context.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Utils.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Material.hpp>
#include <GLS.VectorTypes.hpp>
#include <System.UITypes.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Console
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLConsoleException;
struct TGLUserInputCommand;
class DELPHICLASS TGLConsoleStringList;
class DELPHICLASS TGLConsoleCommand;
class DELPHICLASS TGLConsoleCommandList;
class DELPHICLASS TGLConsoleControls;
class DELPHICLASS TGLCustomConsole;
class DELPHICLASS TGLConsole;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION EGLConsoleException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLConsoleException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLConsoleException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLConsoleException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLConsoleException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLConsoleException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLConsoleException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLConsoleException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLConsoleException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLConsoleException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLConsoleException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLConsoleException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLConsoleException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLConsoleException() { }
	
};


enum DECLSPEC_DENUM TGLConsoleOption : unsigned char { coAutoCompleteCommandsOnKeyPress, coAutoCompleteCommandsOnEnter, coShowConsoleHelpIfUnknownCommand, coRemoveQuotes };

typedef System::Set<TGLConsoleOption, TGLConsoleOption::coAutoCompleteCommandsOnKeyPress, TGLConsoleOption::coRemoveQuotes> TGLConsoleOptions;

struct DECLSPEC_DRECORD TGLUserInputCommand
{
	
private:
	typedef System::DynamicArray<System::UnicodeString> _TGLUserInputCommand__1;
	
	
public:
	int CommandCount;
	_TGLUserInputCommand__1 Strings;
	bool UnknownCommand;
};


typedef void __fastcall (__closure *TGLlConsoleEvent)(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);

typedef System::Set<System::Int8, 0, 120> TGLConsoleMatchList;

class PASCALIMPLEMENTATION TGLConsoleStringList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
private:
	TGLCustomConsole* FConsole;
	
protected:
	virtual void __fastcall Changed();
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	bool __fastcall CommandExists(const System::UnicodeString Command);
	__fastcall TGLConsoleStringList(TGLCustomConsole* const Owner);
public:
	/* TStringList.Destroy */ inline __fastcall virtual ~TGLConsoleStringList() { }
	
};


class PASCALIMPLEMENTATION TGLConsoleCommand : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	bool FVisible;
	bool FEnabled;
	bool FSilentDisabled;
	TGLConsoleCommandList* FCommandList;
	System::UnicodeString FCommandName;
	System::UnicodeString FShortHelp;
	System::Classes::TStringList* FLongHelp;
	TGLlConsoleEvent FOnCommand;
	System::Classes::TNotifyEvent FOnHelp;
	void __fastcall SetCommandName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall ShowInvalidUseOfCommandError();
	virtual void __fastcall ShowInvalidNumberOfArgumentsError(const bool ShowHelpAfter = true);
	virtual void __fastcall DoOnCommand(TGLUserInputCommand &UserInputCommand);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	virtual void __fastcall ShowHelp();
	virtual void __fastcall ShowShortHelp();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TGLConsoleCommand(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLConsoleCommand();
	
__published:
	__property System::UnicodeString CommandName = {read=FCommandName, write=SetCommandName};
	__property System::UnicodeString ShortHelp = {read=FShortHelp, write=FShortHelp};
	__property System::Classes::TStringList* LongHelp = {read=FLongHelp};
	__property TGLlConsoleEvent OnCommand = {read=FOnCommand, write=FOnCommand};
	__property System::Classes::TNotifyEvent OnHelp = {read=FOnHelp, write=FOnHelp};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
	__property bool SilentDisabled = {read=FSilentDisabled, write=FSilentDisabled, default=0};
	__property bool Visible = {read=FVisible, write=FVisible, default=1};
};


class PASCALIMPLEMENTATION TGLConsoleCommandList : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLConsoleCommand* operator[](const int Index) { return this->Items[Index]; }
	
private:
	TGLCustomConsole* FConsole;
	TGLConsoleCommand* __fastcall GetItems(const int Index);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	void __fastcall SortCommands(const bool Ascending = true);
	bool __fastcall CommandExists(const System::UnicodeString Command);
	int __fastcall GetCommandIndex(const System::UnicodeString Command);
	TGLConsoleCommand* __fastcall LastConsoleCommand();
	HIDESBASE TGLConsoleCommand* __fastcall Add()/* overload */;
	__fastcall TGLConsoleCommandList(TGLCustomConsole* const AOwner);
	__fastcall virtual ~TGLConsoleCommandList();
	__property TGLConsoleCommand* Items[const int Index] = {read=GetItems/*, default*/};
};


class PASCALIMPLEMENTATION TGLConsoleControls : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	System::Byte FNavigatePageUp;
	System::Byte FAutoCompleteCommand;
	System::Byte FPreviousCommand;
	System::Byte FNextCommand;
	System::Byte FNavigateUp;
	System::Byte FNavigatePageDown;
	System::Byte FNavigateDown;
	int FDblClickDelay;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall TGLConsoleControls(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property System::Byte NavigateUp = {read=FNavigateUp, write=FNavigateUp, default=36};
	__property System::Byte NavigateDown = {read=FNavigateDown, write=FNavigateDown, default=35};
	__property System::Byte NavigatePageUp = {read=FNavigatePageUp, write=FNavigatePageUp, default=33};
	__property System::Byte NavigatePageDown = {read=FNavigatePageDown, write=FNavigatePageDown, default=34};
	__property System::Byte NextCommand = {read=FNextCommand, write=FNextCommand, default=40};
	__property System::Byte PreviousCommand = {read=FPreviousCommand, write=FPreviousCommand, default=38};
	__property System::Byte AutoCompleteCommand = {read=FAutoCompleteCommand, write=FAutoCompleteCommand, default=17};
	__property int DblClickDelay = {read=FDblClickDelay, write=FDblClickDelay, default=300};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLConsoleControls() { }
	
};


class PASCALIMPLEMENTATION TGLCustomConsole : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
private:
	Gls::Hudobjects::TGLHUDSprite* FHudSprite;
	Gls::Hudobjects::TGLHUDText* FHudText;
	Gls::Sceneviewer::TGLSceneViewer* FSceneViewer;
	System::UnicodeString FInputLine;
	int FStartLine;
	int FCurrentCommand;
	int FPreviousTickCount;
	float FSize;
	System::Classes::TStringList* FColsoleLog;
	TGLConsoleCommandList* FCommands;
	TGLConsoleStringList* FAdditionalCommands;
	System::Classes::TStringList* FTypedCommands;
	TGLConsoleControls* FControls;
	TGLlConsoleEvent FOnCommandIssued;
	TGLConsoleOptions FOptions;
	System::UnicodeString FHint;
	void __fastcall SetSize(const float Value);
	void __fastcall SetSceneViewer(Gls::Sceneviewer::TGLSceneViewer* const Value);
	Gls::Bitmapfont::TGLCustomBitmapFont* __fastcall GetFont();
	void __fastcall SetFont(Gls::Bitmapfont::TGLCustomBitmapFont* const Value);
	
protected:
	virtual void __fastcall DoOnCommandIssued(TGLUserInputCommand &UserInputCommand);
	virtual void __fastcall SetFontColor(const System::Uitypes::TColor Color);
	System::Uitypes::TColor __fastcall GetFontColor();
	virtual void __fastcall SetHUDSpriteColor(const System::Uitypes::TColor Color);
	System::Uitypes::TColor __fastcall GetHUDSpriteColor();
	int __fastcall NumLines();
	virtual void __fastcall ShowConsoleHelp();
	virtual void __fastcall HandleUnknownCommand(const System::UnicodeString Command);
	virtual void __fastcall AutoCompleteCommand()/* overload */;
	void __fastcall AutoCompleteCommand(int &MatchCount, TGLConsoleMatchList &AdditionalCommandsMatchList, TGLConsoleMatchList &CommandsMatchList)/* overload */;
	virtual void __fastcall CommandIssued(TGLUserInputCommand &UserInputCommand);
	virtual void __fastcall FixCommand(TGLUserInputCommand &UserInputCommand);
	TGLUserInputCommand __fastcall ParseString(System::UnicodeString str, System::UnicodeString caract);
	virtual void __fastcall ProcessInput();
	virtual void __fastcall RefreshHud();
	virtual void __fastcall RegisterBuiltInCommands();
	virtual void __fastcall ProcessInternalCommandHelp(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandClearScreen(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandConsoleHide(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandConsoleColor(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandConsoleRename(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandConsoleClearTypedCommands(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandSystemTime(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandSystemDate(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandViewerFPS(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandViewerResetPerformanceMonitor(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandViewerVSync(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandViewerAntiAliasing(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall GetHelpInternalCommandRename(System::TObject* Sender);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall SetName(const System::Classes::TComponentName Value);
	
public:
	virtual void __fastcall ProcessKeyPress(const System::WideChar c);
	virtual void __fastcall ProcessKeyDown(const System::Word key);
	void __fastcall NavigateUp();
	void __fastcall NavigateDown();
	void __fastcall NavigatePageUp();
	void __fastcall NavigatePageDown();
	virtual void __fastcall RefreshHudSize();
	void __fastcall AddLine(const System::UnicodeString str);
	void __fastcall ClearTypedCommands();
	void __fastcall ExecuteCommand(const System::UnicodeString Command);
	void __fastcall ExecuteCommands(System::Classes::TStrings* const Commands);
	__fastcall virtual TGLCustomConsole(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomConsole();
	__property System::Uitypes::TColor FontColor = {read=GetFontColor, write=SetFontColor, stored=false, nodefault};
	__property System::Uitypes::TColor HUDSpriteColor = {read=GetHUDSpriteColor, write=SetHUDSpriteColor, stored=false, nodefault};
	__property System::UnicodeString InputLine = {read=FInputLine, write=FInputLine};
	__property System::Classes::TStringList* TypedCommands = {read=FTypedCommands};
	__property TGLConsoleCommandList* Commands = {read=FCommands};
	__property TGLConsoleStringList* AdditionalCommands = {read=FAdditionalCommands};
	__property TGLConsoleControls* Controls = {read=FControls};
	__property System::Classes::TStringList* ColsoleLog = {read=FColsoleLog};
	__property float Size = {read=FSize, write=SetSize};
	__property Gls::Sceneviewer::TGLSceneViewer* SceneViewer = {read=FSceneViewer, write=SetSceneViewer};
	__property Gls::Hudobjects::TGLHUDSprite* HudSprite = {read=FHudSprite};
	__property Gls::Hudobjects::TGLHUDText* HudText = {read=FHudText};
	__property Gls::Bitmapfont::TGLCustomBitmapFont* Font = {read=GetFont, write=SetFont, stored=false};
	__property TGLConsoleOptions Options = {read=FOptions, write=FOptions, nodefault};
	__property TGLlConsoleEvent OnCommandIssued = {read=FOnCommandIssued, write=FOnCommandIssued};
	__property System::UnicodeString Hint = {read=FHint, write=FHint};
	__property Visible = {default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCustomConsole(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLConsole : public TGLCustomConsole
{
	typedef TGLCustomConsole inherited;
	
__published:
	__property FontColor;
	__property HUDSpriteColor;
	__property InputLine = {default=0};
	__property TypedCommands;
	__property Commands;
	__property AdditionalCommands;
	__property Controls;
	__property ColsoleLog;
	__property SceneViewer;
	__property HudSprite;
	__property HudText;
	__property Font;
	__property Options;
	__property OnCommandIssued;
	__property Hint = {default=0};
	__property Tag = {default=0};
	__property ObjectsSorting = {default=0};
	__property Visible = {default=0};
	__property OnProgress;
public:
	/* TGLCustomConsole.Create */ inline __fastcall virtual TGLConsole(System::Classes::TComponent* AOwner) : TGLCustomConsole(AOwner) { }
	/* TGLCustomConsole.Destroy */ inline __fastcall virtual ~TGLConsole() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLConsole(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLCustomConsole(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 CONSOLE_MAX_COMMANDS = System::Int8(0x78);
}	/* namespace Console */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_CONSOLE)
using namespace Gls::Console;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ConsoleHPP
