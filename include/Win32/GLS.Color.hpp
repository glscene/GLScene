// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Color.pas' rev: 35.00 (Windows)

#ifndef Gls_ColorHPP
#define Gls_ColorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Utils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Color
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLColor;
struct TGLColorEntry;
class DELPHICLASS TGLColorManager;
//-- type declarations -------------------------------------------------------
typedef Gls::Vectortypes::TVector4f *PGLColorVector;

typedef Gls::Vectortypes::TVector4f TGLColorVector;

typedef Gls::Vectortypes::TVector3b *PRGBColor;

typedef Gls::Vectortypes::TVector3b TRGBColor;

class PASCALIMPLEMENTATION TGLColor : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Gls::Vectortypes::TVector4f FColor;
	Gls::Vectortypes::TVector4f *FPDefaultColor;
	void __fastcall SetColorVector(const Gls::Vectortypes::TVector4f &aColor)/* overload */;
	void __fastcall SetColorComponent(int index, float value);
	float __fastcall GetColorComponent(const int index);
	void __fastcall SetAsWinColor(const System::Uitypes::TColor val);
	System::Uitypes::TColor __fastcall GetAsWinColor();
	void __fastcall SetDirectColorVector(const Gls::Vectortypes::TVector4f &aColor);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadData(System::Classes::TStream* Stream);
	void __fastcall WriteData(System::Classes::TStream* Stream);
	Gls::Vectortypes::TVector4f __fastcall GetHSVA();
	void __fastcall SetHSVA(const Gls::Vectortypes::TVector4f &hsva);
	
public:
	__fastcall virtual TGLColor(System::Classes::TPersistent* AOwner);
	__fastcall TGLColor(System::Classes::TPersistent* AOwner, const Gls::Vectortypes::TVector4f &Color, System::Classes::TNotifyEvent changeEvent);
	__fastcall virtual ~TGLColor();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Initialize(const Gls::Vectortypes::TVector4f &color);
	System::PSingle __fastcall AsAddress();
	void __fastcall RandomColor();
	void __fastcall SetColor(float Red, float Green, float Blue, float Alpha = 1.000000E+00f)/* overload */;
	__property Gls::Vectortypes::TVector4f Color = {read=FColor, write=SetColorVector};
	__property Gls::Vectortypes::TVector4f DirectColor = {read=FColor, write=SetDirectColorVector};
	__property System::Uitypes::TColor AsWinColor = {read=GetAsWinColor, write=SetAsWinColor, nodefault};
	__property Gls::Vectortypes::TVector4f hsva = {read=GetHSVA, write=SetHSVA};
	__property Gls::Vectortypes::TVector4f DefaultColor = {read=FColor};
	
__published:
	__property float Red = {read=GetColorComponent, write=SetColorComponent, stored=false, index=0};
	__property float Green = {read=GetColorComponent, write=SetColorComponent, stored=false, index=1};
	__property float Blue = {read=GetColorComponent, write=SetColorComponent, stored=false, index=2};
	__property float Alpha = {read=GetColorComponent, write=SetColorComponent, stored=false, index=3};
};


typedef TGLColorEntry *PGLColorEntry;

struct DECLSPEC_DRECORD TGLColorEntry
{
public:
	System::UnicodeString Name;
	Gls::Vectortypes::TVector4f Color;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLColorManager : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	__fastcall virtual ~TGLColorManager();
	void __fastcall AddColor(const System::UnicodeString aName, const Gls::Vectortypes::TVector4f &aColor);
	void __fastcall EnumColors(System::Classes::TGetStrProc Proc)/* overload */;
	void __fastcall EnumColors(System::Classes::TStrings* AValues)/* overload */;
	Gls::Vectortypes::TVector4f __fastcall FindColor(const System::UnicodeString aName);
	Gls::Vectortypes::TVector4f __fastcall GetColor(const System::UnicodeString aName);
	System::UnicodeString __fastcall GetColorName(const Gls::Vectortypes::TVector4f &aColor);
	void __fastcall RegisterDefaultColors();
	void __fastcall RemoveColor(const System::UnicodeString aName);
public:
	/* TObject.Create */ inline __fastcall TGLColorManager() : System::Classes::TList() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Uitypes::TColor clForeground = System::Uitypes::TColor(-1);
static const System::Uitypes::TColor clButton = System::Uitypes::TColor(-2);
static const System::Uitypes::TColor clLight = System::Uitypes::TColor(-3);
static const System::Uitypes::TColor clMidlight = System::Uitypes::TColor(-4);
static const System::Uitypes::TColor clDark = System::Uitypes::TColor(-5);
static const System::Uitypes::TColor clMid = System::Uitypes::TColor(-6);
static const System::Uitypes::TColor clText = System::Uitypes::TColor(-7);
static const System::Uitypes::TColor clBrightText = System::Uitypes::TColor(-8);
static const System::Uitypes::TColor clButtonText = System::Uitypes::TColor(-9);
static const System::Uitypes::TColor clBase = System::Uitypes::TColor(-10);
static const System::Uitypes::TColor clBackground = System::Uitypes::TColor(-11);
static const System::Uitypes::TColor clShadow = System::Uitypes::TColor(-12);
static const System::Uitypes::TColor clHighlight = System::Uitypes::TColor(-13);
static const System::Uitypes::TColor clHighlightedText = System::Uitypes::TColor(-14);
static const System::Int8 cloNormal = System::Int8(0x20);
static const System::Int8 cloDisabled = System::Int8(0x40);
static const System::Int8 cloActive = System::Int8(0x60);
static const System::Uitypes::TColor clNormalForeground = System::Uitypes::TColor(-33);
static const System::Uitypes::TColor clNormalButton = System::Uitypes::TColor(-34);
static const System::Uitypes::TColor clNormalLight = System::Uitypes::TColor(-35);
static const System::Uitypes::TColor clNormalMidlight = System::Uitypes::TColor(-36);
static const System::Uitypes::TColor clNormalDark = System::Uitypes::TColor(-37);
static const System::Uitypes::TColor clNormalMid = System::Uitypes::TColor(-38);
static const System::Uitypes::TColor clNormalText = System::Uitypes::TColor(-39);
static const System::Uitypes::TColor clNormalBrightText = System::Uitypes::TColor(-40);
static const System::Uitypes::TColor clNormalButtonText = System::Uitypes::TColor(-41);
static const System::Uitypes::TColor clNormalBase = System::Uitypes::TColor(-42);
static const System::Uitypes::TColor clNormalBackground = System::Uitypes::TColor(-43);
static const System::Uitypes::TColor clNormalShadow = System::Uitypes::TColor(-44);
static const System::Uitypes::TColor clNormalHighlight = System::Uitypes::TColor(-45);
static const System::Uitypes::TColor clNormalHighlightedText = System::Uitypes::TColor(-46);
static const System::Uitypes::TColor clDisabledForeground = System::Uitypes::TColor(-65);
static const System::Uitypes::TColor clDisabledButton = System::Uitypes::TColor(-66);
static const System::Uitypes::TColor clDisabledLight = System::Uitypes::TColor(-67);
static const System::Uitypes::TColor clDisabledMidlight = System::Uitypes::TColor(-68);
static const System::Uitypes::TColor clDisabledDark = System::Uitypes::TColor(-69);
static const System::Uitypes::TColor clDisabledMid = System::Uitypes::TColor(-70);
static const System::Uitypes::TColor clDisabledText = System::Uitypes::TColor(-71);
static const System::Uitypes::TColor clDisabledBrightText = System::Uitypes::TColor(-72);
static const System::Uitypes::TColor clDisabledButtonText = System::Uitypes::TColor(-73);
static const System::Uitypes::TColor clDisabledBase = System::Uitypes::TColor(-74);
static const System::Uitypes::TColor clDisabledBackground = System::Uitypes::TColor(-75);
static const System::Uitypes::TColor clDisabledShadow = System::Uitypes::TColor(-76);
static const System::Uitypes::TColor clDisabledHighlight = System::Uitypes::TColor(-77);
static const System::Uitypes::TColor clDisabledHighlightedText = System::Uitypes::TColor(-78);
static const System::Uitypes::TColor clActiveForeground = System::Uitypes::TColor(-97);
static const System::Uitypes::TColor clActiveButton = System::Uitypes::TColor(-98);
static const System::Uitypes::TColor clActiveLight = System::Uitypes::TColor(-99);
static const System::Uitypes::TColor clActiveMidlight = System::Uitypes::TColor(-100);
static const System::Uitypes::TColor clActiveDark = System::Uitypes::TColor(-101);
static const System::Uitypes::TColor clActiveMid = System::Uitypes::TColor(-102);
static const System::Uitypes::TColor clActiveText = System::Uitypes::TColor(-103);
static const System::Uitypes::TColor clActiveBrightText = System::Uitypes::TColor(-104);
static const System::Uitypes::TColor clActiveButtonText = System::Uitypes::TColor(-105);
static const System::Uitypes::TColor clActiveBase = System::Uitypes::TColor(-106);
static const System::Uitypes::TColor clActiveBackground = System::Uitypes::TColor(-107);
static const System::Uitypes::TColor clActiveShadow = System::Uitypes::TColor(-108);
static const System::Uitypes::TColor clActiveHighlight = System::Uitypes::TColor(-109);
static const System::Uitypes::TColor clActiveHighlightedText = System::Uitypes::TColor(-110);
static const System::Uitypes::TColor clFirstSpecialColor = System::Uitypes::TColor(-110);
static const int clMask = int(16777215);
static const int clDontMask = int(0);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrScrollBar;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBackground;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrActiveCaption;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrInactiveCaption;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMenu;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrWindow;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrWindowFrame;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMenuText;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrWindowText;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrCaptionText;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrActiveBorder;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrInactiveBorder;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrAppWorkSpace;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrHighlight;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrHighlightText;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBtnFace;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBtnShadow;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGrayText;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBtnText;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrInactiveCaptionText;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBtnHighlight;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clr3DDkShadow;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clr3DLight;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrInfoText;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrInfoBk;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrTransparent;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBlack;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray05;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray10;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray15;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray20;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray25;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray30;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray35;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray40;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray45;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray50;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray55;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray60;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray65;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray70;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray75;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray80;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray85;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray90;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray95;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrWhite;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDimGray;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGray;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrLightGray;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrAqua;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrAquamarine;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBakersChoc;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBlueViolet;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBrown;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrCadetBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrCoral;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrCornflowerBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDarkGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDarkOliveGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDarkOrchid;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDarkSlateBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDarkSlateGray;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDarkSlateGrey;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDarkTurquoise;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrFirebrick;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrForestGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrFuchsia;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGold;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGoldenrod;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGreenYellow;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrIndian;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrKhaki;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrLightBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrLightSteelBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrLime;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrLimeGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMaroon;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumAquamarine;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumForestGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumGoldenrod;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumOrchid;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumSeaGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumSlateBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumSpringGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumTurquoise;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumViolet;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumPurple;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMidnightBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrNavy;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrNavyBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrOrange;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrOrangeRed;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrOrchid;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrPaleGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrPink;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrPlum;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrSalmon;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrSeaGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrSienna;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrSkyBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrSlateBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrSpringGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrSteelBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrTan;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrThistle;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrTurquoise;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrViolet;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrVioletRed;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrYellowGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrSummerSky;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrRichBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBrass;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrCopper;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBronze;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBronze2;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrSilver;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrBrightGold;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrOldGold;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrFeldspar;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrQuartz;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrNeonPink;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDarkPurple;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrNeonBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrCoolCopper;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMandarinOrange;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrLightWood;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrMediumWood;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDarkWood;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrSpicyPink;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrSemiSweetChoc;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrFlesh;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrNewTan;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrNewMidnightBlue;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrVeryDarkBrown;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDarkBrown;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDarkTan;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGreenCopper;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDkGreenCopper;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrDustyRose;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrHuntersGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrScarlet;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrLightPurple;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrVeryLightPurple;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrGreen;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrOlive;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrPurple;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrTeal;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrRed;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrYellow;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f clrWheat;
#define cDefaultNormalMapScale  (1.250000E-01)
extern DELPHI_PACKAGE bool vUseDefaultColorSets;
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall RGB2Color(const System::Byte r, const System::Byte g, const System::Byte b);
extern DELPHI_PACKAGE TGLColorManager* __fastcall ColorManager(void);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall ConvertWinColor(System::Uitypes::TColor aColor, float Alpha = 1.000000E+00f);
extern DELPHI_PACKAGE void __fastcall InitGLSceneColors(void);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall ConvertColorVector(const Gls::Vectortypes::TVector4f &aColor)/* overload */;
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall ConvertColorVector(const Gls::Vectortypes::TVector4f &aColor, float intensity)/* overload */;
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall ConvertRGBColor(const System::Byte *aColor, const int aColor_High);
extern DELPHI_PACKAGE void __fastcall RegisterColor(const System::UnicodeString aName, const Gls::Vectortypes::TVector4f &aColor);
extern DELPHI_PACKAGE void __fastcall UnRegisterColor(const System::UnicodeString aName);
}	/* namespace Color */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_COLOR)
using namespace Gls::Color;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ColorHPP
