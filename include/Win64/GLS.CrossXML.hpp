// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.CrossXML.pas' rev: 35.00 (Windows)

#ifndef Gls_CrossxmlHPP
#define Gls_CrossxmlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <Xml.XMLIntf.hpp>
#include <Xml.XMLDoc.hpp>
#include <Xml.xmldom.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Crossxml
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef Xml::Xmlintf::_di_IXMLDocument GLSXMLDocument;

typedef Xml::Xmlintf::_di_IXMLNode GLSXMLNode;

typedef Xml::Xmldom::_di_IDOMNode GLSDOMNode;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Xml::Xmlintf::_di_IXMLDocument __fastcall GLSNewXMLDocument(void);
extern DELPHI_PACKAGE void __fastcall ReleaseXMLDocument(Xml::Xmlintf::_di_IXMLDocument &ADoc);
extern DELPHI_PACKAGE void __fastcall WriteXMLFile(Xml::Xmlintf::_di_IXMLDocument &ADoc, System::Classes::TStream* AStream)/* overload */;
extern DELPHI_PACKAGE void __fastcall ReadXMLFile(Xml::Xmlintf::_di_IXMLDocument &ADoc, System::Classes::TStream* AStream)/* overload */;
extern DELPHI_PACKAGE void __fastcall WriteXMLFile(Xml::Xmlintf::_di_IXMLDocument &ADoc, System::UnicodeString AFileName)/* overload */;
extern DELPHI_PACKAGE void __fastcall ReadXMLFile(Xml::Xmlintf::_di_IXMLDocument &ADoc, System::UnicodeString AFileName)/* overload */;
extern DELPHI_PACKAGE bool __fastcall GetXMLAttribute(const Xml::Xmlintf::_di_IXMLNode XMLNode, const System::UnicodeString AttrName, /* out */ System::UnicodeString &Value)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetXMLAttribute(const Xml::Xmlintf::_di_IXMLNode XMLNode, const System::UnicodeString AttrName, const System::UnicodeString Value)/* overload */;
extern DELPHI_PACKAGE void __fastcall SetXMLAttribute(const Xml::Xmldom::_di_IDOMNode DOMNode, const System::UnicodeString AttrName, const System::UnicodeString Value)/* overload */;
extern DELPHI_PACKAGE bool __fastcall FindXMLNode(const Xml::Xmlintf::_di_IXMLNode ParentNode, const System::UnicodeString NodeName, /* out */ Xml::Xmlintf::_di_IXMLNode &ChildNode);
extern DELPHI_PACKAGE Xml::Xmldom::_di_IDOMNode __fastcall CreateDOMNode(const Xml::Xmldom::_di_IDOMNode ParentNode, const System::UnicodeString NodeName);
extern DELPHI_PACKAGE void __fastcall SetXMLText(const Xml::Xmldom::_di_IDOMNode DOMNode, const System::UnicodeString AText);
extern DELPHI_PACKAGE bool __fastcall GetXMLText(const Xml::Xmlintf::_di_IXMLNode XMLNode, /* out */ System::UnicodeString &AText);
extern DELPHI_PACKAGE int __fastcall GetXMLAttributeCount(const Xml::Xmlintf::_di_IXMLNode XMLNode);
extern DELPHI_PACKAGE Xml::Xmlintf::_di_IXMLNode __fastcall GetXMLAttribute(const Xml::Xmlintf::_di_IXMLNode XMLNode, int Idx)/* overload */;
}	/* namespace Crossxml */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_CROSSXML)
using namespace Gls::Crossxml;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_CrossxmlHPP
