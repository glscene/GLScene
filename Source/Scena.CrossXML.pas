//
// The graphics engine GLScene https://github.com/glscene
//

unit Scena.CrossXML;

(* Cross XML routines *)

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Xml.XMLIntf,
  Xml.XMLDoc,
  Xml.XMLDom;

type
  ScenaXMLDocument = IXMLDocument;
  ScenaXMLNode = IXMLNode;
  ScenaDOMNode = IDOMNode;

function ScenaNewXMLDocument: ScenaXMLDocument;
procedure ReleaseXMLDocument(var ADoc: ScenaXMLDocument);
procedure WriteXMLFile(var ADoc: ScenaXMLDocument; AStream: TStream); overload;
procedure ReadXMLFile(var ADoc: ScenaXMLDocument; AStream: TStream); overload;
procedure WriteXMLFile(var ADoc: ScenaXMLDocument; AFileName: string); overload;
procedure ReadXMLFile(var ADoc: ScenaXMLDocument; AFileName: string); overload;
function GetXMLAttribute(const XMLNode: ScenaXMLNode; const AttrName: string; out Value: string): Boolean; overload;
function GetXMLAttribute(const XMLNode: ScenaXMLNode; Idx: Integer): ScenaXMLNode; overload;
procedure SetXMLAttribute(const XMLNode: ScenaXMLNode; const AttrName: string; const Value: string); overload;
procedure SetXMLAttribute(const DOMNode: ScenaDOMNode; const AttrName: string; const Value: string); overload;
function GetXMLAttributeCount(const XMLNode: ScenaXMLNode): Integer;
function FindXMLNode(const ParentNode: ScenaXMLNode; const NodeName: string; out ChildNode: ScenaXMLNode): Boolean;
function CreateDOMNode(const ParentNode: ScenaDOMNode; const NodeName: string): ScenaDOMNode;
procedure SetXMLText(const DOMNode: ScenaDOMNode; const AText: string);
function GetXMLText(const XMLNode: ScenaXMLNode; out AText: string): Boolean;

//---------------------------------------------------------------------
implementation
//---------------------------------------------------------------------

function ScenaNewXMLDocument: ScenaXMLDocument;
begin
  Result := NewXMLDocument();
end;

procedure ReleaseXMLDocument(var ADoc: ScenaXMLDocument);
begin
  ADoc := nil;
end;

procedure WriteXMLFile(var ADoc: ScenaXMLDocument; AStream: TStream);
begin
  ADoc.SaveToStream(AStream);
end;

procedure ReadXMLFile(var ADoc: ScenaXMLDocument; AStream: TStream);
begin
  ADoc.LoadFromStream(AStream);
end;

procedure WriteXMLFile(var ADoc: ScenaXMLDocument; AFileName: string); overload;
begin
  ADoc.SaveToFile(AFileName);
end;

procedure ReadXMLFile(var ADoc: ScenaXMLDocument; AFileName: string); overload;
begin
  ADoc.LoadFromFile(AFileName);
end;

function GetXMLAttribute(const XMLNode: ScenaXMLNode; const AttrName: string; out Value: string): Boolean;
var
  attr: OleVariant;
begin
  attr := 0;
  attr := XMLNode.Attributes[AttrName];
  Result := not VarIsNull(attr);
  if Result then
    Value := attr;
end;

procedure SetXMLAttribute(const XMLNode: ScenaXMLNode; const AttrName: string; const Value: string);
begin
  XMLNode.Attributes[AttrName] := Value;
end;

procedure SetXMLAttribute(const DOMNode: ScenaDOMNode; const AttrName: string; const Value: string);
var
  E: IDOMElement;
begin
  E := DOMNode as IDOMElement;
  E.SetAttribute(AttrName, Value);
end;

function FindXMLNode(const ParentNode: ScenaXMLNode; const NodeName: string; out ChildNode: ScenaXMLNode): Boolean;
begin
  ChildNode := ParentNode.ChildNodes.FindNode(NodeName);
  Result := Assigned(ChildNode);
end;

function CreateDOMNode(const ParentNode: ScenaDOMNode; const NodeName: string): ScenaDOMNode;
begin
  Result := ParentNode.OwnerDocument.CreateElement(NodeName);
  ParentNode.AppendChild(Result);
end;

procedure SetXMLText(const DOMNode: ScenaDOMNode; const AText: string);
begin
  DOMNode.AppendChild(DOMNode.ownerDocument.createTextNode(AText));
end;

function GetXMLText(const XMLNode: ScenaXMLNode; out AText: string): Boolean;
begin
  AText := XMLNode.Text;
  Result := Length(AText)>0;
end;

function GetXMLAttributeCount(const XMLNode: ScenaXMLNode): Integer;
begin
  Result := XMLNode.AttributeNodes.Count;
end;

function GetXMLAttribute(const XMLNode: ScenaXMLNode; Idx: Integer): ScenaXMLNode;
begin
  Result := XMLNode.AttributeNodes[Idx];
end;

end.
