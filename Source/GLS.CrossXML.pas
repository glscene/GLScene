//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.CrossXML;

(* Cross XML routines *)

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  XMLIntf,
  XMLDoc,
  XMLDom;

type
  GLSXMLDocument = IXMLDocument;
  GLSXMLNode = IXMLNode;
  GLSDOMNode = IDOMNode;

function GLSNewXMLDocument: GLSXMLDocument;
procedure ReleaseXMLDocument(var ADoc: GLSXMLDocument);
procedure WriteXMLFile(var ADoc: GLSXMLDocument; AStream: TStream); overload;
procedure ReadXMLFile(var ADoc: GLSXMLDocument; AStream: TStream); overload;
procedure WriteXMLFile(var ADoc: GLSXMLDocument; AFileName: string); overload;
procedure ReadXMLFile(var ADoc: GLSXMLDocument; AFileName: string); overload;
function GetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; out Value: string): Boolean; overload;
function GetXMLAttribute(const XMLNode: GLSXMLNode; Idx: Integer): GLSXMLNode; overload;
procedure SetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; const Value: string); overload;
procedure SetXMLAttribute(const DOMNode: GLSDOMNode; const AttrName: string; const Value: string); overload;
function GetXMLAttributeCount(const XMLNode: GLSXMLNode): Integer;
function FindXMLNode(const ParentNode: GLSXMLNode; const NodeName: string; out ChildNode: GLSXMLNode): Boolean;
function CreateDOMNode(const ParentNode: GLSDOMNode; const NodeName: string): GLSDOMNode;
procedure SetXMLText(const DOMNode: GLSDOMNode; const AText: string);
function GetXMLText(const XMLNode: GLSXMLNode; out AText: string): Boolean;

//---------------------------------------------------------------------
implementation
//---------------------------------------------------------------------

function GLSNewXMLDocument: GLSXMLDocument;
begin
  Result := NewXMLDocument();
end;

procedure ReleaseXMLDocument(var ADoc: GLSXMLDocument);
begin
  ADoc := nil;
end;

procedure WriteXMLFile(var ADoc: GLSXMLDocument; AStream: TStream);
begin
  ADoc.SaveToStream(AStream);
end;

procedure ReadXMLFile(var ADoc: GLSXMLDocument; AStream: TStream);
begin
  ADoc.LoadFromStream(AStream);
end;

procedure WriteXMLFile(var ADoc: GLSXMLDocument; AFileName: string); overload;
begin
  ADoc.SaveToFile(AFileName);
end;

procedure ReadXMLFile(var ADoc: GLSXMLDocument; AFileName: string); overload;
begin
  ADoc.LoadFromFile(AFileName);
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; out Value: string): Boolean;
var
  attr: OleVariant;
begin
  attr := 0;
  attr := XMLNode.Attributes[AttrName];
  Result := not VarIsNull(attr);
  if Result then
    Value := attr;
end;

procedure SetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; const Value: string);
begin
  XMLNode.Attributes[AttrName] := Value;
end;

procedure SetXMLAttribute(const DOMNode: GLSDOMNode; const AttrName: string; const Value: string);
var
  E: IDOMElement;
begin
  E := DOMNode as IDOMElement;
  E.SetAttribute(AttrName, Value);
end;

function FindXMLNode(const ParentNode: GLSXMLNode; const NodeName: string; out ChildNode: GLSXMLNode): Boolean;
begin
  ChildNode := ParentNode.ChildNodes.FindNode(NodeName);
  Result := Assigned(ChildNode);
end;

function CreateDOMNode(const ParentNode: GLSDOMNode; const NodeName: string): GLSDOMNode;
begin
  Result := ParentNode.OwnerDocument.CreateElement(NodeName);
  ParentNode.AppendChild(Result);
end;

procedure SetXMLText(const DOMNode: GLSDOMNode; const AText: string);
begin
  DOMNode.AppendChild(DOMNode.ownerDocument.createTextNode(AText));
end;

function GetXMLText(const XMLNode: GLSXMLNode; out AText: string): Boolean;
begin
  AText := XMLNode.Text;
  Result := Length(AText)>0;
end;

function GetXMLAttributeCount(const XMLNode: GLSXMLNode): Integer;
begin
  Result := XMLNode.AttributeNodes.Count;
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; Idx: Integer): GLSXMLNode;
begin
  Result := XMLNode.AttributeNodes[Idx];
end;

end.
