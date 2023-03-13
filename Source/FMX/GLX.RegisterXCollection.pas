//
// Graphic Scene Engine, http://glscene.org
//
(*
  Register TXCollection property editor
*)
unit GLX.RegisterXCollection;

interface

{$I Scena.inc}

uses
  System.Classes,
  System.TypInfo,
  {
  DesignIntf,
  DesignEditors,
  }
  GLX.XCollection;

type
  TPropertyAttribute = (paValueList, paSubProperties, paDialog, paMultiSelect,
    paAutoUpdate, paSortList, paReadOnly, paRevertable, paFullWidthName,
    paVolatileSubProperties, paFMX, paNotNestable, paDisplayReadOnly,
    paCustomDropDown, paValueEditable);

  TPropertyAttributes = set of TPropertyAttribute;

  TClassProperty = class  //class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; //override;  <- not found in base class
    procedure GetProperties(Proc: TGetChildProc);
      //in VCL -> (Proc: TGetPropProc) //override; <- not found in base class
    function GetValue: string; //override;  <- not found in base class
  end;

	TXCollectionProperty = class(TClassProperty)
		public
      function GetAttributes: TPropertyAttributes; //override;  <- not found in base class
			procedure Edit; //override;  <- not found in base class
	end;


function GetOrdValueAt(Index: Integer): Longint;
function GetOrdValue: Longint;
procedure Register;

//===================================================================
implementation
//===================================================================

uses
  FXCollectionEditor;

function GetOrdValueAt(Index: Integer): Longint;
var
  FPropList: PInstPropList;
begin
  with FPropList^[Index] do Result := GetOrdProp(Instance, PropInfo);
end;

function GetOrdValue: Longint;
begin
  Result := GetOrdValueAt(0);
end;


procedure Register;
begin
  { TODO : E2003 Undeclared identifier: 'RegisterPropertyEditor' }
  (*
  RegisterPropertyEditor(TypeInfo(TXCollection), nil, '', TXCollectionProperty);
  *)
end;

//----------------- TXCollectionProperty ------------------------------------

function TXCollectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog];
end;

procedure TXCollectionProperty.Edit;
begin
   with XCollectionEditor do
   begin
      { TODO : E2003 Undeclared identifier: 'Designer' }
      (*SetXCollection(TXCollection(GetOrdValue), Self.Designer);*)
      Show;
   end;
end;

// ------------------------------------------------------------------

function TClassProperty.GetAttributes: TPropertyAttributes;
begin

end;

procedure TClassProperty.GetProperties(Proc: TGetChildProc);
begin
  inherited;

end;

function TClassProperty.GetValue: string;
begin

end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

	
   
end.
