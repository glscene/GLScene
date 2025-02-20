//
// The graphics engine GLXEngine. The unit of GLScene for Delphi
//
unit GLS.XCollectionRegister;

(* Register TXCollection property editor *)

interface

{$I Stage.Defines.inc}

uses
  System.Classes,
  GLS.XCollection,

  DesignEditors,
  DesignIntf;

type
  TXCollectionProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses
  FmXCollectionEditor;


// ----------------- TXCollectionProperty ------------------------------------

function TXCollectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TXCollectionProperty.Edit;
begin
  with XCollectionEditorForm do
  begin
    SetXCollection(TXCollection(GetOrdValue), Self.Designer);
    Show;
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TXCollection), nil, '', TXCollectionProperty);
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

// class registrations

end.
