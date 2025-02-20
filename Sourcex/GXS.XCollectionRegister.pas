//
// The graphics engine GLXEngine. The unit of GXScene for Delphi
//
unit GXS.XCollectionRegister;

(* Register TXCollection property editor *)
(* TODO *)

interface

uses
  System.Classes,
  System.TypInfo,

  // ToDo
///  DesignEditors,
///  DesignIntf,

    GXS.XCollection,
    FMxXCollectionEditor;

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

//----------------- TXCollectionProperty ------------------------------------

function TXCollectionProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paDialog];
end;

procedure TXCollectionProperty.Edit;
begin
   with FXCollectionEditor do begin
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



end.
