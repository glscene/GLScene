// DXPExpertUnit
{
   DXP cross-platform support for Delphi Expert.
   Integrates FreePascal support into the Delphi IDE.

   Licensed under MPL (http://www.mozilla.org/MPL/)

   Copyright 2003 - Eric Grange
}
unit DXPExpertUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, ToolsApi, DXPExpertModule, Menus;

type

   // TDXPExpert
   //
   TDXPExpert = class(TNotifierObject, IOTAWizard)
      private
         { Private Declarations }

      protected
         { Protected Declarations }
         // IOTAWizard
         procedure Execute;
         function GetIDString: string;
         function GetName: string;
         function GetState: TWizardState;

      public
         { Public Declarations }
         constructor Create;
         destructor Destroy; override;
  end;

function DMDXPExpertModule : TDMDXPExpertModule;

procedure Register;

// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------
implementation
// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------

uses Dialogs, FDXPCompileLog;

var
   vModule : TDMDXPExpertModule;

procedure Register;
begin
   RegisterPackageWizard(TDXPExpert.Create);
end;

// DMDXPExpertModule
//
function DMDXPExpertModule : TDMDXPExpertModule;
begin
   Result:=vModule;
end;

// ------------------
// ------------------ TDXPExpert ------------------
// ------------------

// Create
//
constructor TDXPExpert.Create;
begin
   vModule:=TDMDXPExpertModule.Create(nil);
   vModule.FOTAServices:=BorlandIDEServices as IOTAServices;
   vModule.FNTAServices:=BorlandIDEServices as INTAServices;
   vModule.HookIDE;
end;

// Destroy
//
destructor TDXPExpert.Destroy;
begin
   ReleaseDXPCompileLog;
   vModule.UnHookIDE;
   FreeAndNil(vModule);
   inherited;
end;

// Execute
//
procedure TDXPExpert.Execute;
begin
   ShowMessage('Execute');
end;

function TDXPExpert.GetIDString: string;
begin
   Result:='DXP.Expert';
end;

// GetName
//
function TDXPExpert.GetName: string;
begin
   Result:=ClassName;
end;

// GetState
//
function TDXPExpert.GetState: TWizardState;
begin
   Result:=[];
end;

end.
