//
// Graphic Scene Engine, http://glscene.org
//
(*
   A shader that passes control of the DoApply and DoUnApply
   methods through published events. This component is
   designed to make it a little easier to implement a
   customized shader. Be sure to keep the shader balanced
   by returning the OpenGL state to how you found it.
*)
unit GXSL.UserShader;

interface

uses
  System.Classes, 
  
  GXS.Material,
  GXS.RenderContextInfo;

type
  TOnDoApplyEvent = procedure (Sender : TObject; var rci : TgxRenderContextInfo) of Object;
  TOnDoUnApplyEvent = procedure (Sender : TObject; Pass:Integer; var rci : TgxRenderContextInfo; var Continue : Boolean) of Object;
  
  TgxUserShader = class(TgxShader)
    private
      FPass : Integer;
      FOnDoApply : TOnDoApplyEvent;
      FOnDoUnApply : TOnDoUnApplyEvent;
    protected
      procedure DoApply(var rci : TgxRenderContextInfo; Sender : TObject); override;
      function DoUnApply(var rci : TgxRenderContextInfo) : Boolean; override;
    published
      property OnDoApply : TOnDoApplyEvent read FOnDoApply write FOnDoApply;
      property OnDoUnApply : TOnDoUnApplyEvent read FOnDoUnApply write FOnDoUnApply;
      property ShaderStyle;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TgxUserShader ------------------
// ------------------

// DoApply
//
procedure TgxUserShader.DoApply(var rci: TgxRenderContextInfo; Sender : TObject);
begin
  FPass:=1;
  if Assigned(FOnDoApply) and (not (csDesigning in ComponentState)) then
    FOnDoApply(Self,rci);
end;

// DoUnApply
//
function TgxUserShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
begin
  Result:=False;
  if Assigned(FOnDoUnApply) and (not (csDesigning in ComponentState)) then begin
    FOnDoUnApply(Self,FPass,rci,Result);
    Inc(FPass);
  end;
end;

end.
