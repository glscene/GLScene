//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLSL.UserShader;

(*
   A shader that passes control of the DoApply and DoUnApply
   methods through published events. This component is 
   designed to make it a little easier to implement a 
   customized shader. Be sure to keep the shader balanced
   by returning the OpenGL state to how you found it. 
*)

interface

uses
  System.Classes,
   
  GLS.Material, 
  GLS.RenderContextInfo;

type
  TOnDoApplyEvent = procedure (Sender : TObject; var rci : TGLRenderContextInfo) of Object;
  TOnDoUnApplyEvent = procedure (Sender : TObject; Pass:Integer; var rci : TGLRenderContextInfo; var Continue : Boolean) of Object;
  
  TGLUserShader = class(TGLShader)
    private
      FPass : Integer;
      FOnDoApply : TOnDoApplyEvent;
      FOnDoUnApply : TOnDoUnApplyEvent;
    protected
      procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
      function DoUnApply(var rci : TGLRenderContextInfo) : Boolean; override;
    published
      property OnDoApply : TOnDoApplyEvent read FOnDoApply write FOnDoApply;
      property OnDoUnApply : TOnDoUnApplyEvent read FOnDoUnApply write FOnDoUnApply;
      property ShaderStyle;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLUserShader ------------------
// ------------------

procedure TGLUserShader.DoApply(var rci: TGLRenderContextInfo; Sender : TObject);
begin
  FPass:=1;
  if Assigned(FOnDoApply) and (not (csDesigning in ComponentState)) then
    FOnDoApply(Self,rci);
end;

function TGLUserShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  Result:=False;
  if Assigned(FOnDoUnApply) and (not (csDesigning in ComponentState)) then begin
    FOnDoUnApply(Self,FPass,rci,Result);
    Inc(FPass);
  end;
end;

end.
