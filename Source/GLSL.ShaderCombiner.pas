//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLSL.ShaderCombiner;

(*
   Allows to combine shaders in different sequences.
   Note, that can't just take 2 random shaders and combine them, because
   shaders often override the objects material and vertex data with a total
   disregard to what existed before it. But in some cases, especially with
   multipass shaders, this unit does magic and allows to reuse and upgrade
   previously written shaders.
*)

interface

{$I GLScene.inc}

uses
  System.Classes,

  GLS.Material,
  GLS.Scene,
  GLS.VectorGeometry,
  GLS.Strings,
  GLS.RenderContextInfo;

type
  (*
   MP - multipass, SP-singlepass, AP - anypass (single or multi)
     One-Two or Two-One determines the order of how the shaders should be applied
     For example, sctTwoMPOneSP means that first one will be applied Shader Two,
     which can be a multipass shader, then Shader One is applied, which should be
     a singlepass shader.
     sctOneMPTwoSP and sctTwoMPOneSP modes are actualy quite Str@nge,
                                       because... well look at the code yourself

     TODO: Add more modes here, including sctOneAPTwoAP, which should be the
           default one.
  *)
  TGLShaderCombinerType = (sctOneSPTwoAP, sctTwoSPOneAP,
                           sctOneMPTwoSP, sctTwoMPOneSP);

  TGLCustomShaderCombiner = class(TGLShader)
  private
    FCurrentPass: Integer;
    FCombinerType: TGLShaderCombinerType;
    FShaderOne: TGLShader;
    FShaderTwo: TGLShader;
    procedure SetShaderOne(const Value: TGLShader);
    procedure SetShaderTwo(const Value: TGLShader);
  protected
    procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property CombinerType: TGLShaderCombinerType read FCombinerType write FCombinerType default sctOneSPTwoAP;
    property ShaderOne: TGLShader read FShaderOne write SetShaderOne;
    property ShaderTwo: TGLShader read FShaderTwo write SetShaderTwo;
    property CurrentPass : Integer read FCurrentPass stored False;
  public
    constructor Create(AOwner: TComponent); override;
    function ShaderSupported: Boolean; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TGLShaderCombiner = class(TGLCustomShaderCombiner)
  published
    property CombinerType;
    property ShaderOne;
    property ShaderTwo;
    property ShaderStyle;
  end;

//---------------------------------------------
implementation
//---------------------------------------------

//-----------------------------
// TGLCustomShaderCombiner
//-----------------------------

procedure TGLCustomShaderCombiner.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGLCustomShaderCombiner then
  begin
    SetShaderOne(TGLCustomShaderCombiner(Source).FShaderOne);
    SetShaderTwo(TGLCustomShaderCombiner(Source).FShaderTwo);
  end;
end;

constructor TGLCustomShaderCombiner.Create(AOwner: TComponent);
begin
  inherited;
  FCombinerType := sctOneSPTwoAP;
end;

procedure TGLCustomShaderCombiner.DoApply(var rci: TGLRenderContextInfo;
  Sender: TObject);
begin
  if (csDesigning in ComponentState) then Exit;
  Assert((FShaderOne <> nil) and (FShaderTwo <> nil));

  FCurrentPass:=1;
  case FCombinerType of
    sctOneMPTwoSP:
      begin
        FShaderOne.Apply(rci, Self);
        FShaderTwo.Apply(rci, Self);
      end;

    sctTwoMPOneSP:
      begin
        FShaderTwo.Apply(rci, Self);
        FShaderOne.Apply(rci, Self);
      end;

    sctOneSPTwoAP:
      begin
        FShaderOne.Apply(rci, Self);
      end;

    sctTwoSPOneAP:
      begin
        FShaderTwo.Apply(rci, Self);
      end;
  else
    Assert(False, strErrorEx + strUnknownType);
  end;
end;

function TGLCustomShaderCombiner.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  case FCombinerType of
    sctOneMPTwoSP:
      begin
        if FShaderOne.UnApply(rci) then
          Result := True
        else
          Result := FShaderTwo.UnApply(rci);
      end;

    sctTwoMPOneSP:
      begin
        if FShaderTwo.UnApply(rci) then
          Result := True
        else
          Result := FShaderOne.UnApply(rci);
      end;

    sctOneSPTwoAP:
      begin
        if FCurrentPass = 1 then
        begin
          FShaderOne.UnApply(rci);
          FShaderTwo.Apply(rci, Self);
          Result := True;
        end
        else
          Result := FShaderTwo.UnApply(rci);
      end;

    sctTwoSPOneAP:
      begin
        if FCurrentPass = 1 then
        begin
          FShaderTwo.UnApply(rci);
          FShaderOne.Apply(rci, Self);
          Result := True;
        end
        else
          Result := FShaderOne.UnApply(rci);
      end;
  else
    begin
      Result := False;
      Assert(False, strErrorEx + strUnknownType);
    end;
  end;
  Inc(FCurrentPass);
end;

procedure TGLCustomShaderCombiner.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FShaderOne then
      FShaderOne := nil
    else if AComponent = FShaderTwo then
      FShaderTwo := nil;
  end;
end;

procedure TGLCustomShaderCombiner.SetShaderOne(const Value: TGLShader);
begin
  if FShaderOne <> nil then
    FShaderOne.RemoveFreeNotification(Self);
  FShaderOne := Value;
  if FShaderOne <> nil then
    FShaderOne.FreeNotification(Self);
end;

procedure TGLCustomShaderCombiner.SetShaderTwo(const Value: TGLShader);
begin
  if FShaderTwo <> nil then
    FShaderTwo.RemoveFreeNotification(Self);
  FShaderTwo := Value;
  if FShaderTwo <> nil then
    FShaderTwo.FreeNotification(Self);
end;

function TGLCustomShaderCombiner.ShaderSupported: Boolean;
begin
  Result := (FShaderOne <> nil) and (FShaderTwo <> nil) and
             FShaderOne.ShaderSupported and FShaderTwo.ShaderSupported;
end;

//-----------------------------------
initialization
//-----------------------------------

  RegisterClasses([TGLCustomShaderCombiner, TGLShaderCombiner]);

end.

