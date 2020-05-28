object Form1: TForm1
  Left = 18
  Top = 53
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'GLScene DWS2 Scripting Basics'
  ClientHeight = 333
  ClientWidth = 703
  Color = clBtnFace
  Constraints.MinHeight = 360
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 330
    Height = 333
    Align = alLeft
    Constraints.MinWidth = 330
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 40
      Width = 145
      Height = 13
      Caption = 'GLSphere1.Behaviour[0] script'
    end
    object Label2: TLabel
      Left = 8
      Top = 184
      Width = 136
      Height = 13
      Caption = 'GLCube1.Behaviour[0] script'
    end
    object Button3: TButton
      Left = 8
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Recompile Scripts'
      TabOrder = 0
      OnClick = Button3Click
    end
    object GLSphere1Script: TMemo
      Left = 8
      Top = 56
      Width = 313
      Height = 121
      Lines.Strings = (
        'var ScriptObject : TGLBaseSceneObject;'
        ''
        'procedure OnBeginProgram(Sender: TObject);'
        'begin'
        '  ScriptObject:=nil;'
        '  if Sender is TGLBaseSceneObject then'
        '    ScriptObject:=TGLBaseSceneObject(Sender);'
        'end;'
        ''
        'procedure OnProgress(newTime, deltaTime : Float);'
        'begin'
        '  if Assigned(ScriptObject) then begin'
        '    ScriptObject.Pitch(10*deltaTime);'
        '    ScriptObject.Turn(-20*deltaTime);'
        '    ScriptObject.Roll(15*deltaTime);'
        '  end;'
        'end;')
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
    end
    object CheckBox1: TCheckBox
      Left = 132
      Top = 8
      Width = 73
      Height = 17
      Caption = 'Cadencer'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object GLCube1Script: TMemo
      Left = 8
      Top = 200
      Width = 313
      Height = 121
      Lines.Strings = (
        'var ScriptObject : TGLBaseSceneObject;'
        ''
        'procedure OnBeginProgram(Sender: TObject);'
        'begin'
        '  ScriptObject:=nil;'
        '  if Sender is TGLBaseSceneObject then'
        '    ScriptObject:=TGLBaseSceneObject(Sender);'
        'end;'
        ''
        'procedure OnProgress(newTime, deltaTime : Float);'
        'begin'
        '  if Assigned(ScriptObject) then begin'
        '    ScriptObject.Scale.X:=1.5+Cos(newTime);'
        '  end;'
        'end;')
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 3
      WordWrap = False
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 330
    Top = 0
    Width = 373
    Height = 333
    Camera = GLCamera1
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 336
    Top = 8
    object GLSphere1: TGLSphere
      Radius = 0.5
      BehavioursData = {
        0201061654474C445753324163746976654265686176696F7572020002000906
        14474C44656C7068695765625363726970744949310600}
      object GLCube1: TGLCube
        Position.Coordinates = {0000003F0000803F000000000000803F}
        BehavioursData = {
          0201061654474C445753324163746976654265686176696F7572020002000906
          14474C44656C7068695765625363726970744949310600}
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      Position.Coordinates = {00000000000000000000A0400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1
        SpotCutOff = 180
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 368
    Top = 8
  end
  object GLDelphiWebScriptII1: TGLDelphiWebScriptII
    Config.CompilerOptions = []
    Config.MaxDataSize = 0
    Config.Timeout = 0
    Left = 16
    Top = 72
  end
  object dws2VectorGeometryUnit1: Tdws2VectorGeometryUnit
    Script = GLDelphiWebScriptII1
    Left = 80
    Top = 72
  end
  object dws2ClassesUnit1: Tdws2ClassesUnit
    Script = GLDelphiWebScriptII1
    Left = 48
    Top = 72
  end
  object dws2GLSceneUnit1: Tdws2GLSceneUnit
    Script = GLDelphiWebScriptII1
    Left = 112
    Top = 72
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    Left = 400
    Top = 8
  end
end
