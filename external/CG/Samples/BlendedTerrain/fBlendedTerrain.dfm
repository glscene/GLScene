object Form1: TForm1
  Left = 192
  Top = 114
  Align = alClient
  BorderStyle = bsNone
  Caption = 'Blended Terrain'
  ClientHeight = 428
  ClientWidth = 627
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 627
    Height = 428
    Camera = Camera
    FieldOfView = 153.698104858398400000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object Terrain: TGLTerrainRenderer
      Material.MaterialLibrary = MLTerrain
      Material.LibMaterialName = 'MultiPass'
      Position.Coordinates = {000020C5000020C5000000000000803F}
      Scale.Coordinates = {0000A0400000A0400000A03F00000000}
      HeightDataSource = GLCustomHDS1
      TileSize = 32
      TilesPerTexture = 32.000000000000000000
      QualityDistance = 150.000000000000000000
      CLODPrecision = 25
      OnGetTerrainBounds = TerrainGetTerrainBounds
      ContourWidth = 0
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000204100002041000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
      ModulateColor.Color = {0000000000000000000000000000803F}
    end
    object Camera: TGLCamera
      DepthOfView = 5000.000000000000000000
      FocalLength = 50.000000000000000000
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object GLCustomHDS1: TGLCustomHDS
    MaxPoolSize = 0
    OnStartPreparingData = GLCustomHDS1StartPreparingData
    Left = 200
    Top = 64
  end
  object CgBlendedTexture: TCgShader
    VertexProgram.Enabled = False
    FragmentProgram.Code.Strings = (
      'struct fragmentIN '
      '{'
      '  float4 HPosition : POSITION;'
      '  float4 TexCoord : TEXCOORD0;'
      '};'
      ''
      'struct fragmentOUT '
      '{'
      '  float4 Color : COLOR;'
      '};'
      ''
      'fragmentOUT main('
      '  fragmentIN IN,'
      '  uniform sampler2D blendmap,'
      '  uniform sampler2D channel1,'
      '  uniform sampler2D channel2,'
      '  uniform sampler2D channel3) '
      '{'
      '  fragmentOUT OUT;'
      '  float3 blend, col1,col2,col3;'
      '  float2 tex = IN.TexCoord.xy;'
      ''
      '  blend = f3tex2D(blendmap, IN.TexCoord.xy);'
      '  col1 = f3tex2D(channel1, tex*512);'
      '  col2 = (  f3tex2D(channel2, tex*256)'
      '              +f3tex2D(channel2, tex*2048)) * 0.5;'
      '  col3 = (  f3tex2D(channel3, tex*32)'
      '              +f3tex2D(channel3, tex*64)) * 0.5;'
      ''
      '  OUT.Color.rgb = blend .x*col1+blend .y*col2+blend .z*col3;'
      '  OUT.Color.a = 1.0;'
      '  return OUT;'
      '}')
    FragmentProgram.OnApply = CgBlendedTextureApplyFP
    FragmentProgram.OnUnApply = CgBlendedTextureUnApplyFP
    OnApplyFP = CgBlendedTextureApplyFP
    OnUnApplyFP = CgBlendedTextureUnApplyFP
    Left = 120
    Top = 120
  end
  object MLTerrain: TGLMaterialLibrary
    Materials = <
      item
        Name = 'MultiPass'
        Tag = 0
        Shader = GLUserShader1
      end
      item
        Name = 'CgBlendedTexture'
        Tag = 0
        Shader = CgBlendedTexture
      end>
    Left = 120
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 64
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 200
    Top = 120
  end
  object GLNavigator1: TGLNavigator
    VirtualUp.Coordinates = {00000000000000000000803F0000803F}
    MovingObject = Camera
    UseVirtualUp = True
    AutoUpdateObject = True
    Left = 120
    Top = 64
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial Black'
    Font.Style = []
    Left = 120
    Top = 184
  end
  object GLUserInterface1: TGLUserInterface
    MouseSpeed = 20.000000000000000000
    GLNavigator = GLNavigator1
    GLVertNavigator = GLNavigator1
    Left = 200
    Top = 8
  end
  object GLUserShader1: TGLUserShader
    OnDoApply = GLUserShader1DoApply
    OnDoUnApply = GLUserShader1DoUnApply
    Left = 24
    Top = 120
  end
end
