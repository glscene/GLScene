object Form1: TForm1
  Left = 192
  Top = 107
  Width = 790
  Height = 399
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 416
    Height = 372
    Camera = GLCamera1
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 416
    Top = 0
    Width = 366
    Height = 372
    Align = alRight
    Constraints.MinHeight = 372
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 40
      Width = 40
      Height = 13
      Caption = 'DoApply'
    end
    object ShaderScript: TMemo
      Left = 8
      Top = 56
      Width = 349
      Height = 305
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object Recompile: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Recompile'
      TabOrder = 1
      OnClick = RecompileClick
    end
    object Enabled: TCheckBox
      Left = 96
      Top = 8
      Width = 65
      Height = 17
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = EnabledClick
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000803F0000803F000000400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object GLCube1: TGLCube
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'Shader'
      Material.TextureEx = <>
    end
  end
  object dws2OpenGL1xUnit1: Tdws2OpenGL1xUnit
    Script = GLDelphiWebScriptII1
    Left = 182
    Top = 8
  end
  object GLUserShader1: TGLUserShader
    OnDoApply = GLUserShader1DoApply
    OnDoUnApply = GLUserShader1DoUnApply
    ShaderStyle = ssLowLevel
    Left = 72
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Shader'
        Material.TextureEx = <>
        Tag = 0
        Shader = GLUserShader1
      end>
    Left = 40
    Top = 8
  end
  object dws2VectorGeometryUnit1: Tdws2VectorGeometryUnit
    Script = GLDelphiWebScriptII1
    Left = 152
    Top = 8
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpHigher
    Left = 40
    Top = 40
  end
  object GLDelphiWebScriptII1: TGLDelphiWebScriptII
    Config.CompilerOptions = []
    Config.MaxDataSize = 0
    Config.Timeout = 0
    Left = 120
    Top = 8
  end
  object GLScriptLibrary1: TGLScriptLibrary
    Left = 120
    Top = 40
    ScriptsData = {
      0201060D54474C536372697074445753320200060C5368616465725363726970
      7402000C6C0200002F2F2052656E6465727320776974682072656420666C6174
      2073686164656420706F6C79676F6E7320616E640D0A2F2F206120626C756520
      776972656672616D65206F75746C696E650D0A0D0A70726F6365647572652044
      6F4170706C793B0D0A626567696E0D0A2020676C507573684174747269622847
      4C5F414C4C5F4154545249425F42495453293B0D0A2020676C456E61626C6528
      474C5F434F4C4F525F4D4154455249414C293B0D0A2020676C53686164654D6F
      64656C28474C5F464C4154293B0D0A2020676C436F6C6F72336628312C302C30
      293B0D0A656E643B0D0A0D0A66756E6374696F6E20446F556E4170706C792850
      617373203A20496E746567657229203A20426F6F6C65616E3B0D0A626567696E
      0D0A2020526573756C743A3D46616C73653B0D0A202063617365205061737320
      6F660D0A0D0A2020202031203A20626567696E0D0A202020202020676C446973
      61626C6528474C5F4C49474854494E47293B0D0A202020202020676C44697361
      626C6528474C5F434F4C4F525F4D4154455249414C293B0D0A20202020202067
      6C44697361626C6528474C5F424C454E44293B0D0A202020202020676C446570
      746846756E6328474C5F4C455155414C293B0D0A202020202020676C506F6C79
      676F6E4D6F646528474C5F46524F4E542C20474C5F4C494E45293B0D0A202020
      202020676C4C696E6557696474682833293B0D0A202020202020676C436F6C6F
      72336628302C302C31293B0D0A202020202020526573756C743A3D547275653B
      0D0A20202020656E643B0D0A0D0A2020202032203A20676C506F704174747269
      623B0D0A0D0A2020656E643B0D0A656E643B0D0A062044575332207363726970
      7420696E2061206D6174657269616C2073686164657202000614474C44656C70
      6869576562536372697074494931}
  end
end
