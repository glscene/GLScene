object FormClothify: TFormClothify
  Left = 34
  Top = 114
  Caption = 'Clothify'
  ClientHeight = 498
  ClientWidth = 715
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 60
    Width = 715
    Height = 438
    Camera = GLCamera1
    Buffer.BackgroundColor = clGray
    Buffer.AmbientColor.Color = {CDCC4C3DCDCC4C3DCDCC4C3D0000803F}
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow, roTwoSideLighting]
    Buffer.FaceCulling = False
    FieldOfView = 142.190841674804700000
    PenAsTouch = False
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GroupBox_LoadForm: TGroupBox
    Left = 48
    Top = 88
    Width = 649
    Height = 65
    Caption = 'Load Settings'
    TabOrder = 1
    Visible = False
    object Label2: TLabel
      Left = 8
      Top = 16
      Width = 26
      Height = 13
      Caption = 'Mesh'
    end
    object Label4: TLabel
      Left = 152
      Top = 16
      Width = 27
      Height = 13
      Caption = 'Mode'
    end
    object Label5: TLabel
      Left = 248
      Top = 16
      Width = 34
      Height = 13
      Caption = 'Collider'
    end
    object ComboBox_MeshName: TComboBox
      Left = 8
      Top = 32
      Width = 137
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'Trinityrage.smd, 0.15'
        'HalfSphere.ms3d, 2'
        'Cylinder.ms3d, 0.3'
        'lgrid.3ds, 3'
        'BigHoleBox2.ms3d, 0.5'
        'mushroom.3ds, 0.08'
        'polyhedron.3ds, 2'
        'teapot.3ds, 0.1')
    end
    object ComboBox_ConstraintType: TComboBox
      Left = 152
      Top = 32
      Width = 89
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      Items.Strings = (
        'Constraints'
        'Forces')
    end
    object ComboBox_Collider: TComboBox
      Left = 248
      Top = 32
      Width = 73
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'Sphere'
        'Infinite Cylinder'
        'Cube'
        'Stairs'
        'Capsule'
        'ODE Sphere'
        '(none)')
    end
    object Button_LoadMesh: TButton
      Left = 549
      Top = 32
      Width = 36
      Height = 21
      Caption = 'OK'
      Default = True
      TabOrder = 3
      OnClick = Button_LoadMeshClick
    end
    object CheckBox_UseOctree: TCheckBox
      Left = 384
      Top = 34
      Width = 81
      Height = 17
      Caption = 'Use Octree'
      TabOrder = 4
    end
    object CheckBox_SolidEdges: TCheckBox
      Left = 464
      Top = 34
      Width = 81
      Height = 17
      Caption = 'Solid Edges'
      TabOrder = 5
    end
    object CheckBox_Weld: TCheckBox
      Left = 328
      Top = 34
      Width = 49
      Height = 17
      Caption = 'Weld'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object Button_CancelLoad: TButton
      Left = 589
      Top = 32
      Width = 44
      Height = 21
      Caption = 'Cancel'
      TabOrder = 7
      OnClick = Button_CancelLoadClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 715
    Height = 60
    Align = alTop
    TabOrder = 2
    DesignSize = (
      715
      60)
    object Label3: TLabel
      Left = 8
      Top = 0
      Width = 27
      Height = 13
      Caption = 'Slack'
    end
    object Label6: TLabel
      Left = 104
      Top = 0
      Width = 43
      Height = 13
      Caption = 'Iterations'
    end
    object Label7: TLabel
      Left = 192
      Top = 0
      Width = 34
      Height = 13
      Caption = 'Friction'
    end
    object Label8: TLabel
      Left = 416
      Top = 0
      Width = 44
      Height = 13
      Caption = 'Shadows'
    end
    object Label1: TLabel
      Left = 583
      Top = 19
      Width = 38
      Height = 13
      Anchors = [akTop, akRight]
      Caption = '0,0 FPS'
    end
    object TrackBar_Slack: TTrackBar
      Left = 0
      Top = 14
      Width = 97
      Height = 25
      Max = 100
      TabOrder = 0
      TickStyle = tsNone
      OnChange = TrackBar_SlackChange
    end
    object TrackBar_Iterations: TTrackBar
      Left = 96
      Top = 14
      Width = 81
      Height = 25
      Max = 30
      Min = 1
      Position = 4
      TabOrder = 1
      TickStyle = tsNone
      OnChange = TrackBar_IterationsChange
    end
    object TrackBar_Friction: TTrackBar
      Left = 184
      Top = 14
      Width = 81
      Height = 25
      Max = 100
      Position = 60
      TabOrder = 2
      TickStyle = tsNone
      OnChange = TrackBar_FrictionChange
    end
    object CheckBox_ShowOctree: TCheckBox
      Left = 272
      Top = 15
      Width = 81
      Height = 17
      Caption = 'Show Octree'
      TabOrder = 3
    end
    object Button_OpenLoadForm: TButton
      Left = 360
      Top = 11
      Width = 51
      Height = 21
      Caption = '&Load'
      TabOrder = 4
      OnClick = Button_OpenLoadFormClick
    end
    object ComboBox_Shadow: TComboBox
      Left = 416
      Top = 16
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      OnChange = ComboBox_ShadowChange
      Items.Strings = (
        '(no shadows)'
        'Shadow Volumes'
        'Simple Shadow Plane')
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 168
    object GLShadowVolume1: TGLShadowVolume
      Lights = <
        item
          LightSource = GLLightSource1
        end>
      Occluders = <
        item
          Caster = GLCube_Stair1
        end
        item
          Caster = GLCube_Stair2
        end
        item
          Caster = GLCube_Stair3
        end
        item
          Caster = GLCube_Stair4
        end
        item
          Caster = GLCube1
        end
        item
          Caster = GLCylinder1
        end
        item
          Caster = GLShadowPlane1
        end
        item
          Caster = GLSphere1
        end
        item
          Caster = GLSphere2
        end
        item
          Caster = GLSphere3
        end
        item
          Caster = GL_Capsule
        end>
      Options = [svoCacheSilhouettes, svoScissorClips, svoWorldScissorClip, svoDesignVisible]
      Mode = svmDarkening
      object GLDummyCube1: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLDummyCube_Light: TGLDummyCube
          CubeSize = 1.000000000000000000
          object GLLightSource1: TGLLightSource
            ConstAttenuation = 1.000000000000000000
            Position.Coordinates = {0000C040000090410000C0400000803F}
            SpotCutOff = 180.000000000000000000
          end
        end
        object GL_Capsule: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {1283003F1283003F0000803F0000803F}
          Direction.Coordinates = {F304353FF304353F0000000000000000}
          Up.Coordinates = {F30435BFF304353F0000000000000000}
          BottomRadius = 1.500000000000000000
          Height = 2.000000000000000000
          TopRadius = 1.500000000000000000
          object GLSphere2: TGLSphere
            Material.FrontProperties.Diffuse.Color = {1283003F1283003F0000803F0000803F}
            Position.Coordinates = {000000000000803F000000000000803F}
            Radius = 1.500000000000000000
          end
          object GLSphere3: TGLSphere
            Material.FrontProperties.Diffuse.Color = {1283003F1283003F0000803F0000803F}
            Position.Coordinates = {00000000000080BF000000000000803F}
            Radius = 1.500000000000000000
          end
        end
        object GLDummyCube2: TGLDummyCube
          Position.Coordinates = {0000000000007041000000000000803F}
          CubeSize = 1.000000000000000000
          object GLActor1: TGLActor
            Material.BackProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
            Material.FrontProperties.Diffuse.Color = {1283003F0000803F000000000000803F}
            Material.FaceCulling = fcNoCull
            Interval = 100
            MaterialLibrary = GLMaterialLibrary1
          end
          object GLActor2: TGLActor
            Interval = 100
          end
        end
        object GLSphere1: TGLSphere
          Material.FrontProperties.Diffuse.Color = {1283003F1283003F0000803F0000803F}
          Position.Coordinates = {00000000000000C0000000000000803F}
          Visible = False
          Radius = 2.000000000000000000
        end
        object GLCylinder1: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {1283003F1283003F0000803F0000803F}
          Direction.Coordinates = {0000803F000000000000008000000000}
          Position.Coordinates = {000000000000C0BF000000000000803F}
          Up.Coordinates = {00000000000000000000803F00000000}
          Visible = False
          BottomRadius = 1.500000000000000000
          Height = 50.000000000000000000
          Slices = 24
          Stacks = 16
          TopRadius = 1.500000000000000000
        end
        object GLShadowPlane1: TGLShadowPlane
          Material.FrontProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
          Direction.Coordinates = {000000000000803F0000000000000000}
          Position.Coordinates = {00000000EC51A0C0000000000000803F}
          Up.Coordinates = {0000000000000000000080BF00000000}
          Visible = False
          Height = 60.000000000000000000
          Width = 60.000000000000000000
          XTiles = 30
          YTiles = 30
          Style = [psTileTexture]
          ShadowingObject = GLDummyCube1
          ShadowedLight = GLLightSource1
          ShadowOptions = [spoUseStencil]
        end
        object GLCube1: TGLCube
          Material.FrontProperties.Diffuse.Color = {8180003F8180003F0000803F0000803F}
          Position.Coordinates = {00000000000000C0000000000000803F}
          Visible = False
          CubeSize = {000040400000404000004040}
        end
        object GLDummyCube_Stairs: TGLDummyCube
          Visible = False
          CubeSize = 1.000000000000000000
          object GLCube_Stair1: TGLCube
            Material.FrontProperties.Diffuse.Color = {1283003F1283003F0000803F0000803F}
            Position.Coordinates = {0000000000000040000000000000803F}
            CubeSize = {000000400000004000006041}
          end
          object GLCube_Stair2: TGLCube
            Material.FrontProperties.Diffuse.Color = {1283003F1283003F0000803F0000803F}
            CubeSize = {0000C0400000004000006041}
          end
          object GLCube_Stair3: TGLCube
            Material.FrontProperties.Diffuse.Color = {1283003F1283003F0000803F0000803F}
            Position.Coordinates = {00000000000000C0000000000000803F}
            CubeSize = {000020410000004000006041}
          end
          object GLCube_Stair4: TGLCube
            Material.FrontProperties.Diffuse.Color = {1283003F1283003F0000803F0000803F}
            Position.Coordinates = {00000000000080C0000000000000803F}
            CubeSize = {000060410000004000006041}
          end
        end
        object GLPlane1: TGLPlane
          Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
          Material.FrontProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
          Direction.Coordinates = {000000000000803F0000000000000000}
          Position.Coordinates = {000000000000A0C0000000000000803F}
          Up.Coordinates = {0000000000000000000080BF00000000}
          Height = 60.000000000000000000
          Width = 60.000000000000000000
          XTiles = 30
          YTiles = 30
          Style = [psTileTexture]
        end
      end
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1.000000033181354E32
      FocalLength = 75.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000704100002041000070410000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 40
    Top = 224
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 144
    Top = 168
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 144
    Top = 224
  end
end
