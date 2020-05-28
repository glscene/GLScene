object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Camera Controller'
  ClientHeight = 578
  ClientWidth = 750
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 304
    Top = 0
    Width = 446
    Height = 543
    Align = alRight
    TabOrder = 0
    object Panel2: TPanel
      Left = 6
      Top = 9
      Width = 155
      Height = 201
      TabOrder = 0
      object Label1: TLabel
        Left = 6
        Top = 57
        Width = 63
        Height = 13
        Caption = 'Destination X'
      end
      object Label2: TLabel
        Left = 6
        Top = 105
        Width = 63
        Height = 13
        Caption = 'Destination Y'
      end
      object Label3: TLabel
        Left = 6
        Top = 153
        Width = 63
        Height = 13
        Caption = 'Destination Z'
      end
      object Label5: TLabel
        Left = 4
        Top = 2
        Width = 141
        Height = 47
        AutoSize = False
        Caption = 'Data for MoveToPos, OrbitToPos and SafeOrbitAndZoomToPos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object eDestX: TEdit
        Left = 6
        Top = 72
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '0'
      end
      object eDestY: TEdit
        Left = 6
        Top = 122
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '5.79'
      end
      object eDestZ: TEdit
        Left = 6
        Top = 168
        Width = 121
        Height = 21
        TabOrder = 2
        Text = '-3.34'
      end
    end
    object Panel3: TPanel
      Left = 166
      Top = 241
      Width = 155
      Height = 72
      TabOrder = 1
      object Label6: TLabel
        Left = 6
        Top = 25
        Width = 41
        Height = 13
        Caption = 'Distance'
      end
      object Label9: TLabel
        Left = 4
        Top = 2
        Width = 129
        Height = 15
        AutoSize = False
        Caption = 'Data for ZoomToDistance'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object eDistance: TEdit
        Left = 6
        Top = 40
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '4'
      end
    end
    object Panel4: TPanel
      Left = 166
      Top = 9
      Width = 155
      Height = 224
      TabOrder = 2
      object Label7: TLabel
        Left = 6
        Top = 41
        Width = 74
        Height = 13
        Caption = 'soSafeDistance'
      end
      object Label8: TLabel
        Left = 6
        Top = 137
        Width = 69
        Height = 13
        Caption = 'soTimeToOrbit'
      end
      object Label10: TLabel
        Left = 6
        Top = 89
        Width = 116
        Height = 13
        Caption = 'soTimeToSafePlacement'
      end
      object Label11: TLabel
        Left = 4
        Top = 2
        Width = 141
        Height = 31
        AutoSize = False
        Caption = 'Data for SafeOrbitAndZoomToPos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object Label12: TLabel
        Left = 6
        Top = 177
        Width = 103
        Height = 13
        Caption = 'soTimeToZoomBackIn'
      end
      object eSafeDistance: TEdit
        Left = 6
        Top = 56
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '9'
      end
      object eTimeToSafePlacement: TEdit
        Left = 6
        Top = 106
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '1'
      end
      object eTimeToOrbit: TEdit
        Left = 6
        Top = 152
        Width = 121
        Height = 21
        TabOrder = 2
        Text = '2'
      end
      object eTimeToZoomBackIn: TEdit
        Left = 6
        Top = 192
        Width = 121
        Height = 21
        TabOrder = 3
        Text = '1'
      end
    end
    object Panel5: TPanel
      Left = 6
      Top = 217
      Width = 155
      Height = 96
      TabOrder = 3
      object Label13: TLabel
        Left = 6
        Top = 49
        Width = 22
        Height = 13
        Caption = 'Time'
      end
      object Label14: TLabel
        Left = 4
        Top = 2
        Width = 129
        Height = 47
        AutoSize = False
        Caption = 'Data for MoveToPos, OrbitToPos and ZoomToDistance'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object eTime: TEdit
        Left = 6
        Top = 64
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '2'
      end
    end
    object btnMoveToPos: TButton
      Left = 25
      Top = 336
      Width = 224
      Height = 33
      Caption = 'MoveToPos (linear interpolation)'
      TabOrder = 4
      OnClick = btnMoveToPosClick
    end
    object btnZoomToDistance: TButton
      Left = 25
      Top = 375
      Width = 224
      Height = 34
      Caption = 'ZoomToDistance'
      TabOrder = 5
      OnClick = btnZoomToDistanceClick
    end
    object btnOrbitToPos: TButton
      Left = 25
      Top = 415
      Width = 224
      Height = 34
      Caption = 'OrbitToPos (will not zoom in to pos)'
      TabOrder = 6
      OnClick = btnOrbitToPosClick
    end
    object btnSafeOrbitAndZoomToPos: TButton
      Left = 25
      Top = 455
      Width = 224
      Height = 34
      Caption = 'SafeOrbitAndZoomToPos'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnClick = btnSafeOrbitAndZoomToPosClick
    end
    object btnOrbitToPosAdv: TButton
      Left = 255
      Top = 455
      Width = 186
      Height = 34
      Caption = 'OrbitToPosAdv (will not zoom in to pos)'
      TabOrder = 8
      WordWrap = True
      OnClick = btnOrbitToPosAdvClick
    end
    object Panel8: TPanel
      Left = 327
      Top = 9
      Width = 114
      Height = 320
      TabOrder = 9
      object Label20: TLabel
        Left = 4
        Top = 2
        Width = 44
        Height = 13
        Caption = 'Camera'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object Panel7: TPanel
        Left = 8
        Top = 19
        Width = 97
        Height = 118
        BevelOuter = bvLowered
        Caption = ' '
        TabOrder = 0
        object Label16: TLabel
          Left = 8
          Top = 34
          Width = 6
          Height = 13
          Caption = 'X'
        end
        object Label17: TLabel
          Left = 8
          Top = 61
          Width = 6
          Height = 13
          Caption = 'Y'
        end
        object Label18: TLabel
          Left = 8
          Top = 88
          Width = 6
          Height = 13
          Caption = 'Z'
        end
        object Label19: TLabel
          Left = 4
          Top = 2
          Width = 51
          Height = 13
          Caption = 'Direction'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object camDirY: TEdit
          Left = 20
          Top = 58
          Width = 68
          Height = 21
          TabOrder = 0
          Text = '0.0'
        end
        object camDirZ: TEdit
          Left = 20
          Top = 85
          Width = 68
          Height = 21
          TabOrder = 1
          Text = '0.0'
        end
        object camDirX: TEdit
          Left = 20
          Top = 31
          Width = 69
          Height = 21
          TabOrder = 2
          Text = '0.0'
        end
      end
      object Panel9: TPanel
        Left = 8
        Top = 143
        Width = 97
        Height = 118
        BevelOuter = bvLowered
        Caption = ' '
        TabOrder = 1
        object Label21: TLabel
          Left = 8
          Top = 34
          Width = 6
          Height = 13
          Caption = 'X'
        end
        object Label22: TLabel
          Left = 8
          Top = 61
          Width = 6
          Height = 13
          Caption = 'Y'
        end
        object Label23: TLabel
          Left = 8
          Top = 88
          Width = 6
          Height = 13
          Caption = 'Z'
        end
        object Label24: TLabel
          Left = 4
          Top = 2
          Width = 15
          Height = 13
          Caption = 'Up'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object camUpX: TEdit
          Left = 20
          Top = 31
          Width = 68
          Height = 21
          TabOrder = 0
          Text = '0.0'
        end
        object camUpY: TEdit
          Left = 20
          Top = 58
          Width = 68
          Height = 21
          TabOrder = 1
          Text = '0.0'
        end
        object camUpZ: TEdit
          Left = 20
          Top = 85
          Width = 68
          Height = 21
          TabOrder = 2
          Text = '0.0'
        end
      end
      object UpAxis: TCheckBox
        Left = 9
        Top = 271
        Width = 97
        Height = 17
        Caption = 'Prefer up axis'
        TabOrder = 2
      end
      object cbMoveParent: TCheckBox
        Left = 9
        Top = 295
        Width = 97
        Height = 17
        Caption = 'Move Parent'
        TabOrder = 3
      end
    end
    object btSmoothOrbit: TButton
      Left = 255
      Top = 375
      Width = 184
      Height = 34
      Caption = 'Smooth Orbit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      OnClick = btSmoothOrbitClick
    end
    object btSmoothOrbitToPosAdv: TButton
      Left = 255
      Top = 415
      Width = 184
      Height = 34
      Caption = 'Smooth OrbitToPosAdv'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
      OnClick = btSmoothOrbitToPosAdvClick
    end
    object btSmoothOrbitAndZoom: TButton
      Left = 255
      Top = 335
      Width = 184
      Height = 34
      Caption = 'Smooth OrbitToPos and Zoom'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
      OnClick = btSmoothOrbitAndZoomClick
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 304
    Height = 543
    Camera = GLCamera
    VSync = vsmSync
    FieldOfView = 103.419670104980500000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 1
  end
  object pImg: TPanel
    Left = 4
    Top = 4
    Width = 157
    Height = 24
    BevelOuter = bvNone
    TabOrder = 2
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 24
      Height = 24
      Picture.Data = {
        07544269746D6170F6060000424DF60600000000000036000000280000001800
        0000180000000100180000000000C0060000C30E0000C30E0000000000000000
        000082C9F77FC8F786CBF78CCEF792D0F898D3F89FD6F8A5D8F9AADBF9B1DDFA
        B7E0FABCE3FBC2E5FBC7E7FBCDE9FCD2ECFCD7EEFCDCF0FDE1F3FDE6F5FDE9F7
        FEEDF8FEF2F9FEF5FBFE70C3F5C0C0C0C0C0C0C0C0C0C0C0C08BCDF792D0F898
        D3F89ED6F9A6D9F9ACDBFAB2DEFAB8E1FBBFE3FBC4E6FBCAE9FCCFEBFCD5EDFC
        808080808080808080E8F6FEEDF7FEF2F9FE6DC1F60000000000000000000000
        00C0C0C08CCEF891D0F899D3F89FD6F9A6D9F9ACDCFAB3DEFAB8E1FABFE3FBC4
        E6FBCAE9FC808080D5EDFCDAF0FDDFF2FD808080E8F6FEEEF8FE67BEF563BDF5
        000000000000000000000000C0C0C0C0C0C092D0F798D3F89FD6F9A6D9F9ACDC
        FAB2DEFAB9E1FABFE4FB808080CAE8FCCFEBFCD5EDFCDAF0FDDFF2FD808080EA
        F7FE60BBF55EBAF464BDF56BC0F571C2F60000000000000000008CCEF792D1F8
        98D3F89FD6F9A5D9F9ABDCFAB3DEFAB9E1FB808080C4E6FB8080808080808080
        80DAEFFD808080E6F5FD5BB9F557B7F45EBAF464BDF56AC0F571C3F678C6F600
        000085CBF78CCEF792D1F799D4F89FD6F9A6D9F9ACDCFAB3DEFA808080BFE3FB
        C4E6FBCAE9FCD0EAFCD5EDFC808080E2F3FD56B7F451B5F458B8F45EBAF564BD
        F56BC0F572C3F678C5F67EC8F785CBF78CCEF793D1F899D4F89FD6F9A6D9F9AC
        DCFAB2DEFA808080BFE4FBC4E6FBCAE9FC808080D6EEFCDDF1FD50B5F44AB2F3
        51B5F457B7F400000064BDF56BC0F571C3F60000000000000000000000000000
        00000000000000000000000000000000808080808080808080CAE9FCCFEBFCD8
        EFFC49B2F344B0F34BB2F352B5F400000000000064BDF5000000000000000000
        000000000000000000000000000000000000000000000000000000B9E1FBBFE4
        FBC5E6FBCAE9FCD3ECFC45AFF33FADF346AFF34CB2F300000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000B3DFFAB9E1FBBFE4FBC5E6FBCEE9FC3FADF339AAF240ADF346B0F30000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000ACDBFAB3DEFAB9E1FBBFE4FBC8E7FB3AABF234A8F2
        39ABF23FADF20000000000000000000000000000000000000000000000000000
        00000000000000000000000000000000000000A6D9F9ADDCFAB3DFFABAE1FBC4
        E6FB36A8F22EA6F134A8F23AAAF20000000000004CB2F3000000000000000000
        000000000000000000000000000000000000000000000000000000A0D6F9A7D9
        F9ADDCFAB4DFFABEE3FB30A7F229A3F12FA6F134A9F200000040ADF346B0F34C
        B2F3000000000000000000000000000000000000000000000000000000000000
        93D1F899D4F8A0D7F9A6D9F9ADDCFAB8E1FA2CA6F124A2F029A4F12FA6F134A8
        F23AABF240AEF245B0F34BB3F300000000000000000066BEF56CC0F500000000
        000000000086CCF78DCEF894D1F89AD4F8A1D7F9A7DAF9B3DEFA28A3F11F9FF0
        24A1F129A3F12FA5F134A9F239ABF240ADF20000000000000000000000000000
        0000000000000000000000000000000086CCF78DCEF894D1F89AD4F9A1D7F9AC
        DBF924A1F11A9DF01FA0F0FFFFFFFFFFFFFFFFFF34A8F23AABF2000000000000
        00000000000000000000000000000000000000000000000080C9F786CBF78DCE
        F893D1F89AD4F8A7D9F920A0F0169BF0FFFFFF1FA0F024A2F12AA3F1FFFFFF34
        A9F23AABF200000000000000000053B5F459B8F400000000000000000072C3F6
        7AC6F6C0C0C0C0C0C08DCFF893D1F8A1D7F81C9DF0FFFFFF169CF01A9EF0FFFF
        FF24A2F129A4F1FFFFFF35A9F23BABF240ADF247B0F34DB3F352B5F459B8F460
        BBF566BEF56CC0F673C3F6000000000000C0C0C08DCFF89AD4F8189CF0FFFFFF
        1299EFFFFFFFFFFFFFFFFFFF25A1F1FFFFFF2FA6F134A8F23AABF241AEF347B0
        F34CB2F352B5F459B8F45FBBF566BEF56DC1F673C3F6000000000000C0C0C094
        D1F8159BF0FFFFFF0E98EF129AEFFFFFFF1B9EF01FA0F0FFFFFF2AA4F12FA7F1
        35A9F23BABF240AEF247B0F34DB3F353B6F459B8F460BBF466BEF56CC0F573C3
        F60000000000008ECFF71299EF0695EFFFFFFF0E99EF1299EF169CF0FFFFFF1F
        A0F025A1F12AA4F12FA6F134A8F23BABF241AEF347B0F34DB3F353B6F459B8F4
        5FBBF566BDF56DC1F600000000000088CCF70C97EF0394EE0795EFFFFFFFFFFF
        FFFFFFFF169CF01B9DF0209FF025A1F12AA4F12FA7F135A9F23AABF240AEF347
        B0F34CB3F352B6F459B8F460BBF566BEF56DC1F500000080C8F61E9FF01098EF
        129AEF159BF0189DF01C9EF020A1F024A1F128A3F12DA6F132A7F237A9F23BAB
        F241AEF346B0F34CB3F452B5F457B7F45DBAF563BCF569BFF66FC2F676C5F683
        CBF7}
    end
    object Label4: TLabel
      Left = 32
      Top = 6
      Width = 102
      Height = 13
      Caption = 'TGLCameraController'
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 543
    Width = 750
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object Label15: TLabel
      Left = 4
      Top = 6
      Width = 853
      Height = 31
      Alignment = taCenter
      AutoSize = False
      Caption = 
        'The main method, SafeOrbitAndZoomToPos, resambles the Google Ear' +
        'th "Fly to..." motion. I developed and used it to fly to differe' +
        'nt views of a car (hood, trunk etc) in 3DCar(TM), which probably' +
        ' was/is the first interactive 3D car configurator, released by B' +
        'luemind Software.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 32
    object dcMovingParent: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera: TGLCamera
        DepthOfView = 500.000000000000000000
        FocalLength = 120.000000000000000000
        NearPlaneBias = 0.100000001490116100
        TargetObject = dcSphere
        Position.Coordinates = {0000204100002041000020410000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
      object dcSphere: TGLDummyCube
        ObjectsSorting = osRenderBlendedLast
        ShowAxes = True
        Up.Coordinates = {000000000000803F0000008000000000}
        CubeSize = 4.000000000000000000
        object GLSphere1: TGLSphere
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'transparent blue'
          Radius = 2.000000000000000000
          Slices = 32
          Stacks = 32
        end
        object GLCylinder1: TGLCylinder
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'opaque green'
          ObjectsSorting = osNone
          Direction.Coordinates = {00000000FFFFFF3ED7B35D3F00000000}
          PitchAngle = 30.000000000000000000
          Up.Coordinates = {00000000D7B35D3FFFFFFFBE00000000}
          BottomRadius = 0.200000002980232200
          Height = 4.199999809265137000
          TopRadius = 0.050000000745058060
        end
        object GLSphere2: TGLSphere
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'opaque green'
          Position.Coordinates = {6666E63F00000000000000000000803F}
          Radius = 0.300000011920929000
        end
        object GLSphere3: TGLSphere
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'opaque red'
          Position.Coordinates = {000000006666E63F000000000000803F}
          Radius = 0.300000011920929000
        end
      end
    end
    object dcDebugGUI: TGLDummyCube
      Scale.Coordinates = {0000003F0000003F0000003F00000000}
      CubeSize = 1.000000000000000000
      object ArrowLine: TGLArrowLine
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {0000000000004843000000000000803F}
        Scale.Coordinates = {0000C0400000C0400000004100000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        BottomRadius = 0.100000001490116100
        Height = 1.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object XYZGrid: TGLXYZGrid
        Position.Coordinates = {000000000000F0C1000000000000803F}
        LineColor.Color = {E9E8683E8786063FD2D1513F0000803F}
        XSamplingScale.Min = -150.000000000000000000
        XSamplingScale.Max = 150.000000000000000000
        XSamplingScale.Step = 5.000000000000000000
        YSamplingScale.Step = 0.100000001490116100
        ZSamplingScale.Min = -150.000000000000000000
        ZSamplingScale.Max = 150.000000000000000000
        ZSamplingScale.Step = 5.000000000000000000
        Parts = [gpX, gpZ]
      end
      object GLPlane1: TGLPlane
        Material.FrontProperties.Ambient.Color = {FBFAFA3EBBBA3A3FEEED6D3F0000803F}
        Material.FrontProperties.Diffuse.Color = {CFCECE3ECDCC4C3FE7E6E63E0000803F}
        Material.FrontProperties.Emission.Color = {0000000000000000BBBABA3E0000803F}
        Direction.Coordinates = {000000800000803F0000000000000000}
        Position.Coordinates = {00000000CDCCF4C1000000000000803F}
        Scale.Coordinates = {00009643000096430000803F00000000}
        Up.Coordinates = {0000803F000000000000000000000000}
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'transparent blue'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {FBFAFA3EF7F6F63E0000803F9A99193F}
        Material.BlendingMode = bmTransparency
      end
      item
        Name = 'opaque green'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {000000000000803FF3F2F23E0000803F}
      end
      item
        Name = 'opaque red'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      end>
    Left = 128
    Top = 32
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    FixedDeltaTime = 0.010000000000000000
    SleepLength = 0
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 80
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 232
    Top = 88
  end
  object GLCameraController1: TGLCameraController
    Camera = GLCamera
    CameraTarget = dcSphere
    soSafeDistance = 10.000000000000000000
    soTimeToSafePlacement = 1.000000000000000000
    soTimeToOrbit = 2.000000000000000000
    soTimeToZoomBackIn = 1.000000000000000000
    Left = 128
    Top = 80
  end
  object GLSmoothNavigator: TGLSmoothNavigator
    MoveAroundParams.Inertia = 0.500000000000000000
    MoveAroundParams.MaxAngle = 0.500000000000000000
    MoveAroundParams.PitchSpeed = 50.000000000000000000
    MoveAroundParams.TurnSpeed = 50.000000000000000000
    AdjustDistanceParams.Inertia = 1000.000000000000000000
    Left = 232
    Top = 32
  end
end
