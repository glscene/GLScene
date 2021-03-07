object Form1: TForm1
  Left = 268
  Top = 248
  Caption = 'GLScene Pak Editor'
  ClientHeight = 486
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    539
    486)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 545
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object TreeView: TTreeView
    Left = 8
    Top = 8
    Width = 161
    Height = 297
    Anchors = [akLeft, akTop, akBottom]
    Images = ImageList1
    Indent = 19
    ShowRoot = False
    TabOrder = 0
    ToolTips = False
    OnChange = TreeViewChange
    OnCollapsing = TreeViewCollapsing
    OnKeyDown = TreeViewKeyDown
  end
  object ListView: TListView
    Left = 175
    Top = 8
    Width = 365
    Height = 466
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        Caption = 'Size'
        Width = 70
      end>
    Enabled = False
    Items.ItemData = {
      054E0000000100000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
      001A4F00700065006E0020006F00720020006300720065006100740065002000
      700061006B002000660069006C0065002E002E002E00}
    MultiSelect = True
    SmallImages = ImageList1
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = ListViewChange
    OnClick = ListViewClick
    OnDblClick = ListViewDblClick
    OnKeyDown = ListViewKeyDown
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 311
    Width = 161
    Height = 161
    Camera = GLCamera1
    VSync = vsmSync
    Buffer.BackgroundColor = clWhite
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aa2x
    FieldOfView = 77.668128967285160000
    PenAsTouch = False
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object MainMenu1: TMainMenu
    Left = 184
    Top = 32
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New...'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = 'Open...'
        OnClick = Open1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Createfolder1: TMenuItem
        Caption = 'Create folder...'
        OnClick = Createfolder1Click
      end
      object Addfiles1: TMenuItem
        Caption = 'Add file(s)...'
        OnClick = Addfiles1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Compression1: TMenuItem
        Caption = 'Compression'
        object None1: TMenuItem
          Caption = 'None'
          Checked = True
          GroupIndex = 2
          OnClick = None1Click
        end
        object Fast1: TMenuItem
          Tag = 1
          Caption = 'Fast'
          GroupIndex = 2
          OnClick = None1Click
        end
        object Default1: TMenuItem
          Tag = 2
          Caption = 'Default'
          GroupIndex = 2
          OnClick = None1Click
        end
        object Max1: TMenuItem
          Tag = 3
          Caption = 'Max'
          GroupIndex = 2
          OnClick = None1Click
        end
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object Deleteselectedfile1: TMenuItem
        Caption = 'Delete selected file(s)'
        GroupIndex = 2
        OnClick = Deleteselectedfile1Click
      end
      object Deleteselectedfolder1: TMenuItem
        Caption = 'Delete selected folder'
        GroupIndex = 2
        OnClick = Deleteselectedfolder1Click
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object Extractselectedfiles1: TMenuItem
        Caption = 'Extract selected file(s)'
        GroupIndex = 2
        OnClick = Extractselectedfiles1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 184
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'glp'
    Filter = 'All (Pak, Zlib)|*.pak;*.zlib|PAK files|*.pak|ZLib Files|*.zlib'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 216
    Top = 64
  end
  object ImageList1: TImageList
    Left = 216
    Top = 32
  end
  object GLScene1: TGLScene
    Left = 248
    Top = 32
    object GLCamera1: TGLCamera
      DepthOfView = 100000.000000000000000000
      FocalLength = 100.000000000000000000
      NearPlaneBias = 0.100000001490116100
      TargetObject = GLCube1
      Position.Coordinates = {0000F04100002042000048420000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLFreeForm1: TGLFreeForm
    end
    object GLSprite1: TGLSprite
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'image'
      Position.Coordinates = {0000A0420000A042000000000000803F}
      Visible = False
      Width = 160.000000000000000000
      Height = 160.000000000000000000
      Rotation = 0.000000000000000000
    end
    object GLCube1: TGLCube
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'image'
      Visible = False
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'image'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLCompositeImage'
        Material.Texture.Image.Width = 256
        Material.Texture.Image.Height = 256
        Material.Texture.Image.Depth = 0
        Material.Texture.Disabled = False
      end>
    Left = 248
    Top = 64
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'GLScene Pak Editor - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 280
    Top = 32
  end
  object GLSArchiveManager1: TGLSArchiveManager
    Archives = <
      item
        Name = 'LibArchive'
      end>
    Left = 280
    Top = 64
  end
end
