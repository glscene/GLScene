object RMMSnapShot: TRMMSnapShot
  Left = 306
  Top = 96
  Width = 299
  Height = 361
  BorderStyle = bsSizeToolWin
  BorderWidth = 3
  Caption = 'RecyclerMM SnapShot Viewer'
  Color = clBtnFace
  Constraints.MaxWidth = 299
  Constraints.MinHeight = 300
  Constraints.MinWidth = 299
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 285
    Height = 326
    ActivePage = TSSummary
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object TSSummary: TTabSheet
      Caption = 'Summary'
      object LVStats: TListView
        Left = 0
        Top = 0
        Width = 277
        Height = 297
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Stat'
            Width = 200
          end
          item
            Alignment = taRightJustify
            Caption = 'Value'
            Width = 75
          end>
        ColumnClick = False
        FlatScrollBars = True
        ReadOnly = True
        RowSelect = True
        SmallImages = ImageList1
        TabOrder = 0
        ViewStyle = vsReport
        OnCustomDrawItem = LVStatsCustomDrawItem
      end
    end
    object TSMemoryMap: TTabSheet
      Caption = 'Memory Map'
      ImageIndex = 1
      object PBLegend: TPaintBox
        Left = 0
        Top = 267
        Width = 277
        Height = 30
        Align = alBottom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        OnPaint = PBLegendPaint
      end
      object ScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 277
        Height = 267
        VertScrollBar.Smooth = True
        VertScrollBar.Style = ssHotTrack
        VertScrollBar.Tracking = True
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
        object Image: TImage
          Left = 0
          Top = 0
          Width = 256
          Height = 2048
          ParentShowHint = False
          ShowHint = True
          Stretch = True
          OnMouseMove = ImageMouseMove
        end
      end
    end
    object TSSMBStats: TTabSheet
      Caption = 'SMB Stats'
      ImageIndex = 2
      object LVSMB: TListView
        Left = 0
        Top = 0
        Width = 277
        Height = 297
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'SMB Size'
            Width = 60
          end
          item
            Alignment = taRightJustify
            Caption = 'Virtual'
            Width = 75
          end
          item
            Alignment = taRightJustify
            Caption = 'User'
            Width = 75
          end
          item
            Alignment = taRightJustify
            Caption = 'Blocks'
          end>
        ColumnClick = False
        FlatScrollBars = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object BBRefresh: TBitBtn
    Left = 236
    Top = 0
    Width = 44
    Height = 17
    Caption = 'Refresh'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = BBRefreshClick
  end
  object ImageList1: TImageList
    Left = 24
    Top = 48
  end
end
