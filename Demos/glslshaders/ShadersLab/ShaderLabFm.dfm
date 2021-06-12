object FormShaderLab: TFormShaderLab
  Left = 0
  Top = 0
  Caption = 'FormShaderLab'
  ClientHeight = 581
  ClientWidth = 950
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 353
    Height = 581
    Align = alLeft
    TabOrder = 0
    object Label54: TLabel
      Left = 148
      Top = 502
      Width = 32
      Height = 13
      Caption = 'Object'
    end
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 351
      Height = 496
      ActivePage = TabSheet1
      Align = alTop
      MultiLine = True
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Fur'
        object lblFurDistance: TLabel
          Left = 3
          Top = 53
          Width = 55
          Height = 13
          Caption = 'Fur Length '
        end
        object Label10: TLabel
          Left = 3
          Top = 205
          Width = 51
          Height = 13
          Caption = 'Blend Dest'
        end
        object Label11: TLabel
          Left = 3
          Top = 179
          Width = 44
          Height = 13
          Caption = 'Blend Src'
        end
        object lblFurPassCount1: TLabel
          Left = 3
          Top = 28
          Width = 57
          Height = 13
          Caption = 'Pass Count '
        end
        object lblFurLength: TLabel
          Left = 287
          Top = 53
          Width = 16
          Height = 13
          Caption = '0.3'
        end
        object Label7: TLabel
          Left = 3
          Top = 75
          Width = 59
          Height = 13
          Caption = 'Max Length '
        end
        object lblFurMaxLength: TLabel
          Left = 287
          Top = 76
          Width = 16
          Height = 13
          Caption = '3.0'
        end
        object lblFurPassCount: TLabel
          Left = 287
          Top = 28
          Width = 12
          Height = 13
          Caption = '16'
        end
        object Label12: TLabel
          Left = 3
          Top = 97
          Width = 36
          Height = 13
          Caption = 'Density'
        end
        object lblFurDensity: TLabel
          Left = 287
          Top = 97
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label6: TLabel
          Left = 3
          Top = 233
          Width = 56
          Height = 13
          Caption = 'Light Power'
        end
        object lblFurLightPower: TLabel
          Left = 287
          Top = 233
          Width = 16
          Height = 13
          Caption = '2.5'
        end
        object Label8: TLabel
          Left = 3
          Top = 263
          Width = 56
          Height = 13
          Caption = 'Color Scale '
        end
        object Label9: TLabel
          Left = 3
          Top = 285
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape1: TShape
          Left = 83
          Top = 262
          Width = 64
          Height = 15
          OnMouseDown = Shape1MouseDown
        end
        object Shape2: TShape
          Left = 83
          Top = 283
          Width = 64
          Height = 15
          OnMouseDown = Shape2MouseDown
        end
        object Label63: TLabel
          Left = 2
          Top = 129
          Width = 59
          Height = 13
          Caption = 'Gravity XYZ '
        end
        object chkFurShader: TCheckBox
          Left = 3
          Top = 3
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkFurShaderClick
        end
        object tbFurLength: TTrackBar
          Left = 65
          Top = 49
          Width = 216
          Height = 26
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbFurLengthChange
        end
        object cbxFurBlendSrc: TComboBox
          Left = 72
          Top = 176
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 3
          TabOrder = 2
          Text = 'ONE MINUS SRC COLOR'
          OnChange = cbxFurBlendSrcChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
        object cbxFurBlendDest: TComboBox
          Left = 72
          Top = 203
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 7
          TabOrder = 3
          Text = 'MINUS SRC ALPHA'
          OnChange = cbxFurBlendDestChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
        object tbFurPassCount: TTrackBar
          Left = 65
          Top = 24
          Width = 216
          Height = 23
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 8
          Position = 16
          TabOrder = 4
          TickStyle = tsNone
          OnChange = tbFurPassCountChange
        end
        object tbFurMaxLength: TTrackBar
          Left = 65
          Top = 72
          Width = 216
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 300
          TabOrder = 5
          TickStyle = tsNone
          OnChange = tbFurMaxLengthChange
        end
        object chkFurRandomLength: TCheckBox
          Left = 72
          Top = 154
          Width = 201
          Height = 17
          Caption = 'Random Fur Length'
          TabOrder = 6
          OnClick = chkFurRandomLengthClick
        end
        object tbFurDensity: TTrackBar
          Left = 65
          Top = 94
          Width = 216
          Height = 26
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 7
          TickStyle = tsNone
          OnChange = tbFurDensityChange
        end
        object tbFurLightPower: TTrackBar
          Left = 65
          Top = 230
          Width = 216
          Height = 26
          Max = 1000
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 250
          TabOrder = 8
          TickStyle = tsNone
          OnChange = tbFurLightPowerChange
        end
        object Button8: TButton
          Left = 3
          Top = 315
          Width = 184
          Height = 25
          Caption = 'Load Main Texture'
          TabOrder = 9
          OnClick = Button8Click
        end
        object Button9: TButton
          Left = 3
          Top = 346
          Width = 184
          Height = 25
          Caption = 'Load Noise Texture'
          TabOrder = 10
          OnClick = Button9Click
        end
        object edtFurGravityX: TEdit
          Left = 71
          Top = 124
          Width = 57
          Height = 21
          TabOrder = 11
          Text = '0.0'
          OnChange = edtFurGravityXChange
          OnKeyPress = EditFloatKeyPress
        end
        object edtFurGravityY: TEdit
          Left = 141
          Top = 124
          Width = 57
          Height = 21
          TabOrder = 12
          Text = '-2.0'
          OnChange = edtFurGravityYChange
          OnKeyPress = EditFloatKeyPress
        end
        object edtFurGravityZ: TEdit
          Left = 216
          Top = 124
          Width = 57
          Height = 21
          TabOrder = 13
          Text = '0.0'
          OnChange = edtFurGravityZChange
          OnKeyPress = EditFloatKeyPress
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Lattice'
        ImageIndex = 1
        object lblLatticeScaleX: TLabel
          Left = 291
          Top = 41
          Width = 12
          Height = 13
          Caption = '10'
        end
        object lblLatticeThresholdX: TLabel
          Left = 289
          Top = 93
          Width = 22
          Height = 13
          Caption = '0.15'
        end
        object lblLatticeScaleY: TLabel
          Left = 290
          Top = 67
          Width = 12
          Height = 13
          Caption = '40'
        end
        object Label4: TLabel
          Left = 16
          Top = 147
          Width = 74
          Height = 13
          Caption = 'Specular Power'
        end
        object Label5: TLabel
          Left = 16
          Top = 172
          Width = 56
          Height = 13
          Caption = 'Light Power'
        end
        object lblLatticeThresholdY: TLabel
          Left = 290
          Top = 118
          Width = 22
          Height = 13
          Caption = '0.30'
        end
        object lblLatticeSpecularPower: TLabel
          Left = 290
          Top = 147
          Width = 16
          Height = 13
          Caption = '8.0'
        end
        object lblLatticeLightPower: TLabel
          Left = 290
          Top = 173
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label23: TLabel
          Left = 10
          Top = 41
          Width = 34
          Height = 13
          Caption = 'Scale X'
        end
        object Label29: TLabel
          Left = 10
          Top = 67
          Width = 34
          Height = 13
          Caption = 'Scale Y'
        end
        object Label31: TLabel
          Left = 10
          Top = 93
          Width = 56
          Height = 13
          Caption = 'Threshold X'
        end
        object Label33: TLabel
          Left = 10
          Top = 122
          Width = 56
          Height = 13
          Caption = 'Threshold Y'
        end
        object Label35: TLabel
          Left = 10
          Top = 200
          Width = 62
          Height = 13
          Caption = 'Diffuse Color'
        end
        object Shape10: TShape
          Left = 90
          Top = 198
          Width = 64
          Height = 15
          OnMouseDown = Shape10MouseDown
        end
        object Label38: TLabel
          Left = 10
          Top = 223
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape11: TShape
          Left = 90
          Top = 222
          Width = 64
          Height = 15
          Brush.Color = 1381653
          OnMouseDown = Shape11MouseDown
        end
        object Label39: TLabel
          Left = 10
          Top = 244
          Width = 69
          Height = 13
          Caption = 'Specular Color'
        end
        object Shape12: TShape
          Left = 90
          Top = 244
          Width = 64
          Height = 15
          OnMouseDown = Shape12MouseDown
        end
        object tbLatticeScaleX: TTrackBar
          Left = 73
          Top = 37
          Width = 215
          Height = 30
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 10
          TabOrder = 0
          TickStyle = tsNone
          OnChange = tbLatticeScaleXChange
        end
        object tbLatticeThresholdX: TTrackBar
          Left = 73
          Top = 89
          Width = 215
          Height = 27
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 15
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbLatticeThresholdXChange
        end
        object chkLatticeShader: TCheckBox
          Left = 16
          Top = 12
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 2
          OnClick = chkLatticeShaderClick
        end
        object tbLatticeScaleY: TTrackBar
          Left = 73
          Top = 63
          Width = 215
          Height = 32
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 40
          TabOrder = 3
          TickStyle = tsNone
          OnChange = tbLatticeScaleYChange
        end
        object tbLatticeThresholdY: TTrackBar
          Left = 73
          Top = 114
          Width = 211
          Height = 27
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 4
          TickStyle = tsNone
          OnChange = tbLatticeThresholdYChange
        end
        object tbLatticeSpecularPower: TTrackBar
          Left = 96
          Top = 143
          Width = 188
          Height = 32
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 800
          TabOrder = 5
          TickStyle = tsNone
          OnChange = tbLatticeSpecularPowerChange
        end
        object tbLatticeLightPower: TTrackBar
          Left = 96
          Top = 166
          Width = 188
          Height = 32
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 6
          TickStyle = tsNone
          OnChange = tbLatticeLightPowerChange
        end
        object Button7: TButton
          Left = 16
          Top = 275
          Width = 184
          Height = 25
          Caption = 'Load Main Texture'
          TabOrder = 7
          OnClick = Button3Click
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Erosion'
        ImageIndex = 2
        object Label1: TLabel
          Left = 8
          Top = 46
          Width = 67
          Height = 13
          Caption = 'Erosion factor'
        end
        object lblErosionFactor: TLabel
          Left = 295
          Top = 47
          Width = 22
          Height = 13
          Caption = '0.35'
        end
        object Label3: TLabel
          Left = 8
          Top = 79
          Width = 63
          Height = 13
          Caption = 'Erosion Scale'
        end
        object lblErosionScale: TLabel
          Left = 295
          Top = 79
          Width = 22
          Height = 13
          Caption = '0.03'
        end
        object Label25: TLabel
          Left = 8
          Top = 111
          Width = 84
          Height = 13
          Caption = 'Intensity factor 1'
        end
        object lblErosionIFactor1: TLabel
          Left = 295
          Top = 111
          Width = 22
          Height = 13
          Caption = '0.75'
        end
        object Label28: TLabel
          Left = 8
          Top = 142
          Width = 86
          Height = 13
          Caption = 'Intensity Factor 2'
        end
        object lblerosionIFactor2: TLabel
          Left = 295
          Top = 143
          Width = 22
          Height = 13
          Caption = '1.95'
        end
        object Label2: TLabel
          Left = 8
          Top = 200
          Width = 71
          Height = 13
          Caption = 'Ambient factor'
        end
        object lblErosionAmbientF: TLabel
          Left = 295
          Top = 200
          Width = 22
          Height = 13
          Caption = '0.80'
        end
        object Label27: TLabel
          Left = 8
          Top = 172
          Width = 66
          Height = 13
          Caption = 'Diffuse factor'
        end
        object lblErosionDiffuseF: TLabel
          Left = 295
          Top = 173
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label30: TLabel
          Left = 8
          Top = 228
          Width = 73
          Height = 13
          Caption = 'Specular factor'
        end
        object lblErosionSpecularF: TLabel
          Left = 295
          Top = 228
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label32: TLabel
          Left = 8
          Top = 260
          Width = 97
          Height = 13
          Caption = 'Specular Roughness'
        end
        object lblErosionSpecularR: TLabel
          Left = 297
          Top = 260
          Width = 22
          Height = 13
          Caption = '0.45'
        end
        object Label34: TLabel
          Left = 8
          Top = 292
          Width = 109
          Height = 13
          Caption = 'Anisotropic Roughness'
        end
        object lblErosionAnisoR: TLabel
          Left = 297
          Top = 292
          Width = 22
          Height = 13
          Caption = '0.35'
        end
        object Label36: TLabel
          Left = 6
          Top = 327
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape8: TShape
          Left = 86
          Top = 325
          Width = 64
          Height = 15
          Brush.Color = 2105376
          OnMouseDown = Shape8MouseDown
        end
        object Shape9: TShape
          Left = 86
          Top = 346
          Width = 64
          Height = 15
          Brush.Color = 14540253
          OnMouseDown = Shape9MouseDown
        end
        object Label37: TLabel
          Left = 6
          Top = 346
          Width = 69
          Height = 13
          Caption = 'Specular Color'
        end
        object chkErosionShader: TCheckBox
          Left = 24
          Top = 20
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkErosionShaderClick
        end
        object tbErosionFactor: TTrackBar
          Left = 98
          Top = 43
          Width = 191
          Height = 26
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 35
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbErosionFactorChange
        end
        object tberosionScale: TTrackBar
          Left = 98
          Top = 75
          Width = 191
          Height = 26
          Max = 100
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 3
          TabOrder = 2
          TickStyle = tsNone
          OnChange = tberosionScaleChange
        end
        object tbErosionIFactor1: TTrackBar
          Left = 98
          Top = 107
          Width = 191
          Height = 26
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 75
          TabOrder = 3
          TickStyle = tsNone
          OnChange = tbErosionIFactor1Change
        end
        object tbErosionIFactor2: TTrackBar
          Left = 100
          Top = 139
          Width = 189
          Height = 26
          Max = 200
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 195
          TabOrder = 4
          TickStyle = tsNone
          OnChange = tbErosionIFactor2Change
        end
        object tbErosionAmbientF: TTrackBar
          Left = 98
          Top = 194
          Width = 191
          Height = 28
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 5
          TickStyle = tsNone
          OnChange = tbErosionAmbientFChange
        end
        object tbErosionDiffuseF: TTrackBar
          Left = 99
          Top = 169
          Width = 190
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 6
          TickStyle = tsNone
          OnChange = tbErosionDiffuseFChange
        end
        object tbErosionSpecularF: TTrackBar
          Left = 98
          Top = 224
          Width = 191
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 7
          TickStyle = tsNone
          OnChange = tbErosionSpecularFChange
        end
        object tbErosionSpecularR: TTrackBar
          Left = 111
          Top = 256
          Width = 180
          Height = 26
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 45
          TabOrder = 8
          TickStyle = tsNone
          OnChange = tbErosionSpecularRChange
        end
        object tbErosionAnisoR: TTrackBar
          Left = 123
          Top = 288
          Width = 166
          Height = 26
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 35
          TabOrder = 9
          TickStyle = tsNone
          OnChange = tbErosionAnisoRChange
        end
        object Button5: TButton
          Left = 3
          Top = 367
          Width = 184
          Height = 25
          Caption = 'Load Main Texture'
          TabOrder = 10
          OnClick = Button5Click
        end
        object Button6: TButton
          Left = 3
          Top = 398
          Width = 184
          Height = 25
          Caption = 'Load Erosion Texture'
          TabOrder = 11
          OnClick = Button6Click
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Ivory'
        ImageIndex = 3
        object chkIvoryShader: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkIvoryShaderClick
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Gootch'
        ImageIndex = 4
        object Label13: TLabel
          Left = 16
          Top = 47
          Width = 62
          Height = 13
          Caption = 'Diffuse Color'
        end
        object Shape3: TShape
          Left = 96
          Top = 46
          Width = 64
          Height = 15
          Brush.Color = clSilver
          OnMouseDown = Shape3MouseDown
        end
        object Label14: TLabel
          Left = 16
          Top = 67
          Width = 56
          Height = 13
          Caption = 'Warm Color'
        end
        object Shape4: TShape
          Left = 96
          Top = 67
          Width = 64
          Height = 15
          Brush.Color = clFuchsia
          OnMouseDown = Shape4MouseDown
        end
        object Label15: TLabel
          Left = 16
          Top = 88
          Width = 49
          Height = 13
          Caption = 'Cool Color'
        end
        object Shape5: TShape
          Left = 96
          Top = 88
          Width = 64
          Height = 15
          Brush.Color = 1145343
          OnMouseDown = Shape5MouseDown
        end
        object Label16: TLabel
          Left = 16
          Top = 110
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape6: TShape
          Left = 96
          Top = 109
          Width = 64
          Height = 15
          Brush.Color = 3158064
          OnMouseDown = Shape6MouseDown
        end
        object Label17: TLabel
          Left = 16
          Top = 131
          Width = 69
          Height = 13
          Caption = 'Specular Color'
        end
        object Shape7: TShape
          Left = 96
          Top = 130
          Width = 64
          Height = 15
          OnMouseDown = Shape7MouseDown
        end
        object Label18: TLabel
          Left = 16
          Top = 163
          Width = 66
          Height = 13
          Caption = 'Diffuse factor'
        end
        object lblGoochDFactor: TLabel
          Left = 303
          Top = 164
          Width = 22
          Height = 13
          Caption = '0.80'
        end
        object Label20: TLabel
          Left = 16
          Top = 195
          Width = 60
          Height = 13
          Caption = 'Warm factor'
        end
        object lblGoochWFactor: TLabel
          Left = 303
          Top = 196
          Width = 22
          Height = 13
          Caption = '0.55'
        end
        object Label22: TLabel
          Left = 16
          Top = 227
          Width = 53
          Height = 13
          Caption = 'Cool factor'
        end
        object lblGoochCFactor: TLabel
          Left = 303
          Top = 228
          Width = 22
          Height = 13
          Caption = '0.30'
        end
        object Label24: TLabel
          Left = 16
          Top = 259
          Width = 71
          Height = 13
          Caption = 'Ambient factor'
        end
        object lblGoochAFactor: TLabel
          Left = 303
          Top = 260
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label26: TLabel
          Left = 16
          Top = 292
          Width = 73
          Height = 13
          Caption = 'Specular factor'
        end
        object lblGoochSFactor: TLabel
          Left = 303
          Top = 292
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label40: TLabel
          Left = 16
          Top = 323
          Width = 55
          Height = 13
          Caption = 'Blend Mode'
        end
        object Label41: TLabel
          Left = 164
          Top = 47
          Width = 27
          Height = 13
          Caption = 'Alpha'
        end
        object lblGoochAlpha: TLabel
          Left = 309
          Top = 47
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object chkGoochShader: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkGoochShaderClick
        end
        object tbGoochDFactor: TTrackBar
          Left = 96
          Top = 160
          Width = 201
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbGoochDFactorChange
        end
        object tbGoochWFactor: TTrackBar
          Left = 96
          Top = 192
          Width = 201
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 55
          TabOrder = 2
          TickStyle = tsNone
          OnChange = tbGoochWFactorChange
        end
        object tbGoochCFactor: TTrackBar
          Left = 96
          Top = 224
          Width = 201
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 30
          TabOrder = 3
          TickStyle = tsNone
          OnChange = tbGoochCFactorChange
        end
        object tbGoochAFactor: TTrackBar
          Left = 96
          Top = 256
          Width = 201
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 4
          TickStyle = tsNone
          OnChange = tbGoochAFactorChange
        end
        object tbGoochSFactor: TTrackBar
          Left = 96
          Top = 288
          Width = 201
          Height = 26
          Max = 500
          Min = 1
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 5
          TickStyle = tsNone
          OnChange = tbGoochSFactorChange
        end
        object cbxGootchBlendMode: TComboBox
          Left = 104
          Top = 320
          Width = 193
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 6
          Text = 'Opaque'
          OnChange = cbxGootchBlendModeChange
          Items.Strings = (
            'Opaque'
            'Transparency'
            'Additive'
            'AlphaTest50'
            'AlphaTest100'
            'Modulate'
            'DestColorOne'
            'DestAlphaOne')
        end
        object tbGoochAlpha: TTrackBar
          Left = 196
          Top = 44
          Width = 114
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 7
          TickStyle = tsNone
          OnChange = tbGoochAlphaChange
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'S.E.M'
        ImageIndex = 5
        object Label19: TLabel
          Left = 16
          Top = 44
          Width = 66
          Height = 13
          Caption = 'Diffuse factor'
        end
        object lblSemDiffuseF: TLabel
          Left = 303
          Top = 45
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label42: TLabel
          Left = 16
          Top = 72
          Width = 71
          Height = 13
          Caption = 'Ambient factor'
        end
        object lblSemAmbientF: TLabel
          Left = 303
          Top = 71
          Width = 22
          Height = 13
          Caption = '0.80'
        end
        object Label44: TLabel
          Left = 16
          Top = 100
          Width = 73
          Height = 13
          Caption = 'Specular factor'
        end
        object lblSemSpecularF: TLabel
          Left = 303
          Top = 100
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label46: TLabel
          Left = 14
          Top = 130
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape13: TShape
          Left = 94
          Top = 128
          Width = 64
          Height = 15
          Brush.Color = 2105376
          OnMouseDown = Shape13MouseDown
        end
        object Label47: TLabel
          Left = 14
          Top = 149
          Width = 69
          Height = 13
          Caption = 'Specular Color'
        end
        object Shape14: TShape
          Left = 94
          Top = 149
          Width = 64
          Height = 15
          Brush.Color = 14540253
          OnMouseDown = Shape14MouseDown
        end
        object tbSemDiffuseF: TTrackBar
          Left = 107
          Top = 39
          Width = 190
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 0
          TickStyle = tsNone
          OnChange = tbSemDiffuseFChange
        end
        object tbSemAmbientF: TTrackBar
          Left = 106
          Top = 66
          Width = 191
          Height = 28
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbSemAmbientFChange
        end
        object tbSemSpecularF: TTrackBar
          Left = 106
          Top = 96
          Width = 191
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 2
          TickStyle = tsNone
          OnChange = tbSemSpecularFChange
        end
        object chkSEMShader: TCheckBox
          Left = 24
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 3
          OnClick = chkSEMShaderClick
        end
        object Button4: TButton
          Left = 16
          Top = 179
          Width = 184
          Height = 25
          Caption = 'Load MatCap Texture'
          TabOrder = 4
          OnClick = Button4Click
        end
      end
      object Displacement: TTabSheet
        Caption = 'Displacement'
        ImageIndex = 6
        object Label21: TLabel
          Left = 24
          Top = 52
          Width = 66
          Height = 13
          Caption = 'Diffuse factor'
        end
        object Label43: TLabel
          Left = 24
          Top = 80
          Width = 71
          Height = 13
          Caption = 'Ambient factor'
        end
        object Label45: TLabel
          Left = 24
          Top = 108
          Width = 73
          Height = 13
          Caption = 'Specular factor'
        end
        object lblVDSpecularF: TLabel
          Left = 311
          Top = 108
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object lblVDAmbientF: TLabel
          Left = 311
          Top = 79
          Width = 22
          Height = 13
          Caption = '0.80'
        end
        object lblVDDiffuseF: TLabel
          Left = 311
          Top = 53
          Width = 22
          Height = 13
          Caption = '0.90'
        end
        object Label51: TLabel
          Left = 22
          Top = 138
          Width = 67
          Height = 13
          Caption = 'Ambient Color'
        end
        object Shape15: TShape
          Left = 102
          Top = 136
          Width = 64
          Height = 15
          Brush.Color = 2105376
          OnMouseDown = Shape13MouseDown
        end
        object Label52: TLabel
          Left = 22
          Top = 157
          Width = 69
          Height = 13
          Caption = 'Specular Color'
        end
        object Shape16: TShape
          Left = 102
          Top = 157
          Width = 64
          Height = 15
          Brush.Color = 14540253
          OnMouseDown = Shape14MouseDown
        end
        object Label48: TLabel
          Left = 22
          Top = 188
          Width = 26
          Height = 13
          Caption = 'Noise'
        end
        object lblVDNoise: TLabel
          Left = 311
          Top = 188
          Width = 22
          Height = 13
          Caption = '10.0'
        end
        object Label49: TLabel
          Left = 22
          Top = 211
          Width = 30
          Height = 13
          Caption = 'Period'
        end
        object lblVDPeriod: TLabel
          Left = 311
          Top = 211
          Width = 16
          Height = 13
          Caption = '5.0'
        end
        object Label53: TLabel
          Left = 22
          Top = 236
          Width = 54
          Height = 13
          Caption = 'Noise Scale'
        end
        object lblVDNScale: TLabel
          Left = 311
          Top = 237
          Width = 22
          Height = 13
          Caption = '0.05'
        end
        object Label55: TLabel
          Left = 22
          Top = 260
          Width = 53
          Height = 13
          Caption = 'Turbulence'
        end
        object lblVDTurb: TLabel
          Left = 311
          Top = 261
          Width = 16
          Height = 13
          Caption = '0.5'
        end
        object Label57: TLabel
          Left = 22
          Top = 284
          Width = 91
          Height = 13
          Caption = 'Displacement Scale'
        end
        object lblVDDispScale: TLabel
          Left = 311
          Top = 284
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label50: TLabel
          Left = 22
          Top = 307
          Width = 56
          Height = 13
          Caption = 'Time Factor'
        end
        object lblVDTimeF: TLabel
          Left = 311
          Top = 307
          Width = 22
          Height = 13
          Caption = '0.05'
        end
        object chkVDShader: TCheckBox
          Left = 32
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkVDShaderClick
        end
        object tbVDDiffuseF: TTrackBar
          Left = 115
          Top = 47
          Width = 190
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbVDDiffuseFChange
        end
        object tbVDAmbientF: TTrackBar
          Left = 114
          Top = 74
          Width = 191
          Height = 28
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 80
          TabOrder = 2
          TickStyle = tsNone
          OnChange = tbVDAmbientFChange
        end
        object tbVDSpecularF: TTrackBar
          Left = 114
          Top = 104
          Width = 191
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 90
          TabOrder = 3
          TickStyle = tsNone
          OnChange = tbVDSpecularFChange
        end
        object chkVDAnimate: TCheckBox
          Left = 128
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Animate'
          TabOrder = 4
        end
        object tbVDNoise: TTrackBar
          Left = 114
          Top = 184
          Width = 191
          Height = 26
          Max = 5000
          PageSize = 10
          Frequency = 10
          Position = 1000
          TabOrder = 5
          TickStyle = tsNone
          OnChange = tbVDNoiseChange
        end
        object tbVDPeriod: TTrackBar
          Left = 114
          Top = 207
          Width = 191
          Height = 26
          Max = 500
          PageSize = 10
          Frequency = 10
          Position = 500
          TabOrder = 6
          TickStyle = tsNone
          OnChange = tbVDPeriodChange
        end
        object tbVDNScale: TTrackBar
          Left = 114
          Top = 230
          Width = 191
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 5
          TabOrder = 7
          TickStyle = tsNone
          OnChange = tbVDNScaleChange
        end
        object tbVDTurb: TTrackBar
          Left = 114
          Top = 256
          Width = 191
          Height = 26
          Max = 200
          PageSize = 10
          Frequency = 10
          Position = 50
          TabOrder = 8
          TickStyle = tsNone
          OnChange = tbVDTurbChange
        end
        object tbVDDispScale: TTrackBar
          Left = 114
          Top = 280
          Width = 191
          Height = 26
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 9
          TickStyle = tsNone
          OnChange = tbVDDispScaleChange
        end
        object tbVDTimeF: TTrackBar
          Left = 114
          Top = 303
          Width = 191
          Height = 26
          Max = 1000
          PageSize = 10
          Frequency = 10
          Position = 5
          TabOrder = 10
          TickStyle = tsNone
          OnChange = tbVDTimeFChange
        end
        object Button3: TButton
          Left = 16
          Top = 344
          Width = 113
          Height = 25
          Caption = 'Reset Time'
          TabOrder = 11
          OnClick = Button3Click
        end
        object Button1: TButton
          Left = 16
          Top = 375
          Width = 184
          Height = 25
          Caption = 'Load Main Texture'
          TabOrder = 12
          OnClick = Button1Click
        end
      end
      object TabSheet7: TTabSheet
        Caption = 'Glass'
        ImageIndex = 7
        object Label56: TLabel
          Left = 11
          Top = 36
          Width = 29
          Height = 13
          Caption = 'Depth'
        end
        object Label58: TLabel
          Left = 24
          Top = 66
          Width = 16
          Height = 13
          Caption = 'Mix'
        end
        object Label59: TLabel
          Left = 14
          Top = 146
          Width = 62
          Height = 13
          Caption = 'Diffuse Color'
        end
        object Shape17: TShape
          Left = 87
          Top = 145
          Width = 64
          Height = 15
          Brush.Color = 2105376
          OnMouseDown = Shape17MouseDown
        end
        object lblGlassDepth: TLabel
          Left = 285
          Top = 36
          Width = 16
          Height = 13
          Caption = '0.1'
        end
        object lblGlassMix: TLabel
          Left = 283
          Top = 66
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label60: TLabel
          Left = 10
          Top = 99
          Width = 27
          Height = 13
          Caption = 'Alpha'
        end
        object lblGlassAlpha: TLabel
          Left = 282
          Top = 99
          Width = 16
          Height = 13
          Caption = '1.0'
        end
        object Label61: TLabel
          Left = 18
          Top = 179
          Width = 44
          Height = 13
          Caption = 'Blend Src'
        end
        object Label62: TLabel
          Left = 18
          Top = 205
          Width = 51
          Height = 13
          Caption = 'Blend Dest'
        end
        object tbGlassDepth: TTrackBar
          Left = 46
          Top = 33
          Width = 235
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 10
          TabOrder = 0
          TickStyle = tsNone
          OnChange = tbGlassDepthChange
        end
        object tbGlassMix: TTrackBar
          Left = 45
          Top = 62
          Width = 236
          Height = 28
          Max = 200
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbGlassMixChange
        end
        object Button10: TButton
          Left = 3
          Top = 254
          Width = 184
          Height = 25
          Caption = 'Load Refraction Texture'
          TabOrder = 2
          OnClick = Button10Click
        end
        object chkGlassShader: TCheckBox
          Left = 24
          Top = 8
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 3
          OnClick = chkGlassShaderClick
        end
        object tbGlassAlpha: TTrackBar
          Left = 45
          Top = 96
          Width = 235
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 100
          TabOrder = 4
          TickStyle = tsNone
          OnChange = tbGlassAlphaChange
        end
        object cbxGlassBlendSrc: TComboBox
          Left = 87
          Top = 176
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 6
          TabOrder = 5
          Text = 'SRC ALPHA'
          OnChange = cbxGlassBlendSrcChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
        object cbxGlassBlendDst: TComboBox
          Left = 87
          Top = 203
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 8
          TabOrder = 6
          Text = 'DST ALPHA'
          OnChange = cbxGlassBlendDstChange
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA')
        end
      end
      object TabSheet8: TTabSheet
        Caption = 'Toon'
        ImageIndex = 8
        object Label64: TLabel
          Left = 7
          Top = 51
          Width = 66
          Height = 13
          Caption = 'HighLight Size'
        end
        object lblToonHighlightSize: TLabel
          Left = 290
          Top = 51
          Width = 22
          Height = 13
          Caption = '0.95'
        end
        object Label66: TLabel
          Left = 7
          Top = 73
          Width = 38
          Height = 13
          Caption = 'Mid Size'
        end
        object lblToonMidSize: TLabel
          Left = 290
          Top = 73
          Width = 16
          Height = 13
          Caption = '0.5'
        end
        object Label68: TLabel
          Left = 7
          Top = 95
          Width = 60
          Height = 13
          Caption = 'Shadow Size'
        end
        object lblToonShadowSize: TLabel
          Left = 290
          Top = 95
          Width = 22
          Height = 13
          Caption = '0.25'
        end
        object Label70: TLabel
          Left = 7
          Top = 117
          Width = 65
          Height = 13
          Caption = 'Outline Width'
        end
        object lblToonOutlineWidth: TLabel
          Left = 290
          Top = 117
          Width = 22
          Height = 13
          Caption = '0.25'
        end
        object Label72: TLabel
          Left = 8
          Top = 146
          Width = 69
          Height = 13
          Caption = 'Highlight Color'
        end
        object Shape18: TShape
          Left = 118
          Top = 146
          Width = 64
          Height = 15
          Brush.Color = 15658734
          OnMouseDown = Shape18MouseDown
        end
        object Label73: TLabel
          Left = 8
          Top = 167
          Width = 44
          Height = 13
          Caption = 'Mid Color'
        end
        object Shape19: TShape
          Left = 118
          Top = 167
          Width = 64
          Height = 15
          Brush.Color = 13421772
          OnMouseDown = Shape19MouseDown
        end
        object Label74: TLabel
          Left = 8
          Top = 188
          Width = 104
          Height = 13
          Caption = 'Lighten Shadow Color'
        end
        object Shape20: TShape
          Left = 118
          Top = 188
          Width = 64
          Height = 15
          Brush.Color = clGray
          OnMouseDown = Shape20MouseDown
        end
        object Label75: TLabel
          Left = 8
          Top = 209
          Width = 103
          Height = 13
          Caption = 'Darken Shadow Color'
        end
        object Shape21: TShape
          Left = 118
          Top = 209
          Width = 64
          Height = 15
          Brush.Color = 3158064
          OnMouseDown = Shape21MouseDown
        end
        object Label76: TLabel
          Left = 7
          Top = 230
          Width = 62
          Height = 13
          Caption = 'Outline Color'
        end
        object Shape22: TShape
          Left = 118
          Top = 230
          Width = 64
          Height = 15
          Brush.Color = clBlack
          OnMouseDown = Shape22MouseDown
        end
        object chkToonShader: TCheckBox
          Left = 8
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = chkToonShaderClick
        end
        object tbToonHighlightSize: TTrackBar
          Left = 75
          Top = 48
          Width = 213
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 95
          TabOrder = 1
          TickStyle = tsNone
          OnChange = tbToonHighlightSizeChange
        end
        object tbToonMidSize: TTrackBar
          Left = 75
          Top = 70
          Width = 213
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 50
          TabOrder = 2
          TickStyle = tsNone
          OnChange = tbToonMidSizeChange
        end
        object tbToonShadowSize: TTrackBar
          Left = 75
          Top = 92
          Width = 213
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 25
          TabOrder = 3
          TickStyle = tsNone
          OnChange = tbToonShadowSizeChange
        end
        object tbToonOutlineWidth: TTrackBar
          Left = 75
          Top = 114
          Width = 213
          Height = 26
          Max = 100
          PageSize = 10
          Frequency = 10
          Position = 25
          TabOrder = 4
          TickStyle = tsNone
          OnChange = tbToonOutlineWidthChange
        end
      end
    end
    object chkAnimScene: TCheckBox
      Left = 13
      Top = 499
      Width = 97
      Height = 17
      Caption = 'Animate Scene'
      TabOrder = 1
    end
    object chkLightmoving: TCheckBox
      Left = 13
      Top = 522
      Width = 97
      Height = 17
      Caption = 'Light moving'
      TabOrder = 2
    end
    object cbxObjects: TComboBox
      Left = 187
      Top = 497
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'Suzanne'
      OnChange = cbxObjectsChange
      Items.Strings = (
        'Suzanne'
        'Knot'
        'Spoutnik'
        'Rectangle Spirale'
        'Geode'
        'Syamil'
        'GLTorus'
        'GLSphere')
    end
    object Button2: TButton
      Left = 148
      Top = 524
      Width = 184
      Height = 25
      Caption = 'Choose Background Color'
      TabOrder = 4
      OnClick = Button2Click
    end
    object chkBackgroundImg: TCheckBox
      Left = 128
      Top = 555
      Width = 14
      Height = 17
      Hint = 'Show Background Texture'
      TabOrder = 5
      OnClick = chkBackgroundImgClick
    end
    object Button11: TButton
      Left = 148
      Top = 551
      Width = 184
      Height = 25
      Caption = 'Load Background Texture'
      Enabled = False
      TabOrder = 6
      OnClick = Button11Click
    end
  end
  object Viewer: TGLSceneViewer
    Left = 353
    Top = 0
    Width = 597
    Height = 581
    Camera = Camera
    Buffer.BackgroundColor = clBlack
    Buffer.AntiAliasing = aa2x
    FieldOfView = 160.468215942382800000
    PenAsTouch = False
    Align = alClient
    TabOrder = 1
  end
  object MaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'MainTexture'
        Tag = 0
        Material.BackProperties.Shininess = 10
        Material.FrontProperties.Shininess = 10
        Material.FrontProperties.Specular.Color = {9998983E9998983E9998983E0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'NoiseTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'ShaderMaterial'
        Tag = 0
        Material.FrontProperties.Emission.Color = {77BE9F3D7368913D2506813D0000803F}
        Material.FrontProperties.Specular.Color = {B6F35D3F6DE75B3F6DE75B3F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Material.Texture.KeepImageAfterTransfer = True
      end
      item
        Name = 'ErosionNoiseTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'ErosionMainTexture'
        Tag = 0
        Material.BlendingMode = bmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'ErosionTexture'
        Tag = 0
        Material.BlendingMode = bmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'MatCapTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'ExplosionTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'EnvMap'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.Disabled = False
      end
      item
        Name = 'RefractMap'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'BackgroundTex'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'LibMaterial'
        Tag = 0
      end>
    Left = 416
    Top = 24
  end
  object GLScene1: TGLScene
    Left = 440
    Top = 88
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      NearPlaneBias = 0.001000000047497451
      TargetObject = World
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {0000A0400000A0410000C8420000803F}
    end
    object LightCube: TGLDummyCube
      Position.Coordinates = {000096C30000A040000096430000803F}
      OnProgress = LightCubeProgress
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
    object LightCube2: TGLDummyCube
      Position.Coordinates = {000096430000C842000096430000803F}
      OnProgress = LightCube2Progress
      CubeSize = 1.000000000000000000
      object GLLightSource2: TGLLightSource
        Ambient.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {EAEA6A3FEAEA6A3FA7AD2D3F0000803F}
        LightStyle = lsParallel
        Specular.Color = {0000803F0000003F0000003F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object World: TGLDummyCube
      CubeSize = 1.000000000000000000
      object ScreenBackGround: TGLHUDSprite
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'BackgroundTex'
        Visible = False
        Width = 256.000000000000000000
        Height = 256.000000000000000000
        Rotation = 0.000000000000000000
      end
      object Objects: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLSphere1: TGLSphere
          Material.MaterialLibrary = MaterialLibrary
          Material.LibMaterialName = 'ShaderMaterial'
          Visible = False
          Radius = 45.000000000000000000
          Slices = 64
          Stacks = 64
        end
        object FreeForm: TGLFreeForm
          AutoCentering = [macCenterX, macCenterY, macCenterZ, macUseBarycenter]
          AutoScaling.Coordinates = {0000484200004842000048420000803F}
        end
        object GLTorus1: TGLTorus
          Visible = False
          MajorRadius = 40.000000000000000000
          MinorRadius = 15.000000000000000000
          Rings = 64
          Sides = 64
          StopAngle = 360.000000000000000000
          Parts = [toSides, toStartDisk, toStopDisk]
        end
      end
    end
  end
  object Cadencer: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    OnProgress = CadencerProgress
    Left = 384
    Top = 88
  end
  object ColorDialog: TColorDialog
    Left = 381
    Top = 153
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    ZoomSpeed = 1.100000023841858000
    RotateTargetSpeed = 0.500000000000000000
    FormCaption = 'Shaders Lab - %FPS'
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
    Left = 528
    Top = 24
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 472
    Top = 160
  end
end
