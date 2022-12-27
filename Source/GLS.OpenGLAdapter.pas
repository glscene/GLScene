//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.OpenGLAdapter;

(* OpenGL adapter *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Winapi.Windows,
  System.SysUtils,

  GLS.OpenGLTokens,
  GLS.Strings,
  GLS.Logger,
  GLS.VectorGeometry,
  GLS.VectorTypes;

const
  {$IFDEF CROSSVCL}
    {$IF Defined(LINUX)}
    opengl32 = 'libGL.so';
    glu32 = 'libGLU.so.1';
    {$ELSEIF Defined(MACOS)}
    opengl32 = '/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib';
    glu32 = '/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGLU.dylib';
    {$ELSE}
    Unsupported platform
    {$ENDIF}
  {$ELSE}
  opengl32 = 'OpenGL32.dll';
  glu32 = 'GLU32.dll';
  {$ENDIF}

type
  EOpenGLError = class(Exception);

  TGLExtensionsAndEntryPoints = class
  private
    FBuffer: string;
    FInitialized: boolean;
    FDebug: boolean;
    FDebugIds: Cardinal;
    function CheckExtension(const Extension: string): boolean;
{$IFDEF SUPPORT_WGL}
    procedure ReadWGLExtensions;
    procedure ReadWGLImplementationProperties;
{$ENDIF}
    function GetAddress(const ProcName: string): Pointer;
    function GetAddressNoSuffixes(const ProcName: string): Pointer;
    function GetAddressAlt(const ProcName1, ProcName2: string): Pointer;
    function GetCapAddress: Pointer;
  public
    VERSION_1_0, VERSION_1_1, VERSION_1_2, VERSION_1_3, VERSION_1_4,
      VERSION_1_5, VERSION_2_0, VERSION_2_1, VERSION_3_0, VERSION_3_1,
      VERSION_3_2, VERSION_3_3, VERSION_4_0, VERSION_4_1, VERSION_4_2,
      ARB_blend_func_extended, ARB_color_buffer_float, ARB_compatibility,
      ARB_copy_buffer, ARB_depth_buffer_float, ARB_depth_clamp,
      ARB_depth_texture, ARB_draw_buffers, ARB_draw_buffers_blend,
      ARB_draw_elements_base_vertex, ARB_draw_indirect, ARB_draw_instanced,
      ARB_explicit_attrib_location, ARB_fragment_coord_conventions,
      ARB_fragment_program, ARB_fragment_program_shadow, ARB_fragment_shader,
      ARB_framebuffer_object, ARB_framebuffer_sRGB, ARB_geometry_shader4,
      ARB_gpu_shader_fp64, ARB_gpu_shader5, ARB_half_float_pixel,
      ARB_half_float_vertex, ARB_imaging, ARB_instanced_arrays,
      ARB_map_buffer_range, ARB_matrix_palette, ARB_multisample,
      ARB_multitexture, ARB_occlusion_query, ARB_occlusion_query2,
      ARB_pixel_buffer_object, ARB_point_parameters, ARB_point_sprite,
      ARB_provoking_vertex, ARB_sample_shading, ARB_sampler_objects,
      ARB_seamless_cube_map, ARB_shader_bit_encoding, ARB_shader_subroutine,
      ARB_shader_texture_lod, ARB_shading_language_100, ARB_shadow,
      ARB_shadow_ambient, ARB_shader_objects, ARB_sync, ARB_tessellation_shader,
      ARB_texture_border_clamp, ARB_texture_buffer_object,
      ARB_texture_buffer_object_rgb32, ARB_texture_compression,
      ARB_texture_compression_rgtc, ARB_texture_cube_map,
      ARB_texture_cube_map_array, ARB_texture_env_add, ARB_texture_env_combine,
      ARB_texture_env_crossbar, ARB_texture_env_dot3, ARB_texture_float,
      ARB_texture_gather, ARB_texture_mirrored_repeat, ARB_texture_multisample,
      ARB_texture_non_power_of_two, ARB_texture_query_lod,
      ARB_texture_rectangle, ARB_texture_rg, ARB_texture_rgb10_a2ui,
      ARB_texture_swizzle, ARB_timer_query, ARB_transform_feedback2,
      ARB_transform_feedback3, ARB_transpose_matrix, ARB_uniform_buffer_object,
      ARB_vertex_array_bgra, ARB_vertex_array_object, ARB_vertex_blend,
      ARB_vertex_buffer_object, ARB_vertex_program, ARB_vertex_shader,
      ARB_vertex_type_2_10_10_10_rev, ARB_window_pos,
      ARB_texture_compression_bptc, ARB_get_program_binary,
      ARB_separate_shader_objects, ARB_shader_stencil_export, KHR_debug,
      ARB_clear_buffer_object, ARB_compute_shader, ARB_copy_image,
      ARB_debug_group, ARB_debug_label, ARB_debug_output2,
      ARB_ES3_compatibility, ARB_explicit_uniform_location,
      ARB_fragment_layer_viewport, ARB_framebuffer_no_attachments,
      ARB_internalformat_query2, ARB_invalidate_subdata,
      ARB_multi_draw_indirect, ARB_program_interface_query,
      ARB_shader_image_size, ARB_shader_storage_buffer_object,
      ARB_stencil_texturing, ARB_texture_buffer_range, ARB_texture_query_levels,
      ARB_texture_storage_multisample, ARB_texture_view,
      ARB_vertex_attrib_binding, ARB_robustness_isolation, ARB_cl_event,
      _3DFX_multisample, _3DFX_tbuffer, _3DFX_texture_compression_FXT1,
      ATI_draw_buffers, ATI_texture_compression_3dc, ATI_texture_float,
      ATI_texture_mirror_once, S3_s3tc, EXT_abgr, EXT_bgra,
      EXT_bindable_uniform, EXT_blend_color, EXT_blend_equation_separate,
      EXT_blend_func_separate, EXT_blend_logic_op, EXT_blend_minmax,
      EXT_blend_subtract, EXT_Cg_shader, EXT_clip_volume_hint,
      EXT_compiled_vertex_array, EXT_copy_texture, EXT_depth_bounds_test,
      EXT_draw_buffers2, EXT_draw_instanced, EXT_draw_range_elements,
      EXT_fog_coord, EXT_framebuffer_blit, EXT_framebuffer_multisample,
      EXT_framebuffer_object, EXT_framebuffer_sRGB, EXT_geometry_shader4,
      EXT_gpu_program_parameters, EXT_gpu_shader4, EXT_multi_draw_arrays,
      EXT_multisample, EXT_packed_depth_stencil, EXT_packed_float,
      EXT_packed_pixels, EXT_paletted_texture, EXT_pixel_buffer_object,
      EXT_polygon_offset, EXT_rescale_normal, EXT_secondary_color,
      EXT_separate_specular_color, EXT_shadow_funcs, EXT_shared_texture_palette,
      EXT_stencil_clear_tag, EXT_stencil_two_side, EXT_stencil_wrap,
      EXT_texture3D, EXT_texture_array, EXT_texture_buffer_object,
      EXT_texture_compression_latc, EXT_texture_compression_rgtc,
      EXT_texture_compression_s3tc, EXT_texture_cube_map,
      EXT_texture_edge_clamp, EXT_texture_env_add, EXT_texture_env_combine,
      EXT_texture_env_dot3, EXT_texture_filter_anisotropic, EXT_texture_integer,
      EXT_texture_lod, EXT_texture_lod_bias, EXT_texture_mirror_clamp,
      EXT_texture_object, EXT_texture_rectangle, EXT_texture_sRGB,
      EXT_texture_shared_exponent, EXT_timer_query, EXT_transform_feedback,
      EXT_vertex_array, EXT_texture_sRGB_decode, EXT_direct_state_access,
      EXT_texture_swizzle, HP_occlusion_test, IBM_rasterpos_clip,
      KTX_buffer_region, MESA_resize_buffers, NV_blend_square,
      NV_conditional_render, NV_copy_image, NV_depth_buffer_float, NV_fence,
      NV_float_buffer, NV_fog_distance, NV_geometry_program4,
      NV_light_max_exponent, NV_multisample_filter_hint, NV_occlusion_query,
      NV_point_sprite, NV_primitive_restart, NV_register_combiners,
      NV_shader_buffer_load, NV_texgen_reflection, NV_texture_compression_vtc,
      NV_texture_env_combine4, NV_texture_rectangle, NV_texture_shader,
      NV_texture_shader2, NV_texture_shader3, NV_transform_feedback,
      NV_vertex_array_range, NV_vertex_array_range2,
      NV_vertex_buffer_unified_memory, NV_vertex_program, SGI_color_matrix,
      SGIS_generate_mipmap, SGIS_multisample, SGIS_texture_border_clamp,
      SGIS_texture_color_mask, SGIS_texture_edge_clamp, SGIS_texture_lod,
      SGIX_depth_texture, SGIX_shadow, SGIX_shadow_ambient,
      AMD_vertex_shader_tessellator, WIN_swap_hint, ATI_meminfo,
      NVX_gpu_memory_info, NV_vdpau_interop, NV_path_rendering,
      GREMEDY_frame_terminator, GREMEDY_string_marker, AMDX_debug_output,
      ARB_debug_output: boolean;

    // OpenGL 1.1 core functions and procedures
    BindTexture: procedure(target: Cardinal; texture: Cardinal);stdcall;
    BlendFunc: procedure(sfactor: Cardinal; dfactor: Cardinal);stdcall;
    Clear: procedure(mask: TGLbitfield);stdcall;
    ClearColor: procedure(red, green, blue, alpha: Single);stdcall;
    ClearDepth: procedure(depth: TGLclampd);stdcall;
    ClearStencil: procedure(s: TGLint);stdcall;
    ColorMask: procedure(red, green, blue, alpha: TGLboolean);stdcall;
    CopyTexImage1D: procedure(target: Cardinal; level: TGLint;
      internalFormat: Cardinal; X, y: TGLint; Width: TGLsizei; border: TGLint);stdcall;
    CopyTexImage2D: procedure(target: Cardinal; level: TGLint;
      internalFormat: Cardinal; X, y: TGLint; Width, Height: TGLsizei;
      border: TGLint);stdcall;
    CopyTexSubImage1D: procedure(target: Cardinal; level, xoffset, X, y: TGLint;
      Width: TGLsizei);stdcall;
    CopyTexSubImage2D: procedure(target: Cardinal;
      level, xoffset, yoffset, X, y: TGLint; Width, Height: TGLsizei);stdcall;
    CullFace: procedure(mode: Cardinal);stdcall;
    DeleteTextures: procedure(n: TGLsizei; textures: PGLuint);stdcall;
    DepthFunc: procedure(func: Cardinal);stdcall;
    DepthMask: procedure(flag: TGLboolean);stdcall;
    DepthRange: procedure(zNear, zFar: TGLclampd);stdcall;
    Disable: procedure(cap: Cardinal);stdcall;
    DrawArrays: procedure(mode: Cardinal; First: TGLint; Count: TGLsizei);stdcall;
    DrawBuffer: procedure(mode: Cardinal);stdcall;
    DrawElements: procedure(mode: Cardinal; Count: TGLsizei; atype: Cardinal;
      indices: Pointer);stdcall;
    Enable: procedure(cap: Cardinal);stdcall;
    Finish: procedure();stdcall;
    Flush: procedure();stdcall;
    FrontFace: procedure(mode: Cardinal);stdcall;
    GenTextures: procedure(n: TGLsizei; textures: PGLuint);stdcall;
    GetBooleanv: procedure(pname: Cardinal; params: PGLboolean);stdcall;
    GetDoublev: procedure(pname: Cardinal; params: PGLdouble);stdcall;
    GetError: function: Cardinal;stdcall;
    GetFloatv: procedure(pname: Cardinal; params: PGLfloat);stdcall;
    GetIntegerv: procedure(pname: Cardinal; params: PGLint);stdcall;
    GetPointerv: procedure(pname: Cardinal; var params);stdcall;
    GetString: function(Name: Cardinal): PAnsiChar;stdcall;
    GetTexImage: procedure(target: Cardinal; level: TGLint;
      format, atype: Cardinal; pixels: Pointer);stdcall;
    GetTexLevelParameterfv: procedure(target: Cardinal; level: TGLint;
      pname: Cardinal; params: PGLfloat);stdcall;
    GetTexLevelParameteriv: procedure(target: Cardinal; level: TGLint;
      pname: Cardinal; params: PGLint);stdcall;
    GetTexParameterfv: procedure(target, pname: Cardinal; params: PGLfloat);stdcall;
    GetTexParameteriv: procedure(target, pname: Cardinal; params: PGLint);stdcall;
    Hint: procedure(target, mode: Cardinal);stdcall;
    IsEnabled: function(cap: Cardinal): TGLboolean;stdcall;
    IsTexture: function(texture: Cardinal): TGLboolean;stdcall;
    LineWidth: procedure(Width: TGLfloat);stdcall;
    LogicOp: procedure(opcode: Cardinal);stdcall;
    PixelStoref: procedure(pname: Cardinal; param: TGLfloat);stdcall;
    PixelStorei: procedure(pname: Cardinal; param: TGLint);stdcall;
    PointSize: procedure(size: TGLfloat);stdcall;
    PolygonMode: procedure(face, mode: Cardinal);stdcall;
    PolygonOffset: procedure(factor, units: TGLfloat);stdcall;
    ReadBuffer: procedure(mode: Cardinal);stdcall;
    ReadPixels: procedure(X, y: TGLint; Width, Height: TGLsizei;
      format, atype: Cardinal; pixels: Pointer);stdcall;
    Scissor: procedure(X, y: TGLint; Width, Height: TGLsizei);stdcall;
    StencilFunc: procedure(func: Cardinal; ref: TGLint; mask: Cardinal);stdcall;
    StencilMask: procedure(mask: Cardinal);stdcall;
    StencilOp: procedure(fail, zfail, zpass: Cardinal);stdcall;
    TexImage1D: procedure(target: Cardinal; level, internalFormat: TGLint;
      Width: TGLsizei; border: TGLint; format, atype: Cardinal;
      pixels: Pointer);stdcall;
    TexImage2D: procedure(target: Cardinal; level, internalFormat: TGLint;
      Width, Height: TGLsizei; border: TGLint; format, atype: Cardinal;
      pixels: Pointer);stdcall;
    TexParameterf: procedure(target, pname: Cardinal; param: TGLfloat);stdcall;
    TexParameterfv: procedure(target, pname: Cardinal; params: PGLfloat);stdcall;
    TexParameteri: procedure(target, pname: Cardinal; param: TGLint);stdcall;
    TexParameteriv: procedure(target, pname: Cardinal; params: PGLint);stdcall;
    TexSubImage1D: procedure(target: Cardinal; level, xoffset: TGLint;
      Width: TGLsizei; format, atype: Cardinal; pixels: Pointer);stdcall;
    TexSubImage2D: procedure(target: Cardinal; level, xoffset, yoffset: TGLint;
      Width, Height: TGLsizei; format, atype: Cardinal; pixels: Pointer);stdcall;
    Viewport: procedure(X, y: TGLint; Width, Height: TGLsizei);stdcall;

    // ------------------------------ OpenGL 1.1 deprecated
    Accum: procedure(op: Cardinal; Value: TGLfloat); stdcall;
    AlphaFunc: procedure(func: Cardinal; ref: Single); stdcall;
    AreTexturesResident: function(n: TGLsizei; textures: PGLuint;
      residences: PGLboolean): TGLboolean;stdcall;
    ArrayElement: procedure(i: TGLint); stdcall;
    Begin_: procedure(mode: Cardinal); stdcall;
    Bitmap: procedure(Width: TGLsizei; Height: TGLsizei; xorig, yorig: TGLfloat;
      xmove: TGLfloat; ymove: TGLfloat; Bitmap: Pointer); stdcall;
    CallList: procedure(list: Cardinal); stdcall;
    CallLists: procedure(n: TGLsizei; atype: Cardinal; lists: Pointer);stdcall;
    ClearAccum: procedure(red, green, blue, alpha: TGLfloat); stdcall;
    ClearIndex: procedure(c: TGLfloat); stdcall;
    ClipPlane: procedure(plane: Cardinal; equation: PGLdouble);stdcall;
    Color3b: procedure(red, green, blue: TGLbyte); stdcall;
    Color3bv: procedure(v: PGLbyte); stdcall;
    Color3d: procedure(red, green, blue: TGLdouble); stdcall;
    Color3dv: procedure(v: PGLdouble); stdcall;
    Color3f: procedure(red, green, blue: TGLfloat); stdcall;
    Color3fv: procedure(v: PGLfloat); stdcall;
    Color3i: procedure(red, green, blue: TGLint); stdcall;
    Color3iv: procedure(v: PGLint); stdcall;
    Color3s: procedure(red, green, blue: TGLshort); stdcall;
    Color3sv: procedure(v: PGLshort); stdcall;
    Color3ub: procedure(red, green, blue: TGLubyte); stdcall;
    Color3ubv: procedure(v: PGLubyte); stdcall;
    Color3ui: procedure(red, green, blue: Cardinal); stdcall;
    Color3uiv: procedure(v: PGLuint); stdcall;
    Color3us: procedure(red, green, blue: TGLushort); stdcall;
    Color3usv: procedure(v: PGLushort); stdcall;
    Color4b: procedure(red, green, blue, alpha: TGLbyte); stdcall;
    Color4bv: procedure(v: PGLbyte); stdcall;
    Color4d: procedure(red, green, blue, alpha: TGLdouble); stdcall;
    Color4dv: procedure(v: PGLdouble); stdcall;
    Color4f: procedure(red, green, blue, alpha: TGLfloat); stdcall;
    Color4fv: procedure(v: PGLfloat); stdcall;
    Color4i: procedure(red, green, blue, alpha: TGLint); stdcall;
    Color4iv: procedure(v: PGLint); stdcall;
    Color4s: procedure(red, green, blue, alpha: TGLshort); stdcall;
    Color4sv: procedure(v: TGLshort); stdcall;
    Color4ub: procedure(red, green, blue, alpha: TGLubyte); stdcall;
    Color4ubv: procedure(v: PGLubyte); stdcall;
    Color4ui: procedure(red, green, blue, alpha: Cardinal); stdcall;
    Color4uiv: procedure(v: PGLuint); stdcall;
    Color4us: procedure(red, green, blue, alpha: TGLushort); stdcall;
    Color4usv: procedure(v: PGLushort); stdcall;
    ColorMaterial: procedure(face: Cardinal; mode: Cardinal); stdcall;
    ColorPointer: procedure(size: TGLint; atype: Cardinal; stride: TGLsizei;
      Data: Pointer);stdcall;
    CopyPixels: procedure(X, y: TGLint; Width, Height: TGLsizei;
      atype: Cardinal);stdcall;
    DeleteLists: procedure(list: Cardinal; range: TGLsizei); stdcall;
    DisableClientState: procedure(aarray: Cardinal); stdcall;
    DrawPixels: procedure(Width, Height: TGLsizei; format, atype: Cardinal;
      pixels: Pointer);stdcall;
    EdgeFlag: procedure(flag: TGLboolean); stdcall;
    EdgeFlagPointer: procedure(stride: TGLsizei; Data: Pointer);    stdcall;
    EdgeFlagv: procedure(flag: PGLboolean); stdcall;
    EnableClientState: procedure(aarray: Cardinal); stdcall;
    End_: procedure(); stdcall;
    EndList: procedure(); stdcall;
    EvalCoord1d: procedure(u: TGLdouble); stdcall;
    EvalCoord1dv: procedure(u: PGLdouble); stdcall;
    EvalCoord1f: procedure(u: TGLfloat); stdcall;
    EvalCoord1fv: procedure(u: PGLfloat); stdcall;
    EvalCoord2d: procedure(u: TGLdouble; v: TGLdouble);stdcall;
    EvalCoord2dv: procedure(u: PGLdouble);stdcall;
    EvalCoord2f: procedure(u, v: TGLfloat);stdcall;
    EvalCoord2fv: procedure(u: PGLfloat);stdcall;
    EvalMesh1: procedure(mode: Cardinal; i1, i2: TGLint);stdcall;
    EvalMesh2: procedure(mode: Cardinal; i1, i2, j1, j2: TGLint);stdcall;
    EvalPoint1: procedure(i: TGLint);stdcall;
    EvalPoint2: procedure(i, j: TGLint);stdcall;
    FeedbackBuffer: procedure(size: TGLsizei; atype: Cardinal;
      buffer: PGLfloat);stdcall;
    Fogf: procedure(pname: Cardinal; param: TGLfloat);stdcall;
    Fogfv: procedure(pname: Cardinal; params: PGLfloat);stdcall;
    Fogi: procedure(pname: Cardinal; param: TGLint);stdcall;
    Fogiv: procedure(pname: Cardinal; params: PGLint);stdcall;
    Frustum: procedure(left, right, bottom, top, zNear, zFar: TGLdouble);stdcall;
    GenLists: function(range: TGLsizei): Cardinal;stdcall;
    GetClipPlane: procedure(plane: Cardinal; equation: PGLdouble);stdcall;
    GetLightfv: procedure(light, pname: Cardinal; params: PGLfloat);stdcall;
    GetLightiv: procedure(light, pname: Cardinal; params: PGLint);stdcall;
    GetMapdv: procedure(target, query: Cardinal; v: PGLdouble);stdcall;
    GetMapfv: procedure(target, query: Cardinal; v: PGLfloat);stdcall;
    GetMapiv: procedure(target, query: Cardinal; v: PGLint);stdcall;
    GetMaterialfv: procedure(face, pname: Cardinal; params: PGLfloat);stdcall;
    GetMaterialiv: procedure(face, pname: Cardinal; params: PGLint);stdcall;
    GetPixelMapfv: procedure(map: Cardinal; values: PGLfloat);stdcall;
    GetPixelMapuiv: procedure(map: Cardinal; values: PGLuint);stdcall;
    GetPixelMapusv: procedure(map: Cardinal; values: PGLushort);stdcall;
    GetPolygonStipple: procedure(mask: PGLubyte);stdcall;
    GetTexEnvfv: procedure(target, pname: Cardinal; params: PGLfloat);stdcall;
    GetTexEnviv: procedure(target, pname: Cardinal; params: PGLint);stdcall;
    GetTexGendv: procedure(coord, pname: Cardinal; params: PGLdouble);stdcall;
    GetTexGenfv: procedure(coord, pname: Cardinal; params: PGLfloat);stdcall;
    GetTexGeniv: procedure(coord, pname: Cardinal; params: PGLint);stdcall;
    IndexMask: procedure(mask: Cardinal);stdcall;
    IndexPointer: procedure(atype: Cardinal; stride: TGLsizei; Data: Pointer);stdcall;
    Indexd: procedure(c: TGLdouble);stdcall;
    Indexdv: procedure(c: PGLdouble);stdcall;
    Indexf: procedure(c: TGLfloat);stdcall;
    Indexfv: procedure(c: PGLfloat);stdcall;
    Indexi: procedure(c: TGLint);stdcall;
    Indexiv: procedure(c: PGLint);stdcall;
    Indexs: procedure(c: TGLshort);stdcall;
    Indexsv: procedure(c: PGLshort);stdcall;
    Indexub: procedure(c: TGLubyte);stdcall;
    Indexubv: procedure(c: PGLubyte);stdcall;
    InitNames: procedure();stdcall;
    InterleavedArrays: procedure(format: Cardinal; stride: TGLsizei;
      Data: Pointer);stdcall;
    IsList: function(list: Cardinal): TGLboolean;stdcall;
    LightModelf: procedure(pname: Cardinal; param: TGLfloat);stdcall;
    LightModelfv: procedure(pname: Cardinal; params: PGLfloat);stdcall;
    LightModeli: procedure(pname: Cardinal; param: TGLint);stdcall;
    LightModeliv: procedure(pname: Cardinal; params: PGLint);stdcall;
    Lightf: procedure(light, pname: Cardinal; param: TGLfloat);stdcall;
    Lightfv: procedure(light, pname: Cardinal; params: PGLfloat);stdcall;
    Lighti: procedure(light, pname: Cardinal; param: TGLint);stdcall;
    Lightiv: procedure(light, pname: Cardinal; params: PGLint);stdcall;
    LineStipple: procedure(factor: TGLint; pattern: TGLushort);stdcall;
    ListBase: procedure(base: Cardinal);stdcall;
    LoadIdentity: procedure();stdcall;
    LoadMatrixd: procedure(m: PGLdouble);stdcall;
    LoadMatrixf: procedure(m: PGLfloat);stdcall;
    LoadName: procedure(Name: Cardinal);stdcall;
    Map1d: procedure(target: Cardinal; u1, u2: TGLdouble; stride, order: TGLint;
      points: PGLdouble);stdcall;
    Map1f: procedure(target: Cardinal; u1, u2: TGLfloat; stride, order: TGLint;
      points: PGLfloat);stdcall;
    Map2d: procedure(target: Cardinal; u1, u2: TGLdouble;
      ustride, uorder: TGLint; v1, v2: TGLdouble; vstride, vorder: TGLint;
      points: PGLdouble);stdcall;
    Map2f: procedure(target: Cardinal; u1, u2: TGLfloat;
      ustride, uorder: TGLint; v1, v2: TGLfloat; vstride, vorder: TGLint;
      points: PGLfloat);stdcall;
    MapGrid1d: procedure(un: TGLint; u1, u2: TGLdouble);stdcall;
    MapGrid1f: procedure(un: TGLint; u1, u2: TGLfloat);stdcall;
    MapGrid2d: procedure(un: TGLint; u1, u2: TGLdouble; vn: TGLint;
      v1, v2: TGLdouble);stdcall;
    MapGrid2f: procedure(un: TGLint; u1, u2: TGLfloat; vn: TGLint;
      v1, v2: TGLfloat);stdcall;
    Materialf: procedure(face, pname: Cardinal; param: TGLfloat);stdcall;
    Materialfv: procedure(face, pname: Cardinal; params: PGLfloat);stdcall;
    Materiali: procedure(face, pname: Cardinal; param: TGLint);stdcall;
    Materialiv: procedure(face, pname: Cardinal; params: PGLint);stdcall;
    MatrixMode: procedure(mode: Cardinal);stdcall;
    MultMatrixd: procedure(m: PGLdouble);stdcall;
    MultMatrixf: procedure(m: PGLfloat);stdcall;
    NewList: procedure(list: Cardinal; mode: Cardinal);stdcall;
    Normal3b: procedure(nx, ny, nz: TGLbyte);stdcall;
    Normal3bv: procedure(v: PGLbyte);stdcall;
    Normal3d: procedure(nx, ny, nz: TGLdouble);stdcall;
    Normal3dv: procedure(v: PGLdouble);stdcall;
    Normal3f: procedure(nx, ny, nz: TGLfloat);stdcall;
    Normal3fv: procedure(v: PGLfloat);stdcall;
    Normal3i: procedure(nx, ny, nz: TGLint);stdcall;
    Normal3iv: procedure(v: PGLint);stdcall;
    Normal3s: procedure(nx, ny, nz: TGLshort);stdcall;
    Normal3sv: procedure(v: PGLshort);stdcall;
    NormalPointer: procedure(atype: Cardinal; stride: TGLsizei; Data: Pointer);stdcall;
    Ortho: procedure(left, right, bottom, top, zNear, zFar: TGLdouble);stdcall;
    PassThrough: procedure(token: TGLfloat);stdcall;
    PixelMapfv: procedure(map: Cardinal; mapsize: TGLsizei; values: PGLfloat);stdcall;
    PixelMapuiv: procedure(map: Cardinal; mapsize: TGLsizei; values: PGLuint);stdcall;
    PixelMapusv: procedure(map: Cardinal; mapsize: TGLsizei; values: PGLushort);stdcall;
    PixelTransferf: procedure(pname: Cardinal; param: TGLfloat);stdcall;
    PixelTransferi: procedure(pname: Cardinal; param: TGLint);stdcall;
    PixelZoom: procedure(xfactor, yfactor: TGLfloat);stdcall;
    PolygonStipple: procedure(mask: PGLubyte);stdcall;
    PopAttrib: procedure();stdcall;
    PopClientAttrib: procedure();stdcall;
    PopMatrix: procedure();stdcall;
    PopName: procedure();stdcall;
    PrioritizeTextures: procedure(n: TGLsizei; textures: PGLuint;
      priorities: PSingle);stdcall;
    PushAttrib: procedure(mask: TGLbitfield);stdcall;
    PushClientAttrib: procedure(mask: TGLbitfield);stdcall;
    PushMatrix: procedure();stdcall;
    PushName: procedure(Name: Cardinal);stdcall;
    RasterPos2d: procedure(X, y: TGLdouble);stdcall;
    RasterPos2dv: procedure(v: PGLdouble);stdcall;
    RasterPos2f: procedure(X, y: TGLfloat);stdcall;
    RasterPos2fv: procedure(v: PGLfloat);stdcall;
    RasterPos2i: procedure(X, y: TGLint);stdcall;
    RasterPos2iv: procedure(v: PGLint);stdcall;
    RasterPos2s: procedure(X, y: PGLshort);stdcall;
    RasterPos2sv: procedure(v: PGLshort);stdcall;
    RasterPos3d: procedure(X, y, z: TGLdouble);stdcall;
    RasterPos3dv: procedure(v: PGLdouble);stdcall;
    RasterPos3f: procedure(X, y, z: TGLfloat);stdcall;
    RasterPos3fv: procedure(v: PGLfloat);stdcall;
    RasterPos3i: procedure(X, y, z: TGLint);stdcall;
    RasterPos3iv: procedure(v: PGLint);stdcall;
    RasterPos3s: procedure(X, y, z: TGLshort);stdcall;
    RasterPos3sv: procedure(v: PGLshort);stdcall;
    RasterPos4d: procedure(X, y, z, w: TGLdouble);stdcall;
    RasterPos4dv: procedure(v: PGLdouble);stdcall;
    RasterPos4f: procedure(X, y, z, w: TGLfloat);stdcall;
    RasterPos4fv: procedure(v: PGLfloat);stdcall;
    RasterPos4i: procedure(X, y, z, w: TGLint);stdcall;
    RasterPos4iv: procedure(v: PGLint);stdcall;
    RasterPos4s: procedure(X, y, z, w: TGLshort);stdcall;
    RasterPos4sv: procedure(v: PGLshort);stdcall;
    Rectd: procedure(x1, y1, x2, y2: TGLdouble);stdcall;
    Rectdv: procedure(v1, v2: PGLdouble);stdcall;
    Rectf: procedure(x1, y1, x2, y2: TGLfloat);stdcall;
    Rectfv: procedure(v1, v2: PGLfloat);stdcall;
    Recti: procedure(x1, y1, x2, y2: TGLint);stdcall;
    Rectiv: procedure(v1, v2: PGLint);stdcall;
    Rects: procedure(x1, y1, x2, y2: TGLshort);stdcall;
    Rectsv: procedure(v1, v2: PGLshort);stdcall;
    RenderMode: function(mode: Cardinal): TGLint;stdcall;
    Rotated: procedure(ane, X, y, z: TGLdouble);stdcall;
    Rotatef: procedure(ane, X, y, z: TGLfloat);stdcall;
    Scaled: procedure(X, y, z: TGLdouble);stdcall;
    Scalef: procedure(X, y, z: TGLfloat);stdcall;
    SelectBuffer: procedure(size: TGLsizei; buffer: PGLuint);stdcall;
    ShadeModel: procedure(mode: Cardinal);stdcall;
    TexCoord1d: procedure(s: TGLdouble);stdcall;
    TexCoord1dv: procedure(v: PGLdouble);stdcall;
    TexCoord1f: procedure(s: TGLfloat);stdcall;
    TexCoord1fv: procedure(v: PGLfloat);stdcall;
    TexCoord1i: procedure(s: TGLint);stdcall;
    TexCoord1iv: procedure(v: PGLint);stdcall;
    TexCoord1s: procedure(s: TGLshort);stdcall;
    TexCoord1sv: procedure(v: PGLshort);stdcall;
    TexCoord2d: procedure(s, t: TGLdouble);stdcall;
    TexCoord2dv: procedure(v: PGLdouble);stdcall;
    TexCoord2f: procedure(s, t: TGLfloat);stdcall;
    TexCoord2fv: procedure(v: PGLfloat);stdcall;
    TexCoord2i: procedure(s, t: TGLint);stdcall;
    TexCoord2iv: procedure(v: PGLint);stdcall;
    TexCoord2s: procedure(s, t: TGLshort);stdcall;
    TexCoord2sv: procedure(v: PGLshort);stdcall;
    TexCoord3d: procedure(s, t, r: TGLdouble);stdcall;
    TexCoord3dv: procedure(v: PGLdouble);stdcall;
    TexCoord3f: procedure(s, t, r: TGLfloat);stdcall;
    TexCoord3fv: procedure(v: PGLfloat);stdcall;
    TexCoord3i: procedure(s, t, r: TGLint);stdcall;
    TexCoord3iv: procedure(v: PGLint);stdcall;
    TexCoord3s: procedure(s, t, r: TGLshort);stdcall;
    TexCoord3sv: procedure(v: PGLshort);stdcall;
    TexCoord4d: procedure(s, t, r, q: TGLdouble);stdcall;
    TexCoord4dv: procedure(v: PGLdouble);stdcall;
    TexCoord4f: procedure(s, t, r, q: TGLfloat);stdcall;
    TexCoord4fv: procedure(v: PGLfloat);stdcall;
    TexCoord4i: procedure(s, t, r, q: TGLint);stdcall;
    TexCoord4iv: procedure(v: PGLint);stdcall;
    TexCoord4s: procedure(s, t, r, q: TGLshort);stdcall;
    TexCoord4sv: procedure(v: PGLshort);stdcall;
    TexCoordPointer: procedure(size: TGLint; atype: Cardinal; stride: TGLsizei; Data: Pointer);stdcall;
    TexEnvf: procedure(target, pname: Cardinal; param: TGLfloat);stdcall;
    TexEnvfv: procedure(target, pname: Cardinal; params: PGLfloat);stdcall;
    TexEnvi: procedure(target, pname: Cardinal; param: TGLint);stdcall;
    TexEnviv: procedure(target, pname: Cardinal; params: PGLint);stdcall;
    TexGend: procedure(coord, pname: Cardinal; param: TGLdouble);stdcall;
    TexGendv: procedure(coord, pname: Cardinal; params: PGLdouble);stdcall;
    TexGenf: procedure(coord, pname: Cardinal; param: TGLfloat);stdcall;
    TexGenfv: procedure(coord, pname: Cardinal; params: PGLfloat);stdcall;
    TexGeni: procedure(coord, pname: Cardinal; param: TGLint);stdcall;
    TexGeniv: procedure(coord, pname: Cardinal; params: PGLint);stdcall;
    Translated: procedure(X, y, z: TGLdouble);stdcall;
    Translatef: procedure(X, y, z: TGLfloat);stdcall;
    Vertex2d: procedure(X, y: TGLdouble);stdcall;
    Vertex2dv: procedure(v: PGLdouble);stdcall;
    Vertex2f: procedure(X, y: TGLfloat);stdcall;
    Vertex2fv: procedure(v: PGLfloat);stdcall;
    Vertex2i: procedure(X, y: TGLint);stdcall;
    Vertex2iv: procedure(v: PGLint);stdcall;
    Vertex2s: procedure(X, y: TGLshort);stdcall;
    Vertex2sv: procedure(v: PGLshort);stdcall;
    Vertex3d: procedure(X, y, z: TGLdouble);stdcall;
    Vertex3dv: procedure(v: PGLdouble);stdcall;
    Vertex3f: procedure(X, y, z: TGLfloat);stdcall;
    Vertex3fv: procedure(v: PGLfloat);stdcall;
    Vertex3i: procedure(X, y, z: TGLint);stdcall;
    Vertex3iv: procedure(v: PGLint);stdcall;
    Vertex3s: procedure(X, y, z: TGLshort);stdcall;
    Vertex3sv: procedure(v: PGLshort);stdcall;
    Vertex4d: procedure(X, y, z, w: TGLdouble);stdcall;
    Vertex4dv: procedure(v: PGLdouble);stdcall;
    Vertex4f: procedure(X, y, z, w: TGLfloat);stdcall;
    Vertex4fv: procedure(v: PGLfloat);stdcall;
    Vertex4i: procedure(X, y, z, w: TGLint);stdcall;
    Vertex4iv: procedure(v: PGLint);stdcall;
    Vertex4s: procedure(X, y, z, w: TGLshort);stdcall;
    Vertex4sv: procedure(v: PGLshort);stdcall;
    VertexPointer: procedure(size: TGLint; atype: Cardinal; stride: TGLsizei;
      Data: Pointer);stdcall;

    //*****************************************************************
    // --------- New core function/procedure definitions in OpenGL 1.2'}
    //*****************************************************************

    BlendColor: PFNGLBLENDCOLORPROC;
    BlendEquation: PFNGLBLENDEQUATIONPROC;
    DrawRangeElements: PFNGLDRAWRANGEELEMENTSPROC;
    TexImage3D: PFNGLTEXIMAGE3DPROC;
    TexSubImage3D: PFNGLTEXSUBIMAGE3DPROC;
    CopyTexSubImage3D: PFNGLCOPYTEXSUBIMAGE3DPROC;

    // -------- New core function/procedure definitions in OpenGL 1.4'}
    BlendFuncSeparate: PFNGLBLENDFUNCSEPARATEPROC;
    MultiDrawArrays: PFNGLMULTIDRAWARRAYSPROC;
    MultiDrawElements: PFNGLMULTIDRAWELEMENTSPROC;
    PointParameterf: PFNGLPOINTPARAMETERFPROC;
    PointParameterfv: PFNGLPOINTPARAMETERFVPROC;
    PointParameteri: PFNGLPOINTPARAMETERIPROC;
    PointParameteriv: PFNGLPOINTPARAMETERIVPROC;

    // ------------- New core function/procedure definitions in OpenGL 2.0'}
    BlendEquationSeparate: PFNGLBLENDEQUATIONSEPARATEPROC;
    DrawBuffers: PFNGLDRAWBUFFERSPROC;
    StencilOpSeparate: PFNGLSTENCILOPSEPARATEPROC;
    StencilFuncSeparate: PFNGLSTENCILFUNCSEPARATEPROC;
    StencilMaskSeparate: PFNGLSTENCILMASKSEPARATEPROC;

    // ---------------- Buffer objects
    LockArrays: PFNGLLOCKARRAYSEXTPROC; // EXT only
    UnlockArrays: PFNGLUNLOCKARRAYSEXTPROC; // EXT only
    BindBuffer: PFNGLBINDBUFFERPROC;
    DeleteBuffers: PFNGLDELETEBUFFERSPROC;
    GenBuffers: PFNGLGENBUFFERSPROC;
    IsBuffer: PFNGLISBUFFERPROC;
    BufferData: PFNGLBUFFERDATAPROC;
    BufferSubData: PFNGLBUFFERSUBDATAPROC;
    GetBufferSubData: PFNGLGETBUFFERSUBDATAPROC;
    MapBuffer: PFNGLMAPBUFFERPROC;
    UnmapBuffer: PFNGLUNMAPBUFFERPROC;
    GetBufferParameteriv: PFNGLGETBUFFERPARAMETERIVPROC;
    GetBufferPointerv: PFNGLGETBUFFERPOINTERVPROC;
    MapBufferRange: PFNGLMAPBUFFERRANGEPROC;
    FlushMappedBufferRange: PFNGLFLUSHMAPPEDBUFFERRANGEPROC;
    BindBufferRange: PFNGLBINDBUFFERRANGEPROC;
    BindBufferOffset: PFNGLBINDBUFFEROFFSETEXTPROC; // EXT + NV only
    BindBufferBase: PFNGLBINDBUFFERBASEPROC;
    TransformFeedbackAttribs: PFNGLTRANSFORMFEEDBACKATTRIBSNVPROC; // NV only
    TransformFeedbackVaryingsNV: PFNGLTRANSFORMFEEDBACKVARYINGSNVPROC;
    // NV only
    TransformFeedbackVaryings: PFNGLTRANSFORMFEEDBACKVARYINGSPROC;
    GetTransformFeedbackVarying: PFNGLGETTRANSFORMFEEDBACKVARYINGPROC;
    BeginTransformFeedback: PFNGLBEGINTRANSFORMFEEDBACKPROC;
    EndTransformFeedback: PFNGLENDTRANSFORMFEEDBACKPROC;
    TexBuffer: PFNGLTEXBUFFERPROC;
    ClearBufferiv: PFNGLCLEARBUFFERIVPROC;
    ClearBufferuiv: PFNGLCLEARBUFFERUIVPROC;
    ClearBufferfv: PFNGLCLEARBUFFERFVPROC;
    ClearBufferfi: PFNGLCLEARBUFFERFIPROC;
    GetStringi: PFNGLGETSTRINGIPROC;
    BindVertexArray: PFNGLBINDVERTEXARRAYPROC;
    DeleteVertexArrays: PFNGLDELETEVERTEXARRAYSPROC;
    GenVertexArrays: PFNGLGENVERTEXARRAYSPROC;
    IsVertexArray: PFNGLISVERTEXARRAYPROC;
    FlushVertexArrayRangeNV: PFNGLFLUSHVERTEXARRAYRANGENVPROC;
    VertexArrayRangeNV: PFNGLVERTEXARRAYRANGENVPROC;
    GetUniformIndices: PFNGLGETUNIFORMINDICESPROC;
    GetActiveUniformsiv: PFNGLGETACTIVEUNIFORMSIVPROC;
    GetActiveUniformName: PFNGLGETACTIVEUNIFORMNAMEPROC;
    GetUniformBlockIndex: PFNGLGETUNIFORMBLOCKINDEXPROC;
    GetActiveUniformBlockiv: PFNGLGETACTIVEUNIFORMBLOCKIVPROC;
    GetActiveUniformBlockName: PFNGLGETACTIVEUNIFORMBLOCKNAMEPROC;
    UniformBlockBinding: PFNGLUNIFORMBLOCKBINDINGPROC;
    CopyBufferSubData: PFNGLCOPYBUFFERSUBDATAPROC;
    UniformBuffer: PFNGLUNIFORMBUFFEREXTPROC;
    GetUniformBufferSize: PFNGLGETUNIFORMBUFFERSIZEEXTPROC; // EXT only
    GetUniformOffset: PFNGLGETUNIFORMOFFSETEXTPROC; // EXT only
    PrimitiveRestartIndex: PFNGLPRIMITIVERESTARTINDEXPROC;
    DrawElementsBaseVertex: PFNGLDRAWELEMENTSBASEVERTEXPROC;
    DrawRangeElementsBaseVertex: PFNGLDRAWRANGEELEMENTSBASEVERTEXPROC;
    DrawElementsInstancedBaseVertex: PFNGLDRAWELEMENTSINSTANCEDBASEVERTEXPROC;
    MultiDrawElementsBaseVertex: PFNGLMULTIDRAWELEMENTSBASEVERTEXPROC;
    DrawArraysInstanced: PFNGLDRAWARRAYSINSTANCEDPROC;
    DrawElementsInstanced: PFNGLDRAWELEMENTSINSTANCEDPROC;
    VertexAttrib1d: PFNGLVERTEXATTRIB1DPROC;
    VertexAttrib1dv: PFNGLVERTEXATTRIB1DVPROC;
    VertexAttrib1f: PFNGLVERTEXATTRIB1FPROC;
    VertexAttrib1fv: PFNGLVERTEXATTRIB1FVPROC;
    VertexAttrib1s: PFNGLVERTEXATTRIB1SPROC;
    VertexAttrib1sv: PFNGLVERTEXATTRIB1SVPROC;
    VertexAttrib2d: PFNGLVERTEXATTRIB2DPROC;
    VertexAttrib2dv: PFNGLVERTEXATTRIB2DVPROC;
    VertexAttrib2f: PFNGLVERTEXATTRIB2FPROC;
    VertexAttrib2fv: PFNGLVERTEXATTRIB2FVPROC;
    VertexAttrib2s: PFNGLVERTEXATTRIB2SPROC;
    VertexAttrib2sv: PFNGLVERTEXATTRIB2SVPROC;
    VertexAttrib3d: PFNGLVERTEXATTRIB3DPROC;
    VertexAttrib3dv: PFNGLVERTEXATTRIB3DVPROC;
    VertexAttrib3f: PFNGLVERTEXATTRIB3FPROC;
    VertexAttrib3fv: PFNGLVERTEXATTRIB3FVPROC;
    VertexAttrib3s: PFNGLVERTEXATTRIB3SPROC;
    VertexAttrib3sv: PFNGLVERTEXATTRIB3SVPROC;
    VertexAttrib4Nbv: PFNGLVERTEXATTRIB4NBVPROC;
    VertexAttrib4Niv: PFNGLVERTEXATTRIB4NIVPROC;
    VertexAttrib4Nsv: PFNGLVERTEXATTRIB4NSVPROC;
    VertexAttrib4Nub: PFNGLVERTEXATTRIB4NUBPROC;
    VertexAttrib4Nubv: PFNGLVERTEXATTRIB4NUBVPROC;
    VertexAttrib4Nuiv: PFNGLVERTEXATTRIB4NUIVPROC;
    VertexAttrib4Nusv: PFNGLVERTEXATTRIB4NUSVPROC;
    VertexAttrib4bv: PFNGLVERTEXATTRIB4BVPROC;
    VertexAttrib4d: PFNGLVERTEXATTRIB4DPROC;
    VertexAttrib4dv: PFNGLVERTEXATTRIB4DVPROC;
    VertexAttrib4f: PFNGLVERTEXATTRIB4FPROC;
    VertexAttrib4fv: PFNGLVERTEXATTRIB4FVPROC;
    VertexAttrib4iv: PFNGLVERTEXATTRIB4IVPROC;
    VertexAttrib4s: PFNGLVERTEXATTRIB4SPROC;
    VertexAttrib4sv: PFNGLVERTEXATTRIB4SVPROC;
    VertexAttrib4ubv: PFNGLVERTEXATTRIB4UBVPROC;
    VertexAttrib4uiv: PFNGLVERTEXATTRIB4UIVPROC;
    VertexAttrib4usv: PFNGLVERTEXATTRIB4USVPROC;
    VertexAttribPointer: PFNGLVERTEXATTRIBPOINTERPROC;
    VertexAttribI1i: PFNGLVERTEXATTRIBI1IPROC;
    VertexAttribI2i: PFNGLVERTEXATTRIBI2IPROC;
    VertexAttribI3i: PFNGLVERTEXATTRIBI3IPROC;
    VertexAttribI4i: PFNGLVERTEXATTRIBI4IPROC;
    VertexAttribI1ui: PFNGLVERTEXATTRIBI1UIPROC;
    VertexAttribI2ui: PFNGLVERTEXATTRIBI2UIPROC;
    VertexAttribI3ui: PFNGLVERTEXATTRIBI3UIPROC;
    VertexAttribI4ui: PFNGLVERTEXATTRIBI4UIPROC;
    VertexAttribI1iv: PFNGLVERTEXATTRIBI1IVPROC;
    VertexAttribI2iv: PFNGLVERTEXATTRIBI2IVPROC;
    VertexAttribI3iv: PFNGLVERTEXATTRIBI3IVPROC;
    VertexAttribI4iv: PFNGLVERTEXATTRIBI4IVPROC;
    VertexAttribI1uiv: PFNGLVERTEXATTRIBI1UIVPROC;
    VertexAttribI2uiv: PFNGLVERTEXATTRIBI2UIVPROC;
    VertexAttribI3uiv: PFNGLVERTEXATTRIBI3UIVPROC;
    VertexAttribI4uiv: PFNGLVERTEXATTRIBI4UIVPROC;
    VertexAttribI4bv: PFNGLVERTEXATTRIBI4BVPROC;
    VertexAttribI4sv: PFNGLVERTEXATTRIBI4SVPROC;
    VertexAttribI4ubv: PFNGLVERTEXATTRIBI4UBVPROC;
    VertexAttribI4usv: PFNGLVERTEXATTRIBI4USVPROC;
    VertexAttribIPointer: PFNGLVERTEXATTRIBIPOINTERPROC;
    GetVertexAttribIiv: PFNGLGETVERTEXATTRIBIIVPROC;
    GetVertexAttribIuiv: PFNGLGETVERTEXATTRIBIUIVPROC;
    Uniform1ui: PFNGLUNIFORM1UIPROC;
    Uniform2ui: PFNGLUNIFORM2UIPROC;
    Uniform3ui: PFNGLUNIFORM3UIPROC;
    Uniform4ui: PFNGLUNIFORM4UIPROC;
    Uniform1uiv: PFNGLUNIFORM1UIVPROC;
    Uniform2uiv: PFNGLUNIFORM2UIVPROC;
    Uniform3uiv: PFNGLUNIFORM3UIVPROC;
    Uniform4uiv: PFNGLUNIFORM4UIVPROC;
    GetUniformuiv: PFNGLGETUNIFORMUIVPROC;
    BindFragDataLocation: PFNGLBINDFRAGDATALOCATIONPROC;
    GetFragDataLocation: PFNGLGETFRAGDATALOCATIONPROC;
    ClampColor: PFNGLCLAMPCOLORPROC;
    ColorMaski: PFNGLCOLORMASKIPROC;
    GetBooleani_v: PFNGLGETBOOLEANI_VPROC;
    GetIntegeri_v: PFNGLGETINTEGERI_VPROC;
    Enablei: PFNGLENABLEIPROC;
    Disablei: PFNGLDISABLEIPROC;
    IsEnabledi: PFNGLISENABLEDIPROC;
    EnableVertexAttribArray: PFNGLENABLEVERTEXATTRIBARRAYPROC;
    DisableVertexAttribArray: PFNGLDISABLEVERTEXATTRIBARRAYPROC;
    VertexAttribDivisor: PFNGLVERTEXATTRIBDIVISORPROC;
    ClearColorIi: PFNGLCLEARCOLORIIEXTPROC; // EXT only
    ClearColorIui: PFNGLCLEARCOLORIUIEXTPROC; // EXT only
    TexParameterIiv: PFNGLTEXPARAMETERIIVPROC;
    TexParameterIuiv: PFNGLTEXPARAMETERIUIVPROC;
    GetTexParameterIiv: PFNGLGETTEXPARAMETERIIVPROC;
    GetTexParameterIuiv: PFNGLGETTEXPARAMETERIUIVPROC;
    PatchParameteri: PFNGLPATCHPARAMETERIPROC;
    PatchParameterfv: PFNGLPATCHPARAMETERFVPROC;
    BufferAddressRangeNV: PFNGLBUFFERADDRESSRANGENVPROC;
    VertexFormatNV: PFNGLVERTEXFORMATNVPROC;
    NormalFormatNV: PFNGLNORMALFORMATNVPROC;
    ColorFormatNV: PFNGLCOLORFORMATNVPROC;
    IndexFormatNV: PFNGLINDEXFORMATNVPROC;
    TexCoordFormatNV: PFNGLTEXCOORDFORMATNVPROC;
    EdgeFlagFormatNV: PFNGLEDGEFLAGFORMATNVPROC;
    SecondaryColorFormatNV: PFNGLSECONDARYCOLORFORMATNVPROC;
    FogCoordFormatNV: PFNGLFOGCOORDFORMATNVPROC;
    VertexAttribFormatNV: PFNGLVERTEXATTRIBFORMATNVPROC;
    VertexAttribIFormatNV: PFNGLVERTEXATTRIBIFORMATNVPROC;
    GetIntegerui64i_vNV: PFNGLGETINTEGERUI64I_VNVPROC;
    GetBufferParameterui64vNV: PGNGLGETBUFFERPARAMETERUI64VNV;
    MakeBufferResidentNV: PFNGLMAKEBUFFERRESIDENTNVPROC;
    MakeBufferNonResidentNV: PFNGLMAKEBUFFERNONRESIDENTNVPROC;
    IsBufferResidentNV: PFNGLISBUFFERRESIDENTNVPROC;
    MakeNamedBufferResidentNV: PFNGLMAKENAMEDBUFFERRESIDENTNVPROC;
    MakeNamedBufferNonResidentNV: PFNGLMAKENAMEDBUFFERNONRESIDENTNVPROC;
    IsNamedBufferResidentNV: PFNGLISNAMEDBUFFERRESIDENTNVPROC;
    GetNamedBufferParameterui64vNV: PFNGLGETNAMEDBUFFERPARAMETERUI64VNVPROC;
    GetIntegerui64vNV: PFNGLGETINTEGERUI64VNVPROC;
    Uniformui64NV: PFNGLUNIFORMUI64NVPROC;
    Uniformui64vNV: PFNGLUNIFORMUI64VNVPROC;
    GetUniformui64vNV: PFNGLGETUNIFORMUI64VNVPROC;
    ProgramUniformui64NV: PFNGLPROGRAMUNIFORMUI64NVPROC;
    ProgramUniformui64vNV: PFNGLPROGRAMUNIFORMUI64VNVPROC;
    ClearBufferData: PFNGLClearBufferData;
    ClearBufferSubData: PFNGLClearBufferSubData;
    ClearNamedBufferData: PFNGLClearNamedBufferData;
    ClearNamedBufferSubData: PFNGLClearNamedBufferSubData;
    InvalidateTexSubImage: PFNGLInvalidateTexSubImage;
    InvalidateTexImage: PFNGLInvalidateTexImage;
    InvalidateBufferSubData: PFNGLInvalidateBufferSubData;
    InvalidateBufferData: PFNGLInvalidateBufferData;
    InvalidateFramebuffer: PFNGLInvalidateFramebuffer;
    InvalidateSubFramebuffer: PFNGLInvalidateSubFramebuffer;
    MultiDrawArraysIndirect: PFNGLMultiDrawArraysIndirect;
    MultiDrawElementsIndirect: PFNGLMultiDrawElementsIndirect;
    BindVertexBuffer: PFNGLBindVertexBuffer;
    VertexAttribFormat: PFNGLVertexAttribFormat;
    VertexAttribIFormat: PFNGLVertexAttribIFormat;
    VertexAttribLFormat: PFNGLVertexAttribLFormat;
    VertexAttribBinding: PFNGLVertexAttribBinding;
    VertexBindingDivisor: PFNGLVertexBindingDivisor;
    VertexArrayBindVertexBuffer: PFNGLVertexArrayBindVertexBuffer;
    VertexArrayVertexAttribFormat: PFNGLVertexArrayVertexAttribFormat;
    VertexArrayVertexAttribIFormat: PFNGLVertexArrayVertexAttribIFormat;
    VertexArrayVertexAttribLFormat: PFNGLVertexArrayVertexAttribLFormat;
    VertexArrayVertexAttribBinding: PFNGLVertexArrayVertexAttribBinding;
    VertexArrayVertexBindingDivisor: PFNGLVertexArrayVertexBindingDivisor;

    // ------------------------------ Shader object
    DeleteObject: PFNGLDELETEOBJECTARBPROC; // ARB only
    GetHandle: PFNGLGETHANDLEARBPROC; // ARB only
    DetachShader: PFNGLDETACHSHADERPROC;
    CreateShader: PFNGLCREATESHADERPROC;
    DeleteShader: PFNGLDELETESHADERPROC;
    ShaderSource: PFNGLSHADERSOURCEPROC;
    CompileShader: PFNGLCOMPILESHADERPROC;
    CreateProgram: PFNGLCREATEPROGRAMPROC;
    DeleteProgram: PFNGLDELETEPROGRAMPROC;
    AttachShader: PFNGLATTACHSHADERPROC;
    LinkProgram: PFNGLLINKPROGRAMPROC;
    UseProgram: PFNGLUSEPROGRAMPROC;
    ValidateProgram: PFNGLVALIDATEPROGRAMPROC;
    Uniform1f: PFNGLUNIFORM1FPROC;
    Uniform2f: PFNGLUNIFORM2FPROC;
    Uniform3f: PFNGLUNIFORM3FPROC;
    Uniform4f: PFNGLUNIFORM4FPROC;
    Uniform1i: PFNGLUNIFORM1IPROC;
    Uniform2i: PFNGLUNIFORM2IPROC;
    Uniform3i: PFNGLUNIFORM3IPROC;
    Uniform4i: PFNGLUNIFORM4IPROC;
    Uniform1fv: PFNGLUNIFORM1FVPROC;
    Uniform2fv: PFNGLUNIFORM2FVPROC;
    Uniform3fv: PFNGLUNIFORM3FVPROC;
    Uniform4fv: PFNGLUNIFORM4FVPROC;
    Uniform1iv: PFNGLUNIFORM1IVPROC;
    Uniform2iv: PFNGLUNIFORM2IVPROC;
    Uniform3iv: PFNGLUNIFORM3IVPROC;
    Uniform4iv: PFNGLUNIFORM4IVPROC;
    UniformMatrix2fv: PFNGLUNIFORMMATRIX2FVPROC;
    UniformMatrix3fv: PFNGLUNIFORMMATRIX3FVPROC;
    UniformMatrix4fv: PFNGLUNIFORMMATRIX4FVPROC;
    GetObjectParameterfv: PFNGLGETOBJECTPARAMETERFVARBPROC; // ARB only
    GetObjectParameteriv: PFNGLGETOBJECTPARAMETERIVARBPROC; // ARB only
    GetInfoLog: PFNGLGETINFOLOGARBPROC; // ARB only
    GetAttachedObjects: PFNGLGETATTACHEDOBJECTSARBPROC; // ARB only
    GetActiveAttrib: PFNGLGETACTIVEATTRIBPROC;
    GetActiveUniform: PFNGLGETACTIVEUNIFORMPROC;
    GetAttachedShaders: PFNGLGETATTACHEDSHADERSPROC;
    GetAttribLocation: PFNGLGETATTRIBLOCATIONPROC;
    GetProgramiv: PFNGLGETPROGRAMIVPROC;
    GetProgramInfoLog: PFNGLGETPROGRAMINFOLOGPROC;
    GetShaderiv: PFNGLGETSHADERIVPROC;
    GetShaderInfoLog: PFNGLGETSHADERINFOLOGPROC;
    GetShaderSource: PFNGLGETSHADERSOURCEPROC;
    GetUniformLocation: PFNGLGETUNIFORMLOCATIONPROC;
    GetUniformfv: PFNGLGETUNIFORMFVPROC;
    GetUniformiv: PFNGLGETUNIFORMIVPROC;
    GetVertexAttribdv: PFNGLGETVERTEXATTRIBDVPROC;
    GetVertexAttribfv: PFNGLGETVERTEXATTRIBFVPROC;
    GetVertexAttribiv: PFNGLGETVERTEXATTRIBIVPROC;
    GetVertexAttribPointerv: PFNGLGETVERTEXATTRIBPOINTERVPROC;
    IsProgram: PFNGLISPROGRAMPROC;
    IsShader: PFNGLISSHADERPROC;
    BindAttribLocation: PFNGLBINDATTRIBLOCATIONPROC;
    BindFragDataLocationIndexed: PFNGLBINDFRAGDATALOCATIONINDEXEDPROC;
    GetFragDataIndex: PFNGLGETFRAGDATAINDEXPROC;
    GetVaryingLocation: PFNGLGETVARYINGLOCATIONNVPROC; // NV only
    GetActiveVarying: PFNGLGETACTIVEVARYINGNVPROC; // NV only
    ActiveVarying: PFNGLACTIVEVARYINGNVPROC; // NV only
    GetProgramBinary: PFNGLGETPROGRAMBINARYPROC;
    ProgramBinary: PFNGLPROGRAMBINARYPROC;
    UseProgramStages: PFNGLUSEPROGRAMSTAGESPROC;
    ActiveShaderProgram: PFNGLACTIVESHADERPROGRAMPROC;
    CreateShaderProgramv: PFNGLCREATESHADERPROGRAMVPROC;
    BindProgramPipeline: PFNGLBINDPROGRAMPIPELINEPROC;
    DeleteProgramPipelines: PFNGLDELETEPROGRAMPIPELINESPROC;
    GenProgramPipelines: PFNGLGENPROGRAMPIPELINESPROC;
    IsProgramPipeline: PFNGLISPROGRAMPIPELINEPROC;
    GetProgramPipelineiv: PFNGLGETPROGRAMPIPELINEIVPROC;
    ProgramUniform1i: PFNGLPROGRAMUNIFORM1IPROC;
    ProgramUniform1iv: PFNGLPROGRAMUNIFORM1IVPROC;
    ProgramUniform1f: PFNGLPROGRAMUNIFORM1FPROC;
    ProgramUniform1fv: PFNGLPROGRAMUNIFORM1FVPROC;
    ProgramUniform1d: PFNGLPROGRAMUNIFORM1DPROC;
    ProgramUniform1dv: PFNGLPROGRAMUNIFORM1DVPROC;
    ProgramUniform1ui: PFNGLPROGRAMUNIFORM1UIPROC;
    ProgramUniform1uiv: PFNGLPROGRAMUNIFORM1UIVPROC;
    ProgramUniform2i: PFNGLPROGRAMUNIFORM2IPROC;
    ProgramUniform2iv: PFNGLPROGRAMUNIFORM2IVPROC;
    ProgramUniform2f: PFNGLPROGRAMUNIFORM2FPROC;
    ProgramUniform2fv: PFNGLPROGRAMUNIFORM2FVPROC;
    ProgramUniform2d: PFNGLPROGRAMUNIFORM2DPROC;
    ProgramUniform2dv: PFNGLPROGRAMUNIFORM2DVPROC;
    ProgramUniform2ui: PFNGLPROGRAMUNIFORM2UIPROC;
    ProgramUniform2uiv: PFNGLPROGRAMUNIFORM2UIVPROC;
    ProgramUniform3i: PFNGLPROGRAMUNIFORM3IPROC;
    ProgramUniform3iv: PFNGLPROGRAMUNIFORM3IVPROC;
    ProgramUniform3f: PFNGLPROGRAMUNIFORM3FPROC;
    ProgramUniform3fv: PFNGLPROGRAMUNIFORM3FVPROC;
    ProgramUniform3d: PFNGLPROGRAMUNIFORM3DPROC;
    ProgramUniform3dv: PFNGLPROGRAMUNIFORM3DVPROC;
    ProgramUniform3ui: PFNGLPROGRAMUNIFORM3UIPROC;
    ProgramUniform3uiv: PFNGLPROGRAMUNIFORM3UIVPROC;
    ProgramUniform4i: PFNGLPROGRAMUNIFORM4IPROC;
    ProgramUniform4iv: PFNGLPROGRAMUNIFORM4IVPROC;
    ProgramUniform4f: PFNGLPROGRAMUNIFORM4FPROC;
    ProgramUniform4fv: PFNGLPROGRAMUNIFORM4FVPROC;
    ProgramUniform4d: PFNGLPROGRAMUNIFORM4DPROC;
    ProgramUniform4dv: PFNGLPROGRAMUNIFORM4DVPROC;
    ProgramUniform4ui: PFNGLPROGRAMUNIFORM4UIPROC;
    ProgramUniform4uiv: PFNGLPROGRAMUNIFORM4UIVPROC;
    ProgramUniformMatrix2fv: PFNGLPROGRAMUNIFORMMATRIX2FVPROC;
    ProgramUniformMatrix3fv: PFNGLPROGRAMUNIFORMMATRIX3FVPROC;
    ProgramUniformMatrix4fv: PFNGLPROGRAMUNIFORMMATRIX4FVPROC;
    ProgramUniformMatrix2dv: PFNGLPROGRAMUNIFORMMATRIX2DVPROC;
    ProgramUniformMatrix3dv: PFNGLPROGRAMUNIFORMMATRIX3DVPROC;
    ProgramUniformMatrix4dv: PFNGLPROGRAMUNIFORMMATRIX4DVPROC;
    ProgramUniformMatrix2x3fv: PFNGLPROGRAMUNIFORMMATRIX2X3FVPROC;
    ProgramUniformMatrix3x2fv: PFNGLPROGRAMUNIFORMMATRIX3X2FVPROC;
    ProgramUniformMatrix2x4fv: PFNGLPROGRAMUNIFORMMATRIX2X4FVPROC;
    ProgramUniformMatrix4x2fv: PFNGLPROGRAMUNIFORMMATRIX4X2FVPROC;
    ProgramUniformMatrix3x4fv: PFNGLPROGRAMUNIFORMMATRIX3X4FVPROC;
    ProgramUniformMatrix4x3fv: PFNGLPROGRAMUNIFORMMATRIX4X3FVPROC;
    ProgramUniformMatrix2x3dv: PFNGLPROGRAMUNIFORMMATRIX2X3DVPROC;
    ProgramUniformMatrix3x2dv: PFNGLPROGRAMUNIFORMMATRIX3X2DVPROC;
    ProgramUniformMatrix2x4dv: PFNGLPROGRAMUNIFORMMATRIX2X4DVPROC;
    ProgramUniformMatrix4x2dv: PFNGLPROGRAMUNIFORMMATRIX4X2DVPROC;
    ProgramUniformMatrix3x4dv: PFNGLPROGRAMUNIFORMMATRIX3X4DVPROC;
    ProgramUniformMatrix4x3dv: PFNGLPROGRAMUNIFORMMATRIX4X3DVPROC;
    ValidateProgramPipeline: PFNGLVALIDATEPROGRAMPIPELINEPROC;
    GetProgramPipelineInfoLog: PFNGLGETPROGRAMPIPELINEINFOLOGPROC;
    DispatchCompute: PFNGLDispatchCompute;
    DispatchComputeIndirect: PFNGLDispatchComputeIndirect;
    ShaderStorageBlockBinding: PFNGLShaderStorageBlockBinding;

    // ------------- Framebuffer object
    IsRenderbuffer: PFNGLISRENDERBUFFERPROC;
    BindRenderbuffer: PFNGLBINDRENDERBUFFERPROC;
    DeleteRenderbuffers: PFNGLDELETERENDERBUFFERSPROC;
    GenRenderbuffers: PFNGLGENRENDERBUFFERSPROC;
    RenderbufferStorage: PFNGLRENDERBUFFERSTORAGEPROC;
    RenderbufferStorageMultisample: PFNGLRENDERBUFFERSTORAGEMULTISAMPLEPROC;
    GetRenderbufferParameteriv: PFNGLGETRENDERBUFFERPARAMETERIVPROC;
    IsFramebuffer: PFNGLISFRAMEBUFFERPROC;
    BindFramebuffer: PFNGLBINDFRAMEBUFFERPROC;
    DeleteFramebuffers: PFNGLDELETEFRAMEBUFFERSPROC;
    GenFramebuffers: PFNGLGENFRAMEBUFFERSPROC;
    CheckFramebufferStatus: PFNGLCHECKFRAMEBUFFERSTATUSPROC;
    FramebufferTexture: PFNGLFRAMEBUFFERTEXTUREPROC;
    FramebufferTexture1D: PFNGLFRAMEBUFFERTEXTURE1DPROC;
    FramebufferTexture2D: PFNGLFRAMEBUFFERTEXTURE2DPROC;
    FramebufferTexture3D: PFNGLFRAMEBUFFERTEXTURE3DPROC;
    FramebufferTextureLayer: PFNGLFRAMEBUFFERTEXTURELAYERPROC;
    FramebufferTextureFace: PFNGLFRAMEBUFFERTEXTUREFACEARBPROC; // ARB only
    FramebufferRenderbuffer: PFNGLFRAMEBUFFERRENDERBUFFERPROC;
    GetFramebufferAttachmentParameteriv: PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVPROC;
    BlitFramebuffer: PFNGLBLITFRAMEBUFFERPROC;
    GenerateMipmap: PFNGLGENERATEMIPMAPPROC;
    FramebufferParameteri: PFNGLFramebufferParameteri;
    NamedFramebufferParameteri: PFNGLNamedFramebufferParameteri;
    GetNamedFramebufferParameteriv: PFNGLGetNamedFramebufferParameteriv;

    // ------------------------------ Queries object
    GenQueries: PFNGLGENQUERIESPROC;
    DeleteQueries: PFNGLDELETEQUERIESPROC;
    IsQuery: PFNGLISQUERYPROC;
    BeginQuery: PFNGLBEGINQUERYPROC;
    EndQuery: PFNGLENDQUERYPROC;
    GetQueryiv: PFNGLGETQUERYIVPROC;
    GetQueryObjectiv: PFNGLGETQUERYOBJECTIVPROC;
    GetQueryObjectuiv: PFNGLGETQUERYOBJECTUIVPROC;
    QueryCounter: PFNGLQUERYCOUNTERPROC;
    GetQueryObjecti64v: PFNGLGETQUERYOBJECTI64VPROC;
    GetQueryObjectui64v: PFNGLGETQUERYOBJECTUI64VPROC;
    GetInternalformati64v: PFNGLGetInternalformati64v;
    GetProgramInterfaceiv: PFNGLGetProgramInterfaceiv;
    GetProgramResourceIndex: PFNGLGetProgramResourceIndex;
    GetProgramResourceName: PFNGLGetProgramResourceName;
    GetProgramResourceiv: PFNGLGetProgramResourceiv;
    GetProgramResourceLocation: PFNGLGetProgramResourceLocation;
    GetProgramResourceLocationIndex: PFNGLGetProgramResourceLocationIndex;

    // ------------------------------ Texture & Sampler object
    // promoted to core v1.3 from GL_ARB_multitexture (#1)
    ActiveTexture: PFNGLACTIVETEXTUREPROC;
    SampleCoverage: PFNGLSAMPLECOVERAGEPROC;
    // promoted to core v1.3 from GL_ARB_texture_compression (#12)
    CompressedTexImage3D: PFNGLCOMPRESSEDTEXIMAGE3DPROC;
    CompressedTexImage2D: PFNGLCOMPRESSEDTEXIMAGE2DPROC;
    CompressedTexImage1D: PFNGLCOMPRESSEDTEXIMAGE1DPROC;
    CompressedTexSubImage3D: PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC;
    CompressedTexSubImage2D: PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC;
    CompressedTexSubImage1D: PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC;
    GetCompressedTexImage: PFNGLGETCOMPRESSEDTEXIMAGEPROC;
    ClientActiveTexture: PFNGLCLIENTACTIVETEXTUREPROC;
    MultiTexCoord1d: PFNGLMULTITEXCOORD1DPROC;
    MultiTexCoord1dV: PFNGLMULTITEXCOORD1DVPROC;
    MultiTexCoord1f: PFNGLMULTITEXCOORD1FPROC;
    MultiTexCoord1fv: PFNGLMULTITEXCOORD1FVPROC;
    MultiTexCoord1i: PFNGLMULTITEXCOORD1IPROC;
    MultiTexCoord1iv: PFNGLMULTITEXCOORD1IVPROC;
    MultiTexCoord1s: PFNGLMULTITEXCOORD1SPROC;
    MultiTexCoord1sv: PFNGLMULTITEXCOORD1SVPROC;
    MultiTexCoord2d: PFNGLMULTITEXCOORD2DPROC;
    MultiTexCoord2dv: PFNGLMULTITEXCOORD2DVPROC;
    MultiTexCoord2f: PFNGLMULTITEXCOORD2FPROC;
    MultiTexCoord2fv: PFNGLMULTITEXCOORD2FVPROC;
    MultiTexCoord2i: PFNGLMULTITEXCOORD2IPROC;
    MultiTexCoord2iv: PFNGLMULTITEXCOORD2IVPROC;
    MultiTexCoord2s: PFNGLMULTITEXCOORD2SPROC;
    MultiTexCoord2sv: PFNGLMULTITEXCOORD2SVPROC;
    MultiTexCoord3d: PFNGLMULTITEXCOORD3DPROC;
    MultiTexCoord3dv: PFNGLMULTITEXCOORD3DVPROC;
    MultiTexCoord3f: PFNGLMULTITEXCOORD3FPROC;
    MultiTexCoord3fv: PFNGLMULTITEXCOORD3FVPROC;
    MultiTexCoord3i: PFNGLMULTITEXCOORD3IPROC;
    MultiTexCoord3iv: PFNGLMULTITEXCOORD3IVPROC;
    MultiTexCoord3s: PFNGLMULTITEXCOORD3SPROC;
    MultiTexCoord3sv: PFNGLMULTITEXCOORD3SVPROC;
    MultiTexCoord4d: PFNGLMULTITEXCOORD4DPROC;
    MultiTexCoord4dv: PFNGLMULTITEXCOORD4DVPROC;
    MultiTexCoord4f: PFNGLMULTITEXCOORD4FPROC;
    MultiTexCoord4fv: PFNGLMULTITEXCOORD4FVPROC;
    MultiTexCoord4i: PFNGLMULTITEXCOORD4IPROC;
    MultiTexCoord4iv: PFNGLMULTITEXCOORD4IVPROC;
    MultiTexCoord4s: PFNGLMULTITEXCOORD4SPROC;
    MultiTexCoord4sv: PFNGLMULTITEXCOORD4SVPROC;
    GetInteger64i_v: PFNGLGETINTEGER64I_VPROC;
    GetBufferParameteri64v: PFNGLGETBUFFERPARAMETERI64VPROC;
    ProgramParameteri: PFNGLPROGRAMPARAMETERIPROC;
    ProgramString: PFNGLPROGRAMSTRINGARBPROC; // ARB only
    BindProgram: PFNGLBINDPROGRAMARBPROC; // ARB + NV only
    DeletePrograms: PFNGLDELETEPROGRAMSARBPROC; // ARB + NV only
    GenPrograms: PFNGLGENPROGRAMSARBPROC; // ARB + NV only
    ProgramEnvParameter4d: PFNGLPROGRAMENVPARAMETER4DARBPROC; // ARB only
    ProgramEnvParameter4dv: PFNGLPROGRAMENVPARAMETER4DVARBPROC; // ARB only
    ProgramEnvParameter4f: PFNGLPROGRAMENVPARAMETER4FARBPROC; // ARB only
    ProgramEnvParameter4fv: PFNGLPROGRAMENVPARAMETER4FVARBPROC; // ARB only
    ProgramLocalParameter4d: PFNGLPROGRAMLOCALPARAMETER4DARBPROC; // ARB only
    ProgramLocalParameter4dv: PFNGLPROGRAMLOCALPARAMETER4DVARBPROC; // ARB only
    ProgramLocalParameter4f: PFNGLPROGRAMLOCALPARAMETER4FARBPROC; // ARB only
    ProgramLocalParameter4fv: PFNGLPROGRAMLOCALPARAMETER4FVARBPROC; // ARB only
    GetProgramEnvParameterdv: PFNGLGETPROGRAMENVPARAMETERDVARBPROC; // ARB only
    GetProgramEnvParameterfv: PFNGLGETPROGRAMENVPARAMETERFVARBPROC; // ARB only
    GetProgramLocalParameterdv: PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC;
    // ARB only
    GetProgramLocalParameterfv: PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC;
    // ARB only
    TexImage2DMultisample: PFNGLTEXIMAGE2DMULTISAMPLEPROC;
    TexImage3DMultisample: PFNGLTEXIMAGE3DMULTISAMPLEPROC;
    GetMultisamplefv: PFNGLGETMULTISAMPLEFVPROC;
    SampleMaski: PFNGLSAMPLEMASKIPROC;
    ProvokingVertex: PFNGLPROVOKINGVERTEXPROC;
    FenceSync: PFNGLFENCESYNCPROC;
    IsSync: PFNGLISSYNCPROC;
    DeleteSync: PFNGLDELETESYNCPROC;
    ClientWaitSync: PFNGLCLIENTWAITSYNCPROC;
    WaitSync: PFNGLWAITSYNCPROC;
    GetInteger64v: PFNGLGETINTEGER64VPROC;
    GetSynciv: PFNGLGETSYNCIVPROC;
    BlendEquationi: PFNGLBLENDEQUATIONIPROC;
    BlendEquationSeparatei: PFNGLBLENDEQUATIONSEPARATEIPROC;
    BlendFunci: PFNGLBLENDFUNCIPROC;
    BlendFuncSeparatei: PFNGLBLENDFUNCSEPARATEIPROC;
    MinSampleShading: PFNGLMINSAMPLESHADINGPROC;
    GenSamplers: PFNGLGENSAMPLERSPROC;
    DeleteSamplers: PFNGLDELETESAMPLERSPROC;
    IsSampler: PFNGLISSAMPLERPROC;
    BindSampler: PFNGLBINDSAMPLERPROC;
    SamplerParameteri: PFNGLSAMPLERPARAMETERIPROC;
    SamplerParameteriv: PFNGLSAMPLERPARAMETERIVPROC;
    SamplerParameterf: PFNGLSAMPLERPARAMETERFPROC;
    SamplerParameterfv: PFNGLSAMPLERPARAMETERFVPROC;
    SamplerParameterIiv: PFNGLSAMPLERPARAMETERIIVPROC;
    SamplerParameterIuiv: PFNGLSAMPLERPARAMETERIUIVPROC;
    GetSamplerParameteriv: PFNGLGETSAMPLERPARAMETERIVPROC;
    GetSamplerParameterIiv: PFNGLGETSAMPLERPARAMETERIIVPROC;
    GetSamplerParameterfv: PFNGLGETSAMPLERPARAMETERFVPROC;
    GetSamplerParameterIfv: PFNGLGETSAMPLERPARAMETERIFVPROC;
    CopyImageSubData: PFNGLCopyImageSubData;
    TexBufferRange: PFNGLTexBufferRange;
    TextureBufferRange: PFNGLTextureBufferRange;
    TexStorage2DMultisample: PFNGLTexStorage2DMultisample;
    TexStorage3DMultisample: PFNGLTexStorage3DMultisample;
    TextureStorage2DMultisample: PFNGLTextureStorage2DMultisample;
    TextureStorage3DMultisample: PFNGLTextureStorage3DMultisample;
    BufferStorage: PFNGLBufferStorage;
    ClearTexImage: PFNGLClearTexImage;
    ClearTexSubImage: PFNGLClearTexSubImage;
    BindBuffersBase: PFNGLBindBuffersBase;
    BindBuffersRange: PFNGLBindBuffersRange;
    BindTextures: PFNGLBindTextures;
    BindSamplers: PFNGLBindSamplers;
    BindImageTextures: PFNGLBindImageTextures;
    BindVertexBuffers: PFNGLBindVertexBuffers;
    TextureView: PFNGLTextureView;

    // ------------------------------ Direct access
    ClientAttribDefault: PFNGLCLIENTATTRIBDEFAULTEXTPROC;
    PushClientAttribDefault: PFNGLPUSHCLIENTATTRIBDEFAULTEXTPROC;
    MatrixLoadf: PFNGLMATRIXLOADFEXTPROC;
    MatrixLoadd: PFNGLMATRIXLOADDEXTPROC;
    MatrixMultf: PFNGLMATRIXMULTFEXTPROC;
    MatrixMultd: PFNGLMATRIXMULTDEXTPROC;
    MatrixLoadIdentity: PFNGLMATRIXLOADIDENTITYEXTPROC;
    MatrixRotatef: PFNGLMATRIXROTATEFEXTPROC;
    MatrixRotated: PFNGLMATRIXROTATEDEXTPROC;
    MatrixScalef: PFNGLMATRIXSCALEFEXTPROC;
    MatrixScaled: PFNGLMATRIXSCALEDEXTPROC;
    MatrixTranslatef: PFNGLMATRIXTRANSLATEFEXTPROC;
    MatrixTranslated: PFNGLMATRIXTRANSLATEDEXTPROC;
    MatrixFrustum: PFNGLMATRIXFRUSTUMEXTPROC;
    MatrixOrtho: PFNGLMATRIXORTHOEXTPROC;
    MatrixPop: PFNGLMATRIXPOPEXTPROC;
    MatrixPush: PFNGLMATRIXPUSHEXTPROC;
    MatrixLoadTransposef: PFNGLMATRIXLOADTRANSPOSEFEXTPROC;
    MatrixLoadTransposed: PFNGLMATRIXLOADTRANSPOSEDEXTPROC;
    MatrixMultTransposef: PFNGLMATRIXMULTTRANSPOSEFEXTPROC;
    MatrixMultTransposed: PFNGLMATRIXMULTTRANSPOSEDEXTPROC;
    TextureParameterfv: PFNGLTEXTUREPARAMETERFVEXTPROC;
    TextureParameteri: PFNGLTEXTUREPARAMETERIEXTPROC;
    TextureParameteriv: PFNGLTEXTUREPARAMETERIVEXTPROC;
    TextureImage1D: PFNGLTEXTUREIMAGE1DEXTPROC;
    TextureImage2D: PFNGLTEXTUREIMAGE2DEXTPROC;
    TextureSubImage1D: PFNGLTEXTURESUBIMAGE1DEXTPROC;
    TextureSubImage2D: PFNGLTEXTURESUBIMAGE2DEXTPROC;
    CopyTextureImage1D: PFNGLCOPYTEXTUREIMAGE1DEXTPROC;
    CopyTextureImage2D: PFNGLCOPYTEXTUREIMAGE2DEXTPROC;
    CopyTextureSubImage1D: PFNGLCOPYTEXTURESUBIMAGE1DEXTPROC;
    CopyTextureSubImage2D: PFNGLCOPYTEXTURESUBIMAGE2DEXTPROC;
    GetTextureImage: PFNGLGETTEXTUREIMAGEEXTPROC;
    GetTextureParameterfv: PFNGLGETTEXTUREPARAMETERFVEXTPROC;
    GetTextureParameteriv: PFNGLGETTEXTUREPARAMETERIVEXTPROC;
    GetTextureLevelParameterfv: PFNGLGETTEXTURELEVELPARAMETERFVEXTPROC;
    GetTextureLevelParameteriv: PFNGLGETTEXTURELEVELPARAMETERIVEXTPROC;
    TextureImage3D: PFNGLTEXTUREIMAGE3DEXTPROC;
    TextureSubImage3D: PFNGLTEXTURESUBIMAGE3DEXTPROC;
    CopyTextureSubImage3D: PFNGLCOPYTEXTURESUBIMAGE3DEXTPROC;
    MultiTexParameterf: PFNGLMULTITEXPARAMETERFEXTPROC;
    MultiTexParameterfv: PFNGLMULTITEXPARAMETERFVEXTPROC;
    MultiTexParameteri: PFNGLMULTITEXPARAMETERIEXTPROC;
    MultiTexParameteriv: PFNGLMULTITEXPARAMETERIVEXTPROC;
    MultiTexImage1D: PFNGLMULTITEXIMAGE1DEXTPROC;
    MultiTexImage2D: PFNGLMULTITEXIMAGE2DEXTPROC;
    MultiTexSubImage1D: PFNGLMULTITEXSUBIMAGE1DEXTPROC;
    MultiTexSubImage2D: PFNGLMULTITEXSUBIMAGE2DEXTPROC;
    CopyMultiTexImage1D: PFNGLCOPYMULTITEXIMAGE1DEXTPROC;
    CopyMultiTexImage2D: PFNGLCOPYMULTITEXIMAGE2DEXTPROC;
    CopyMultiTexSubImage1D: PFNGLCOPYMULTITEXSUBIMAGE1DEXTPROC;
    CopyMultiTexSubImage2D: PFNGLCOPYMULTITEXSUBIMAGE2DEXTPROC;
    GetMultiTexImage: PFNGLGETMULTITEXIMAGEEXTPROC;
    GetMultiTexParameterfv: PFNGLGETMULTITEXPARAMETERFVEXTPROC;
    GetMultiTexParameteriv: PFNGLGETMULTITEXPARAMETERIVEXTPROC;
    GetMultiTexLevelParameterfv: PFNGLGETMULTITEXLEVELPARAMETERFVEXTPROC;
    GetMultiTexLevelParameteriv: PFNGLGETMULTITEXLEVELPARAMETERIVEXTPROC;
    MultiTexImage3D: PFNGLMULTITEXIMAGE3DEXTPROC;
    MultiTexSubImage3D: PFNGLMULTITEXSUBIMAGE3DEXTPROC;
    CopyMultiTexSubImage3D: PFNGLCOPYMULTITEXSUBIMAGE3DEXTPROC;
    BindMultiTexture: PFNGLBINDMULTITEXTUREEXTPROC;
    EnableClientStateIndexed: PFNGLENABLECLIENTSTATEINDEXEDEXTPROC;
    DisableClientStateIndexed: PFNGLDISABLECLIENTSTATEINDEXEDEXTPROC;
    MultiTexCoordPointer: PFNGLMULTITEXCOORDPOINTEREXTPROC;
    MultiTexEnvf: PFNGLMULTITEXENVFEXTPROC;
    MultiTexEnvfv: PFNGLMULTITEXENVFVEXTPROC;
    MultiTexEnvi: PFNGLMULTITEXENVIEXTPROC;
    MultiTexEnviv: PFNGLMULTITEXENVIVEXTPROC;
    MultiTexGend: PFNGLMULTITEXGENDEXTPROC;
    MultiTexGendv: PFNGLMULTITEXGENDVEXTPROC;
    MultiTexGenf: PFNGLMULTITEXGENFEXTPROC;
    MultiTexGenfv: PFNGLMULTITEXGENFVEXTPROC;
    MultiTexGeni: PFNGLMULTITEXGENIEXTPROC;
    MultiTexGeniv: PFNGLMULTITEXGENIVEXTPROC;
    GetMultiTexEnvfv: PFNGLGETMULTITEXENVFVEXTPROC;
    GetMultiTexEnviv: PFNGLGETMULTITEXENVIVEXTPROC;
    GetMultiTexGendv: PFNGLGETMULTITEXGENDVEXTPROC;
    GetMultiTexGenfv: PFNGLGETMULTITEXGENFVEXTPROC;
    GetMultiTexGeniv: PFNGLGETMULTITEXGENIVEXTPROC;
    GetFloatIndexedv: PFNGLGETFLOATINDEXEDVEXTPROC;
    GetDoubleIndexedv: PFNGLGETDOUBLEINDEXEDVEXTPROC;
    GetPointerIndexedv: PFNGLGETPOINTERINDEXEDVEXTPROC;
    CompressedTextureImage3D: PFNGLCOMPRESSEDTEXTUREIMAGE3DEXTPROC;
    CompressedTextureImage2D: PFNGLCOMPRESSEDTEXTUREIMAGE2DEXTPROC;
    CompressedTextureImage1D: PFNGLCOMPRESSEDTEXTUREIMAGE1DEXTPROC;
    CompressedTextureSubImage3D: PFNGLCOMPRESSEDTEXTURESUBIMAGE3DEXTPROC;
    CompressedTextureSubImage2D: PFNGLCOMPRESSEDTEXTURESUBIMAGE2DEXTPROC;
    CompressedTextureSubImage1D: PFNGLCOMPRESSEDTEXTURESUBIMAGE1DEXTPROC;
    GetCompressedTextureImage: PFNGLGETCOMPRESSEDTEXTUREIMAGEEXTPROC;
    CompressedMultiTexImage3D: PFNGLCOMPRESSEDMULTITEXIMAGE3DEXTPROC;
    CompressedMultiTexImage2D: PFNGLCOMPRESSEDMULTITEXIMAGE2DEXTPROC;
    CompressedMultiTexImage1D: PFNGLCOMPRESSEDMULTITEXIMAGE1DEXTPROC;
    CompressedMultiTexSubImage3D: PFNGLCOMPRESSEDMULTITEXSUBIMAGE3DEXTPROC;
    CompressedMultiTexSubImage2D: PFNGLCOMPRESSEDMULTITEXSUBIMAGE2DEXTPROC;
    CompressedMultiTexSubImage1D: PFNGLCOMPRESSEDMULTITEXSUBIMAGE1DEXTPROC;
    GetCompressedMultiTexImage: PFNGLGETCOMPRESSEDMULTITEXIMAGEEXTPROC;
    NamedProgramString: PFNGLNAMEDPROGRAMSTRINGEXTPROC;
    NamedProgramLocalParameter4d: PFNGLNAMEDPROGRAMLOCALPARAMETER4DEXTPROC;
    NamedProgramLocalParameter4dv: PFNGLNAMEDPROGRAMLOCALPARAMETER4DVEXTPROC;
    NamedProgramLocalParameter4f: PFNGLNAMEDPROGRAMLOCALPARAMETER4FEXTPROC;
    NamedProgramLocalParameter4fv: PFNGLNAMEDPROGRAMLOCALPARAMETER4FVEXTPROC;
    GetNamedProgramLocalParameterdv: PFNGLGETNAMEDPROGRAMLOCALPARAMETERDVEXTPROC;
    GetNamedProgramLocalParameterfv: PFNGLGETNAMEDPROGRAMLOCALPARAMETERFVEXTPROC;
    GetNamedProgramiv: PFNGLGETNAMEDPROGRAMIVEXTPROC;
    GetNamedProgramString: PFNGLGETNAMEDPROGRAMSTRINGEXTPROC;
    NamedProgramLocalParameters4fv: PFNGLNAMEDPROGRAMLOCALPARAMETERS4FVEXTPROC;
    NamedProgramLocalParameterI4i: PFNGLNAMEDPROGRAMLOCALPARAMETERI4IEXTPROC;
    NamedProgramLocalParameterI4iv: PFNGLNAMEDPROGRAMLOCALPARAMETERI4IVEXTPROC;
    NamedProgramLocalParametersI4iv: PFNGLNAMEDPROGRAMLOCALPARAMETERSI4IVEXTPROC;
    NamedProgramLocalParameterI4ui: PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIEXTPROC;
    NamedProgramLocalParameterI4uiv: PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIVEXTPROC;
    NamedProgramLocalParametersI4uiv: PFNGLNAMEDPROGRAMLOCALPARAMETERSI4UIVEXTPROC;
    GetNamedProgramLocalParameterIiv: PFNGLGETNAMEDPROGRAMLOCALPARAMETERIIVEXTPROC;
    GetNamedProgramLocalParameterIuiv: PFNGLGETNAMEDPROGRAMLOCALPARAMETERIUIVEXTPROC;
    TextureParameterIiv: PFNGLTEXTUREPARAMETERIIVEXTPROC;
    TextureParameterIuiv: PFNGLTEXTUREPARAMETERIUIVEXTPROC;
    GetTextureParameterIiv: PFNGLGETTEXTUREPARAMETERIIVEXTPROC;
    GetTextureParameterIuiv: PFNGLGETTEXTUREPARAMETERIUIVEXTPROC;
    MultiTexParameterIiv: PFNGLMULTITEXPARAMETERIIVEXTPROC;
    MultiTexParameterIuiv: PFNGLMULTITEXPARAMETERIUIVEXTPROC;
    GetMultiTexParameterIiv: PFNGLGETMULTITEXPARAMETERIIVEXTPROC;
    GetMultiTexParameterIuiv: PFNGLGETMULTITEXPARAMETERIUIVEXTPROC;
    NamedBufferData: PFNGLNAMEDBUFFERDATAEXTPROC;
    NamedBufferSubData: PFNGLNAMEDBUFFERSUBDATAEXTPROC;
    MapNamedBuffer: PFNGLMAPNAMEDBUFFEREXTPROC;
    UnmapNamedBuffer: PFNGLUNMAPNAMEDBUFFEREXTPROC;
    MapNamedBufferRange: PFNGLMAPNAMEDBUFFERRANGEEXTPROC;
    FlushMappedNamedBufferRange: PFNGLFLUSHMAPPEDNAMEDBUFFERRANGEEXTPROC;
    NamedCopyBufferSubData: PFNGLNAMEDCOPYBUFFERSUBDATAEXTPROC;
    GetNamedBufferParameteriv: PFNGLGETNAMEDBUFFERPARAMETERIVEXTPROC;
    GetNamedBufferPointerv: PFNGLGETNAMEDBUFFERPOINTERVEXTPROC;
    GetNamedBufferSubData: PFNGLGETNAMEDBUFFERSUBDATAEXTPROC;
    TextureBuffer: PFNGLTEXTUREBUFFEREXTPROC;
    MultiTexBuffer: PFNGLMULTITEXBUFFEREXTPROC;
    NamedRenderbufferStorage: PFNGLNAMEDRENDERBUFFERSTORAGEEXTPROC;
    GetNamedRenderbufferParameteriv: PFNGLGETNAMEDRENDERBUFFERPARAMETERIVEXTPROC;
    CheckNamedFramebufferStatus: PFNGLCHECKNAMEDFRAMEBUFFERSTATUSEXTPROC;
    NamedFramebufferTexture1D: PFNGLNAMEDFRAMEBUFFERTEXTURE1DEXTPROC;
    NamedFramebufferTexture2D: PFNGLNAMEDFRAMEBUFFERTEXTURE2DEXTPROC;
    NamedFramebufferTexture3D: PFNGLNAMEDFRAMEBUFFERTEXTURE3DEXTPROC;
    NamedFramebufferRenderbuffer: PFNGLNAMEDFRAMEBUFFERRENDERBUFFEREXTPROC;
    GetNamedFramebufferAttachmentParameteriv: PFNGLGETNAMEDFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC;
    GenerateTextureMipmap: PFNGLGENERATETEXTUREMIPMAPEXTPROC;
    GenerateMultiTexMipmap: PFNGLGENERATEMULTITEXMIPMAPEXTPROC;
    FramebufferDrawBuffer: PFNGLFRAMEBUFFERDRAWBUFFEREXTPROC;
    FramebufferDrawBuffers: PFNGLFRAMEBUFFERDRAWBUFFERSEXTPROC;
    FramebufferReadBuffer: PFNGLFRAMEBUFFERREADBUFFEREXTPROC;
    GetFramebufferParameteriv: PFNGLGETFRAMEBUFFERPARAMETERIVEXTPROC;
    NamedRenderbufferStorageMultisample: PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC;
    NamedRenderbufferStorageMultisampleCoverage: PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLECOVERAGEEXTPROC;
    NamedFramebufferTexture: PFNGLNAMEDFRAMEBUFFERTEXTUREEXTPROC;
    NamedFramebufferTextureLayer: PFNGLNAMEDFRAMEBUFFERTEXTURELAYEREXTPROC;
    NamedFramebufferTextureFace: PFNGLNAMEDFRAMEBUFFERTEXTUREFACEEXTPROC;
    TextureRenderbuffer: PFNGLTEXTURERENDERBUFFEREXTPROC;
    MultiTexRenderbuffer: PFNGLMULTITEXRENDERBUFFEREXTPROC;

    // ------------------------------ Debugging
    // Special Gremedy debugger extension
    FrameTerminatorGREMEDY: PFNGLFRAMETERMINATORGREMEDYPROC;
    StringMarkerGREMEDY: PFNGLSTRINGMARKERGREMEDYPROC;
    DebugMessageEnableAMDX: procedure(category: Cardinal; severity: Cardinal;
      Count: TGLsizei; var ids: Cardinal; Enabled: boolean);

    PushDebugGroup: PFNGLPushDebugGroup;
    PopDebugGroup: PFNGLPopDebugGroup;
    ObjectLabel: PFNGLObjectLabel;
    GetObjectLabel: PFNGLGetObjectLabel;
    ObjectPtrLabel: PFNGLObjectPtrLabel;
    GetObjectPtrLabel: PFNGLGetObjectPtrLabel;
    DebugMessageCallbackAMDX: procedure(callback: TDebugProcAMD;
      userParam: Pointer);stdcall;
    DebugMessageControl: procedure(type_: Cardinal; Source: Cardinal;
      severity: Cardinal; Count: TGLsizei; var ids: Cardinal; Enabled: boolean);stdcall;
    DebugMessageInsert: procedure(Source: Cardinal; severity: Cardinal;
      id: Cardinal; length: TGLsizei; const buf: PAnsiChar);stdcall;
    DebugMessageCallback: procedure(callback: TDebugProc; userParam: Pointer);stdcall;
    GetDebugMessageLog: function(Count: Cardinal; bufSize: TGLsizei;
      var severity: Cardinal; var severities: Cardinal; var ids: Cardinal;
      var lengths: TGLsizei; messageLog: PAnsiChar): Cardinal;stdcall;
    // ------------------------------ Interrop
    CreateSyncFromCLevent: PFNGLCreateSyncFromCLevent;
    VDPAUInitNV: procedure(const vdpDevice: Pointer;
      const getProcAddress: Pointer); stdcall;
    VDPAUFiniNV: procedure(); stdcall;
    VDPAURegisterVideoSurfaceNV: function(const vdpSurface: Pointer;
      target: Cardinal; numTextureNames: TGLsizei; const textureNames: PGLuint)
      : TGLvdpauSurfaceNV;stdcall;
    VDPAURegisterOutputSurfaceNV: function(const vdpSurface: Pointer;
      target: Cardinal; numTextureNames: TGLsizei; const textureNames: PGLuint)
      : TGLvdpauSurfaceNV;stdcall;
    VDPAUIsSurfaceNV: procedure(surface: TGLvdpauSurfaceNV);stdcall;
    VDPAUUnregisterSurfaceNV: procedure(surface: TGLvdpauSurfaceNV);stdcall;
    VDPAUGetSurfaceivNV: procedure(surface: TGLvdpauSurfaceNV; pname: Cardinal;
      bufSize: TGLsizei; length: PGLsizei; values: PGLint);stdcall;
    VDPAUSurfaceAccessNV: procedure(surface: TGLvdpauSurfaceNV;
      access: Cardinal);stdcall;
    VDPAUMapSurfacesNV: procedure(numSurfaces: TGLsizei;
      const surfaces: PGLvdpauSurfaceNV);stdcall;
    VDPAUUnmapSurfacesNV: procedure(numSurface: TGLsizei;
      const surfaces: PGLvdpauSurfaceNV);stdcall;

    // ------------------------------ Path rendering
    GenPathsNV: PFNGLGENPATHSNVPROC;
    DeletePathsNV: PFNGLDELETEPATHSNVPROC;
    IsPathNV: PFNGLISPATHNVPROC;
    PathCommandsNV: PFNGLPATHCOMMANDSNVPROC;
    PathCoordsNV: PFNGLPATHCOORDSNVPROC;
    PathSubCommandsNV: PFNGLPATHSUBCOMMANDSNVPROC;
    PathSubCoordsNV: PFNGLPATHSUBCOORDSNVPROC;
    PathStringNV: PFNGLPATHSTRINGNVPROC;
    PathGlyphsNV: PFNGLPATHGLYPHSNVPROC;
    PathGlyphRangeNV: PFNGLPATHGLYPHRANGENVPROC;
    WeightPathsNV: PFNGLWEIGHTPATHSNVPROC;
    CopyPathNV: PFNGLCOPYPATHNVPROC;
    InterpolatePathsNV: PFNGLINTERPOLATEPATHSNVPROC;
    PathParameterivNV: PFNGLPATHPARAMETERIVNVPROC;
    PathParameteriNV: PFNGLPATHPARAMETERINVPROC;
    PathParameterfvNV: PFNGLPATHPARAMETERFVNVPROC;
    PathParameterfNV: PFNGLPATHPARAMETERFNVPROC;
    PathDashArrayNV: PFNGLPATHDASHARRAYNVPROC;
    PathStencilFuncNV: PFNGLPATHSTENCILFUNCNVPROC;
    StencilFillPathNV: PFNGLSTENCILFILLPATHNVPROC;
    StencilStrokePathNV: PFNGLSTENCILSTROKEPATHNVPROC;
    StencilFillPathInstancedNV: PFNGLSTENCILFILLPATHINSTANCEDNVPROC;
    StencilStrokePathInstancedNV: PFNGLSTENCILSTROKEPATHINSTANCEDNVPROC;
    PathColorGenNV: PFNGLPATHCOLORGENNVPROC;
    PathTexGenNV: PFNGLPATHTEXGENNVPROC;
    PathFogGenNV: PFNGLPATHFOGGENNVPROC;
    CoverFillPathNV: PFNGLCOVERFILLPATHNVPROC;
    CoverStrokePathNV: PFNGLCOVERSTROKEPATHNVPROC;
    CoverFillPathInstancedNV: PFNGLCOVERFILLPATHINSTANCEDNVPROC;
    CoverStrokePathInstancedNV: PFNGLCOVERSTROKEPATHINSTANCEDNVPROC;
    GetPathParameterivNV: PFNGLGETPATHPARAMETERIVNVPROC;
    GetPathParameterfvNV: PFNGLGETPATHPARAMETERFVNVPROC;
    GetPathCommandsNV: PFNGLGETPATHCOMMANDSNVPROC;
    GetPathCoordsNV: PFNGLGETPATHCOORDSNVPROC;
    GetPathDashArrayNV: PFNGLGETPATHDASHARRAYNVPROC;
    GetPathMetricsNV: PFNGLGETPATHMETRICSNVPROC;
    GetPathMetricRangeNV: PFNGLGETPATHMETRICRANGENVPROC;
    GetPathSpacingNV: PFNGLGETPATHSPACINGNVPROC;
    GetPathColorGenivNV: PFNGLGETPATHCOLORGENIVNVPROC;
    GetPathColorGenfvNV: PFNGLGETPATHCOLORGENFVNVPROC;
    GetPathTexGenivNV: PFNGLGETPATHTEXGENIVNVPROC;
    GetPathTexGenfvNV: PFNGLGETPATHTEXGENFVNVPROC;
    IsPointInFillPathNV: PFNGLISPOINTINFILLPATHNVPROC;
    IsPointInStrokePathNV: PFNGLISPOINTINSTROKEPATHNVPROC;
    GetPathLengthNV: PFNGLGETPATHLENGTHNVPROC;
    PointAlongPathNV: PFNGLPOINTALONGPATHNVPROC;
    PathStencilDepthOffsetNV: PFNGLPATHSTENCILDEPTHOFFSETNVPROC;
    PathCoverDepthFuncNV: PFNGLPATHCOVERDEPTHFUNCNVPROC;

{$IFDEF SUPPORT_WGL}

    // ###########################################################
    // function and procedure definitions for
    // ARB approved WGL extensions
    // ###########################################################

    // ARB approved WGL extension checks
    W_ARB_buffer_region, W_ARB_create_context, W_ARB_create_context_profile,
      W_ARB_extensions_string, W_ARB_framebuffer_sRGB, W_ARB_make_current_read,
      W_ARB_multisample, W_ARB_pbuffer, W_ARB_pixel_format,
      W_ARB_pixel_format_float, W_ARB_render_texture,

    // Vendor/EXT WGL extension checks
    W_ATI_pixel_format_float, W_EXT_framebuffer_sRGB,
      W_EXT_pixel_format_packed_float, W_EXT_swap_control, W_NV_gpu_affinity,
      W_EXT_create_context_es2_profile, W_NV_DX_interop,
      W_NV_DX_interop2: boolean;

    // WGL_buffer_region (ARB #4)
    WCreateBufferRegionARB: PFNWGLCREATEBUFFERREGIONARBPROC;
    WDeleteBufferRegionARB: PFNWGLDELETEBUFFERREGIONARBPROC;
    WSaveBufferRegionARB: PFNWGLSAVEBUFFERREGIONARBPROC;
    WRestoreBufferRegionARB: PFNWGLRESTOREBUFFERREGIONARBPROC;

    // WGL_ARB_extensions_string (ARB #8)
    WGetExtensionsStringARB: PFNWGLGETEXTENSIONSSTRINGARBPROC;

    // WGL_ARB_pixel_format (ARB #9)
    WGetPixelFormatAttribivARB: PFNWGLGETPIXELFORMATATTRIBIVARBPROC;
    WGetPixelFormatAttribfvARB: PFNWGLGETPIXELFORMATATTRIBFVARBPROC;
    WChoosePixelFormatARB: PFNWGLCHOOSEPIXELFORMATARBPROC;

    // WGL_make_current_read (ARB #10)
    WMakeContextCurrentARB: PFNWGLMAKECONTEXTCURRENTARBPROC;
    WGetCurrentReadDCARB: PFNWGLGETCURRENTREADDCARBPROC;

    // WGL_ARB_pbuffer (ARB #11)
    WCreatePbufferARB: PFNWGLCREATEPBUFFERARBPROC;
    WGetPbufferDCARB: PFNWGLGETPBUFFERDCARBPROC;
    WReleasePbufferDCARB: PFNWGLRELEASEPBUFFERDCARBPROC;
    WDestroyPbufferARB: PFNWGLDESTROYPBUFFERARBPROC;
    WQueryPbufferARB: PFNWGLQUERYPBUFFERARBPROC;

    // WGL_ARB_render_texture (ARB #20)
    WBindTexImageARB: PFNWGLBINDTEXIMAGEARBPROC;
    WReleaseTexImageARB: PFNWGLRELEASETEXIMAGEARBPROC;
    WSetPbufferAttribARB: PFNWGLSETPBUFFERATTRIBARBPROC;

    // WGL_ARB_create_context (ARB #55)
    WCreateContextAttribsARB: PFNWGLCREATECONTEXTATTRIBSARBPROC;
    // WGL_NV_gpu_affinity
    WEnumGpusNV: PFNWGLENUMGPUSNVPROC;
    WEnumGpuDevicesNV: PFNWGLENUMGPUDEVICESNVPROC;
    WCreateAffinityDCNV: PFNWGLCREATEAFFINITYDCNVPROC;
    WEnumGpusFromAffinityDCNV: PFNWGLENUMGPUSFROMAFFINITYDCNVPROC;
    WDeleteDCNV: PFNWGLDELETEDCNVPROC;

    // WGL_NV_DX_interop (EXT #407)
    WDXSetResourceShareHandleNV: PFNWGLDXSETRESOURCESHAREHANDLEPROC;
    WDXOpenDeviceNV: PFNWGLDXOPENDEVICEPROC;
    WDXCloseDeviceNV: PFNWGLDXCLOSEDEVICEPROC;
    WDXRegisterObjectNV: PFNWGLDXREGISTEROBJECTPROC;
    WDXUnregisterObjectNV: PFNWGLDXUNREGISTEROBJECTPROC;
    WDXObjectAccessNV: PFNWGLDXOBJECTACCESSPROC;
    WDXLockObjectsNV: PFNWGLDXLOCKOBJECTSPROC;
    WDXUnlockObjectsNV: PFNWGLDXUNLOCKOBJECTSNVPROC;

    // ###########################################################
    // function and procedure definitions for
    // Vendor/EXT WGL extensions
    // ###########################################################

    // WGL_EXT_swap_control (EXT #172)
    WSwapIntervalEXT: PFNWGLSWAPINTERVALEXTPROC;
    WGetSwapIntervalEXT: PFNWGLGETSWAPINTERVALEXTPROC;

    // GL_NV_vertex_array_range (EXT #190)
    WAllocateMemoryNV: PFNWGLALLOCATEMEMORYNVPROC;
    WFreeMemoryNV: PFNWGLFREEMEMORYNVPROC;

{$ENDIF SUPPORT_WGL}


    // ------------------------------ EGL function/procedure

{$IFDEF EGL_SUPPORT}

    OES_depth24, OES_depth32, OES_depth_texture, OES_element_index_uint,
      OES_fbo_render_mipmap, OES_get_program_binary, OES_mapbuffer,
      OES_packed_depth_stencil, OES_rgb8_rgba8, OES_standard_derivatives,
      OES_texture_3D, OES_texture_float, OES_texture_float_linear,
      OES_texture_half_float, OES_texture_half_float_linear, OES_texture_npot,
      OES_vertex_array_object, OES_vertex_half_float: boolean;

    EGetError: function: EGLint;stdcall;
    EGetDisplay: function(display_id: EGLNativeDisplayType): EGLDisplay;stdcall;
    EInitialize: function(dpy: EGLDisplay; major: pEGLint; minor: pEGLint)
      : EGLBoolean;stdcall;
    ETerminate: function(dpy: EGLDisplay): EGLBoolean;stdcall;
    EQueryString: function(dpy: EGLDisplay; name: EGLint): pchar;stdcall;
    EGetConfigs: function(dpy: EGLDisplay; configs: pEGLConfig;
      config_size: EGLint; num_config: pEGLint): EGLBoolean;stdcall;
    EChooseConfig: function(dpy: EGLDisplay; attrib_list: pEGLint;
      configs: pEGLConfig; config_size: EGLint; num_config: pEGLint)
      : EGLBoolean;stdcall;
    EGetConfigAttrib: function(dpy: EGLDisplay; config: EGLConfig;
      attribute: EGLint; Value: pEGLint): EGLBoolean;stdcall;
    ECreateWindowSurface: function(dpy: EGLDisplay; config: EGLConfig;
      win: EGLNativeWindowType; attrib_list: pEGLint): EGLSurface;stdcall;
    ECreatePbufferSurface: function(dpy: EGLDisplay; config: EGLConfig;
      attrib_list: pEGLint): EGLSurface;stdcall;
    ECreatePixmapSurface: function(dpy: EGLDisplay; config: EGLConfig;
      pixmap: EGLNativePixmapType; attrib_list: pEGLint): EGLSurface;stdcall;
    EDestroySurface: function(dpy: EGLDisplay; surface: EGLSurface): EGLBoolean;stdcall;
    EQuerySurface: function(dpy: EGLDisplay; surface: EGLSurface;
      attribute: EGLint; Value: pEGLint): EGLBoolean;stdcall;
    EBindAPI: function(api: EGLenum): EGLBoolean;stdcall;
    EQueryAPI: function: EGLenum;stdcall;
    EWaitClient: function: EGLBoolean;stdcall;
    EReleaseThread: function: EGLBoolean;stdcall;
    ECreatePbufferFromClientBuffer: function(dpy: EGLDisplay; buftype: EGLenum;
      buffer: EGLClientBuffer; config: EGLConfig; attrib_list: pEGLint)
      : EGLSurface;stdcall;
    ESurfaceAttrib: function(dpy: EGLDisplay; surface: EGLSurface;
      attribute: EGLint; Value: EGLint): EGLBoolean;stdcall;    EBindTexImage: function(dpy: EGLDisplay; surface: EGLSurface;
      buffer: EGLint): EGLBoolean;stdcall;
    EReleaseTexImage: function(dpy: EGLDisplay; surface: EGLSurface;
      buffer: EGLint): EGLBoolean;stdcall;
    ESwapInterval: function(dpy: EGLDisplay; interval: EGLint): EGLBoolean;stdcall;
    ECreateContext: function(dpy: EGLDisplay; config: EGLConfig;
      share_context: EGLContext; attrib_list: pEGLint): EGLContext;stdcall;
    EDestroyContext: function(dpy: EGLDisplay; ctx: EGLContext): EGLBoolean;stdcall;
    EMakeCurrent: function(dpy: EGLDisplay; draw: EGLSurface; read: EGLSurface;
      ctx: EGLContext): EGLBoolean;stdcall;
    EGetCurrentContext: function: EGLContext;stdcall;
    EGetCurrentSurface: function(readdraw: EGLint): EGLSurface;stdcall;
    EGetCurrentDisplay: function: EGLDisplay;stdcall;
    EQueryContext: function(dpy: EGLDisplay; ctx: EGLContext; attribute: EGLint;
      Value: pEGLint): EGLBoolean;stdcall;
    EWaitGL: function: EGLBoolean;stdcall;
    EWaitNative: function(engine: EGLint): EGLBoolean;stdcall;
    ESwapBuffers: function(dpy: EGLDisplay; surface: EGLSurface): EGLBoolean;stdcall;
    ECopyBuffers: function(dpy: EGLDisplay; surface: EGLSurface;
      target: EGLNativePixmapType): EGLBoolean;stdcall;

{$ENDIF EGL_SUPPORT}

    // ------------------------------ locate functions/procedures for OpenGL Utility (GLU) extensions'} {$ENDIF}

    // ###########################################################
    // locate functions and procedures for
    // GLU extensions
    // ###########################################################

    UNurbsCallbackDataEXT: procedure(nurb: PGLUnurbs; userData: Pointer);stdcall;
    UNewNurbsTessellatorEXT: function: PGLUnurbs;stdcall;
    UDeleteNurbsTessellatorEXT: procedure(nurb: PGLUnurbs);stdcall;
    constructor Create;
    procedure Initialize(ATemporary: boolean = False);
    procedure Close;
    procedure CheckError;
    procedure ClearError;
    property IsInitialized: boolean read FInitialized;
    property DebugMode: boolean read FDebug write FDebug;
  end; // TGLExtensionsAndEntryPoints

// ------------------------------ Windows OpenGL (WGL) support functions

{$IFDEF MSWINDOWS} // {$IFDEF SUPPORT_WGL}
///function wglGetProcAddress(ProcName: PAnsiChar): Pointer; stdcall; external opengl32;
function wglCopyContext(p1: HGLRC; p2: HGLRC; p3: cardinal): BOOL; stdcall; external opengl32;
function wglCreateContext(DC: HDC): HGLRC; stdcall; external opengl32;
function wglCreateLayerContext(p1: HDC; p2: integer): HGLRC; stdcall; external opengl32;
function wglDeleteContext(p1: HGLRC): BOOL; stdcall; external opengl32;
///function wglDescribeLayerPlane(p1: HDC; p2, p3: integer; p4: cardinal; var p5: TLayerPlaneDescriptor): BOOL; stdcall; external opengl32;
function wglGetCurrentContext: HGLRC; stdcall; external opengl32;
function wglGetCurrentDC: HDC; stdcall; external opengl32;
///function wglGetLayerPaletteEntries(p1: HDC; p2, p3, p4: integer; var pcr): integer; stdcall; external opengl32;
function wglMakeCurrent(DC: HDC; p2: HGLRC): BOOL; stdcall; external opengl32;
function wglRealizeLayerPalette(p1: HDC; p2: integer; p3: BOOL): BOOL; stdcall; external opengl32;
///function wglSetLayerPaletteEntries(p1: HDC; p2, p3, p4: integer; var pcr): integer; stdcall; external opengl32;
function wglShareLists(p1, p2: HGLRC): BOOL; stdcall; external opengl32;
function wglSwapLayerBuffers(p1: HDC; p2: cardinal): BOOL; stdcall; external opengl32;
(*
function wglSwapMultipleBuffers(p1: UINT; const p2: PWGLSwap): DWORD; stdcall; external opengl32;
function wglUseFontBitmapsA(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
function wglUseFontOutlinesA(p1: HDC; p2, p3, p4: DWORD; p5, p6: single;
  p7: integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
function wglUseFontBitmapsW(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
function wglUseFontOutlinesW(p1: HDC; p2, p3, p4: DWORD; p5, p6: single;
  p7: integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
function wglUseFontBitmaps(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32 Name 'wglUseFontBitmapsA';
function wglUseFontOutlines(p1: HDC; p2, p3, p4: DWORD; p5, p6: single;
  p7: integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32 Name 'wglUseFontOutlinesA';
*)
{$ENDIF}


function gluErrorString(errCode: Cardinal): PAnsiChar; stdcall; external glu32;
function gluGetString(Name: Cardinal): PAnsiChar; stdcall; external glu32;
procedure gluOrtho2D(left, right, bottom, top: TGLdouble);
stdcall; external glu32;
procedure gluPerspective(fovy, aspect, zNear, zFar: TGLdouble);stdcall; external glu32;
procedure gluPickMatrix(X, y, Width, Height: TGLdouble; const Viewport: TVector4i);
stdcall; external glu32;
procedure gluLookAt(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: TGLdouble);
stdcall; external glu32;
function gluProject(objx, objy, objz: TGLdouble; const modelMatrix: TMatrix4d;
  const projMatrix: TMatrix4d; const Viewport: TVector4i;
  winx, winy, winz: PGLdouble): TGLint;stdcall; external glu32;
function gluUnProject(winx, winy, winz: TGLdouble; const modelMatrix: TMatrix4d;
  const projMatrix: TMatrix4d; const Viewport: TVector4i;
  objx, objy, objz: PGLdouble): TGLint;stdcall; external glu32;
function gluScaleImage(format: Cardinal; widthin, heightin: TGLint;
  typein: Cardinal; datain: Pointer; widthout, heightout: TGLint;
  typeout: Cardinal; dataout: Pointer): TGLint; stdcall; external glu32;
function gluBuild1DMipmaps(target: Cardinal; Components, Width: TGLint;
  format, atype: Cardinal; Data: Pointer): TGLint; stdcall; external glu32;
function gluBuild2DMipmaps(target: Cardinal; Components, Width, Height: TGLint;
  format, atype: Cardinal; Data: Pointer): TGLint; stdcall; external glu32;
function gluNewQuadric: PGLUquadric; stdcall; external glu32;
procedure gluDeleteQuadric(state: PGLUquadric);stdcall; external glu32;
procedure gluQuadricNormals(quadObject: PGLUquadric; normals: Cardinal);stdcall; external glu32;
procedure gluQuadricTexture(quadObject: PGLUquadric; textureCoords: TGLboolean);
stdcall; external glu32;
procedure gluQuadricOrientation(quadObject: PGLUquadric; orientation: Cardinal);
stdcall; external glu32;
procedure gluQuadricDrawStyle(quadObject: PGLUquadric; drawStyle: Cardinal);
stdcall; external glu32;
procedure gluCylinder(quadObject: PGLUquadric;
  baseRadius, topRadius, Height: TGLdouble; slices, stacks: TGLint);
stdcall; external glu32;
procedure gluDisk(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble;
  slices, loops: TGLint);
stdcall; external glu32;
procedure gluPartialDisk(quadObject: PGLUquadric;
  innerRadius, outerRadius: TGLdouble; slices, loops: TGLint;
  startAngle, sweepAngle: TGLdouble);
stdcall; external glu32;
procedure gluSphere(quadObject: PGLUquadric; radius: TGLdouble;
  slices, stacks: TGLint);
stdcall; external glu32;
procedure gluQuadricCallback(quadObject: PGLUquadric; which: Cardinal;
  fn: TGLUQuadricErrorProc); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl;
{$ENDIF} external glu32;
function gluNewTess: PGLUtesselator; stdcall; external glu32;
procedure gluDeleteTess(tess: PGLUtesselator);
stdcall; external glu32;
procedure gluTessBeginPolygon(tess: PGLUtesselator; polygon_data: Pointer);
{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl;
{$ENDIF} external glu32;
procedure gluTessBeginContour(tess: PGLUtesselator); stdcall; external glu32;
procedure gluTessVertex(tess: PGLUtesselator; const coords: TVector3d;
  Data: Pointer);
stdcall; external glu32;
procedure gluTessEndContour(tess: PGLUtesselator); stdcall; external glu32;
procedure gluTessEndPolygon(tess: PGLUtesselator);
stdcall; external glu32;
procedure gluTessProperty(tess: PGLUtesselator; which: Cardinal;
  Value: TGLdouble);stdcall; external glu32;
procedure gluTessNormal(tess: PGLUtesselator; X, y, z: TGLdouble);stdcall; external glu32;
procedure gluTessCallback(tess: PGLUtesselator; which: Cardinal; fn: Pointer);stdcall; external glu32;
procedure gluGetTessProperty(tess: PGLUtesselator; which: Cardinal; Value: PGLdouble);
stdcall; external glu32;
function gluNewNurbsRenderer: PGLUnurbs; stdcall; external glu32;
procedure gluDeleteNurbsRenderer(nobj: PGLUnurbs);
stdcall; external glu32;
procedure gluBeginSurface(nobj: PGLUnurbs);
stdcall; external glu32;
procedure gluBeginCurve(nobj: PGLUnurbs);
stdcall; external glu32;
procedure gluEndCurve(nobj: PGLUnurbs);
stdcall; external glu32;
procedure gluEndSurface(nobj: PGLUnurbs);
stdcall; external glu32;
procedure gluBeginTrim(nobj: PGLUnurbs);
stdcall; external glu32;
procedure gluEndTrim(nobj: PGLUnurbs);
stdcall; external glu32;
procedure gluPwlCurve(nobj: PGLUnurbs; Count: TGLint; points: PGLfloat; stride: TGLint; atype: Cardinal);
stdcall; external glu32;
procedure gluNurbsCurve(nobj: PGLUnurbs; nknots: TGLint; knot: PGLfloat;
  stride: TGLint; ctlarray: PGLfloat; order: TGLint; atype: Cardinal);
stdcall; external glu32;
procedure gluNurbsSurface(nobj: PGLUnurbs; sknot_count: TGLint; sknot: PGLfloat;
  tknot_count: TGLint; tknot: PGLfloat; s_stride, t_stride: TGLint;
  ctlarray: PGLfloat; sorder, torder: TGLint; atype: Cardinal);
stdcall; external glu32;
procedure gluLoadSamplingMatrices(nobj: PGLUnurbs; const modelMatrix: TMatrix4f;
  const projMatrix: TMatrix4f; const Viewport: TVector4i);
stdcall; external glu32;
procedure gluNurbsProperty(nobj: PGLUnurbs; aproperty: Cardinal; Value: TGLfloat);
stdcall; external glu32;
procedure gluGetNurbsProperty(nobj: PGLUnurbs; aproperty: Cardinal; Value: PGLfloat);
stdcall; external glu32;
procedure gluNurbsCallback(nobj: PGLUnurbs; which: Cardinal;
  fn: TGLUNurbsErrorProc);
stdcall; external glu32;
procedure gluBeginPolygon(tess: PGLUtesselator);
stdcall; external glu32;
procedure gluNextContour(tess: PGLUtesselator; atype: Cardinal);
stdcall; external glu32;
procedure gluEndPolygon(tess: PGLUtesselator);
stdcall; external glu32;

function GetProcAddressGLLib(ProcName: PAnsiChar): Pointer;
function GetProcAddressGLS(ProcName: PAnsiChar): Pointer;

procedure CloseOpenGL;
function InitOpenGL: boolean;
function InitOpenGLFromLibrary(const GLName, GLUName: string): boolean;
function IsOpenGLInitialized: boolean;

// compatibility routines
procedure UnloadOpenGL;
function LoadOpenGL: boolean;
function LoadOpenGLFromLibrary(GLName, GLUName: string): boolean;
function IsOpenGLLoaded: boolean;

function IsMesaGL: boolean;
procedure TrimAndSplitVersionString(buffer: string; var max, min: integer);
function IsVersionMet(MajorVersion, MinorVersion, actualMajorVersion,
  actualMinorVersion: integer): boolean;

//------------------------------------------------------------------
implementation
//------------------------------------------------------------------

const
  glPrefix = 'gl';


{$IFDEF MSWINDOWS}

// ************** Windows specific ********************

const
  INVALID_MODULEHANDLE = 0;

var
  GLHandle: HINST;
  GLUHandle: HINST;
{$IFDEF EGL_SUPPORT}
  EGLHandle: HINST;
  EGL2Handle: HINST;
{$ENDIF}

function GetProcAddressGLS(ProcName: PAnsiChar): Pointer;
begin
{$IFNDEF EGL_SUPPORT}
  Result := wglGetProcAddress(ProcName);
{$ELSE}
  Result := getProcAddress(EGL2Handle, ProcName);
{$ENDIF}
end;

{$IFDEF EGL_SUPPORT}
function GetProcAddressEGL(ProcName: PAnsiChar): Pointer;
begin
  Result := getProcAddress(EGLHandle, ProcName);
end;
{$ENDIF}

{$ENDIF} //MSWINDOWS

function GetProcAddressGLLib(ProcName: PAnsiChar): Pointer;
{$IFDEF CROSSVCL}
var
  UniProcName: string;
{$ENDIF}
begin
  {$IFDEF CROSSVCL}
  UniProcName := ProcName;
  Result := getProcAddress(GLHandle, PChar(UniProcName));
  {$ELSE}
  Result := getProcAddress(GLHandle, ProcName);
  {$ENDIF}
end;

var
  vNotInformed: boolean = True;

procedure DebugCallBack(Source: Cardinal; type_: Cardinal; id: Cardinal;
  severity: Cardinal; length: TGLsizei; const message: PAnsiChar;
  userParam: Pointer);
stdcall;
begin
{$IFDEF USE_LOGGING}
  if length > 0 then
    GLSLogger.LogDebug(string(message));
{$ENDIF}
end;

procedure DebugCallBackAMD(id: Cardinal; category: Cardinal; severity: Cardinal;
  length: TGLsizei; message: PAnsiChar; userParam: Pointer);
stdcall;
begin
  if length > 0 then
    GLSLogger.LogDebug(string(message));
end;

constructor TGLExtensionsAndEntryPoints.Create;
begin
  FInitialized := False;
end;

procedure glCap;stdcall;
begin
{$IFDEF USE_LOGGING}
  GLSLogger.LogError('Call OpenGL function with undefined entry point');
{$ENDIF}
  Abort;
end;

function TGLExtensionsAndEntryPoints.GetAddress(const ProcName: string): Pointer;
var
  vName: string;
begin
  vName := glPrefix + ProcName;
  Result := GetProcAddressGLS(PAnsiChar(AnsiString(vName)));
      if Result = nil then
      begin
        vName := glPrefix + ProcName + 'ARB';
        Result := GetProcAddressGLS(PAnsiChar(AnsiString(vName)));
        if Result = nil then
        begin
          vName := glPrefix + ProcName;
          Result := GetProcAddressGLLib(PAnsiChar(AnsiString(vName)));
          if Result = nil then
          begin
            vName := glPrefix + ProcName + 'EXT';
            Result := GetProcAddressGLS(PAnsiChar(AnsiString(vName)));
            if Result = nil then
            begin
              vName := glPrefix + ProcName + 'NV';
              Result := GetProcAddressGLS(PAnsiChar(AnsiString(vName)));
              if Result = nil then
              begin
                vName := glPrefix + ProcName + 'ATI';
                Result := GetProcAddressGLS(PAnsiChar(AnsiString(vName)));
                if Result = nil then
                begin
                  vName := glPrefix + ProcName + 'OES';
                  Result := GetProcAddressGLS(PAnsiChar(AnsiString(vName)));
                  if Result = nil then
                    Result := @glCap;
                end;
              end;
            end;
          end;
        end;
      end;
{$IFDEF USE_OPENGL_DEBUG}
  if Result <> @glCap then
    GLSLogger.LogDebug('Finded entry point of ' + vName)
  else
    GLSLogger.LogDebug('Can''t find entry point of ' + vName);
{$ENDIF}
end;

function TGLExtensionsAndEntryPoints.GetAddressAlt(const ProcName1,
  ProcName2: string): Pointer;
begin
  Result := GetAddress(ProcName1);
  if Result = @glCap then
    Result := GetAddress(ProcName2);
end;

function TGLExtensionsAndEntryPoints.GetAddressNoSuffixes
  (const ProcName: string): Pointer;
var
  vName: string;
begin
  vName := glPrefix + ProcName;
  Result := GetProcAddressGLS(PAnsiChar(AnsiString(vName)));
  if Result = nil then
    Result := @glCap;
{$IFDEF USE_OPENGL_DEBUG}
  if Result <> @glCap then
    GLSLogger.LogDebug('Finded entry point of ' + vName)
  else
    GLSLogger.LogDebug('Can''t find entry point of ' + vName);
{$ENDIF}
end;

function TGLExtensionsAndEntryPoints.GetCapAddress: Pointer;
begin
  Result := @glCap;
end;

procedure TGLExtensionsAndEntryPoints.CheckError;
var
  glError: Cardinal;
  Count: word;
begin
  if FInitialized then
    try
      glError := GetError();
      if glError <> GL_NO_ERROR then
      begin
        Count := 0;
        try
          while (GetError <> GL_NO_ERROR) and (Count < 6) do
            Inc(Count);
        except
        end;
        if not(FDebug and ARB_debug_output) then
          case glError of
            GL_INVALID_ENUM:
              GLSLogger.LogError(format(strOpenGLError, ['Invalid enum']));
            GL_INVALID_VALUE:
              GLSLogger.LogError(format(strOpenGLError, ['Invalid value']));
            GL_INVALID_OPERATION:
              GLSLogger.LogError(format(strOpenGLError,
                ['Invalid Operation']));
            GL_OUT_OF_MEMORY:
              GLSLogger.LogError(format(strOpenGLError, ['Out of memory']));
          end;
      end;
    except
      GLSLogger.LogError(format(strOpenGLError, ['Exception in glGetError']));
    end;
end;

procedure TGLExtensionsAndEntryPoints.ClearError;
var
  n: integer;
begin
  n := 0;
  while (GetError <> GL_NO_ERROR) and (n < 6) do
    Inc(n);
end;

function TGLExtensionsAndEntryPoints.CheckExtension(const Extension
  : string): boolean;
var
  ExtPos: integer;
begin
  // First find the position of the extension string as substring in Buffer.
  ExtPos := Pos(Extension, FBuffer);
  Result := ExtPos > 0;
  // Now check that it isn't only a substring of another extension.
  if Result then
    Result := ((ExtPos + length(Extension) - 1) = length(FBuffer)) or
      (FBuffer[ExtPos + length(Extension)] = ' ');
{$IFDEF USE_OPENGL_DEBUG}
  if Result then
    GLSLogger.LogDebug(Extension);
{$ENDIF}
end;

procedure TGLExtensionsAndEntryPoints.Initialize(ATemporary: boolean);
var
  i: integer;
  numExt: TGLint;
  MajorVersion, MinorVersion: integer;
begin
  GLSLogger.LogNotice('Getting OpenGL entry points and extension');
{$IFDEF SUPPORT_WGL}
  ReadWGLExtensions;
  ReadWGLImplementationProperties;
{$ENDIF}
{$IFDEF EGL_SUPPORT}
  ReadEGLExtensions;
  ReadEGLImplementationProperties;
{$ENDIF}
  GetString := GetAddress('GetString');
  GetStringi := GetAddress('GetStringi');
  GetIntegerv := GetAddress('GetIntegerv');
  GetError := GetAddress('GetError');
  // determine OpenGL versions supported
  FBuffer := string(GetString(GL_VERSION));
  TrimAndSplitVersionString(FBuffer, MajorVersion, MinorVersion);
  VERSION_1_0 := True;
  VERSION_1_1 := IsVersionMet(1, 1, MajorVersion, MinorVersion);
  VERSION_1_2 := IsVersionMet(1, 2, MajorVersion, MinorVersion);
  VERSION_1_3 := IsVersionMet(1, 3, MajorVersion, MinorVersion);
  VERSION_1_4 := IsVersionMet(1, 4, MajorVersion, MinorVersion);
  VERSION_1_5 := IsVersionMet(1, 5, MajorVersion, MinorVersion);
  VERSION_2_0 := IsVersionMet(2, 0, MajorVersion, MinorVersion);
  VERSION_2_1 := IsVersionMet(2, 1, MajorVersion, MinorVersion);
  VERSION_3_0 := IsVersionMet(3, 0, MajorVersion, MinorVersion);
  VERSION_3_1 := IsVersionMet(3, 1, MajorVersion, MinorVersion);
  VERSION_3_2 := IsVersionMet(3, 2, MajorVersion, MinorVersion);
  VERSION_3_3 := IsVersionMet(3, 3, MajorVersion, MinorVersion);
  VERSION_4_0 := IsVersionMet(4, 0, MajorVersion, MinorVersion);
  VERSION_4_1 := IsVersionMet(4, 1, MajorVersion, MinorVersion);
  VERSION_4_2 := IsVersionMet(4, 2, MajorVersion, MinorVersion);

  if vNotInformed then
  begin
    GLSLogger.LogNotice('');
    GLSLogger.LogInfo('OpenGL rendering context information:');
    GLSLogger.LogInfo(format('Renderer     : %s', [GetString(GL_RENDERER)]));
    GLSLogger.LogInfo(format('Vendor       : %s', [GetString(GL_VENDOR)]));
    GLSLogger.LogInfo(format('Version      : %s', [GetString(GL_VERSION)]));
    if VERSION_2_0 then
      GLSLogger.LogInfo(format('GLSL version : %s',
        [GetString(GL_SHADING_LANGUAGE_VERSION)]))
    else
      GLSLogger.LogWarning('GLSL version : not supported');
    GLSLogger.LogNotice('');
    vNotInformed := False;
  end;

  if ATemporary then
  begin
    FInitialized := True;
    exit;
  end;

  // check supported OpenGL extensions
  if VERSION_3_0 then
  begin
    FBuffer := '';
    GetIntegerv(GL_NUM_EXTENSIONS, @numExt);
    for i := 0 to numExt - 1 do
      FBuffer := FBuffer + string(GetStringi(GL_EXTENSIONS, i)) + ' ';
  end
  else
    FBuffer := string(GetString(GL_EXTENSIONS));
  // check ARB approved OpenGL extensions
  ARB_blend_func_extended := CheckExtension('GL_ARB_blend_func_extended');
  ARB_color_buffer_float := CheckExtension('GL_ARB_color_buffer_float');
  ARB_compatibility := CheckExtension('GL_ARB_compatibility');
  ARB_copy_buffer := CheckExtension('GL_ARB_copy_buffer');
  ARB_depth_buffer_float := CheckExtension('GL_ARB_depth_buffer_float');
  ARB_depth_clamp := CheckExtension('GL_ARB_depth_clamp');
  ARB_depth_texture := CheckExtension('GL_ARB_depth_texture');
  ARB_draw_buffers := CheckExtension('GL_ARB_draw_buffers');
  ARB_draw_buffers_blend := CheckExtension('GL_ARB_draw_buffers_blend');
  ARB_draw_elements_base_vertex := CheckExtension('GL_ARB_draw_elements_base_vertex');
  ARB_draw_indirect := CheckExtension('GL_ARB_draw_indirect');
  ARB_draw_instanced := CheckExtension('GL_ARB_draw_instanced');
  ARB_explicit_attrib_location := CheckExtension('GL_ARB_explicit_attrib_location');
  ARB_fragment_coord_conventions := CheckExtension('GL_ARB_fragment_coord_conventions');
  ARB_fragment_program := CheckExtension('GL_ARB_fragment_program');
  ARB_fragment_program_shadow := CheckExtension('GL_ARB_fragment_program_shadow');
  ARB_fragment_shader := CheckExtension('GL_ARB_fragment_shader');
  ARB_framebuffer_object := CheckExtension('GL_ARB_framebuffer_object');
  ARB_framebuffer_sRGB := CheckExtension('GL_ARB_framebuffer_sRGB');
  ARB_geometry_shader4 := CheckExtension('GL_ARB_geometry_shader4');
  ARB_gpu_shader_fp64 := CheckExtension('GL_ARB_gpu_shader_fp64');
  ARB_gpu_shader5 := CheckExtension('GL_ARB_gpu_shader5');
  ARB_half_float_pixel := CheckExtension('GL_ARB_half_float_pixel');
  ARB_half_float_vertex := CheckExtension('GL_ARB_half_float_vertex');
  ARB_imaging := CheckExtension('GL_ARB_imaging');
  ARB_instanced_arrays := CheckExtension('GL_ARB_instanced_arrays');
  ARB_map_buffer_range := CheckExtension('GL_ARB_map_buffer_range');
  ARB_matrix_palette := CheckExtension('GL_ARB_matrix_palette');
  ARB_multisample := CheckExtension('GL_ARB_multisample');
  // ' ' to avoid collision with WGL variant
  ARB_multitexture := CheckExtension('GL_ARB_multitexture');
  ARB_occlusion_query := CheckExtension('GL_ARB_occlusion_query');
  ARB_occlusion_query2 := CheckExtension('GL_ARB_occlusion_query2');
  ARB_pixel_buffer_object := CheckExtension('GL_ARB_pixel_buffer_object');
  ARB_point_parameters := CheckExtension('GL_ARB_point_parameters');
  ARB_point_sprite := CheckExtension('GL_ARB_point_sprite');
  ARB_provoking_vertex := CheckExtension('GL_ARB_provoking_vertex');
  ARB_sample_shading := CheckExtension('GL_ARB_sample_shading');
  ARB_sampler_objects := CheckExtension('GL_ARB_sampler_objects');
  ARB_seamless_cube_map := CheckExtension('GL_ARB_seamless_cube_map');
  ARB_shader_bit_encoding := CheckExtension('GL_ARB_shader_bit_encoding');
  ARB_shader_objects := CheckExtension('GL_ARB_shader_objects');
  ARB_shader_subroutine := CheckExtension('GL_ARB_shader_subroutine');
  ARB_shader_texture_lod := CheckExtension('GL_ARB_shader_texture_lod');
  ARB_shading_language_100 := CheckExtension('GL_ARB_shading_language_100');
  ARB_shadow := CheckExtension('GL_ARB_shadow');
  ARB_shadow_ambient := CheckExtension('GL_ARB_shadow_ambient');
  ARB_sync := CheckExtension('GL_ARB_sync');
  ARB_tessellation_shader := CheckExtension('GL_ARB_tessellation_shader');
  ARB_texture_border_clamp := CheckExtension('GL_ARB_texture_border_clamp');
  ARB_texture_buffer_object := CheckExtension('GL_ARB_texture_buffer_object');
  ARB_texture_buffer_object_rgb32 := CheckExtension('GL_ARB_texture_buffer_object_rgb32');
  ARB_texture_compression := CheckExtension('GL_ARB_texture_compression');
  ARB_texture_compression_rgtc := CheckExtension('GL_ARB_texture_compression_rgtc');
  ARB_texture_cube_map := CheckExtension('GL_ARB_texture_cube_map');
  ARB_texture_cube_map_array := CheckExtension('GL_ARB_texture_cube_map_array');
  ARB_texture_env_add := CheckExtension('GL_ARB_texture_env_add');
  ARB_texture_env_combine := CheckExtension('GL_ARB_texture_env_combine');
  ARB_texture_env_crossbar := CheckExtension('GL_ARB_texture_env_crossbar');
  ARB_texture_env_dot3 := CheckExtension('GL_ARB_texture_env_dot3');
  ARB_texture_float := CheckExtension('GL_ARB_texture_float');
  ARB_texture_gather := CheckExtension('GL_ARB_texture_gather');
  ARB_texture_mirrored_repeat := CheckExtension('GL_ARB_texture_mirrored_repeat');
  ARB_texture_multisample := CheckExtension('GL_ARB_texture_multisample');
  ARB_texture_non_power_of_two := CheckExtension('GL_ARB_texture_non_power_of_two');
  ARB_texture_query_lod := CheckExtension('GL_ARB_texture_query_lod');
  ARB_texture_rectangle := CheckExtension('GL_ARB_texture_rectangle');
  ARB_texture_rg := CheckExtension('GL_ARB_texture_rg');
  ARB_texture_rgb10_a2ui := CheckExtension('GL_ARB_texture_rgb10_a2ui');
  ARB_texture_swizzle := CheckExtension('GL_ARB_texture_swizzle');
  ARB_timer_query := CheckExtension('GL_ARB_timer_query');
  ARB_transform_feedback2 := CheckExtension('GL_ARB_transform_feedback2');
  ARB_transform_feedback3 := CheckExtension('GL_ARB_transform_feedback3');
  ARB_transpose_matrix := CheckExtension('GL_ARB_transpose_matrix');
  ARB_uniform_buffer_object := CheckExtension('GL_ARB_uniform_buffer_object');
  ARB_vertex_array_bgra := CheckExtension('GL_ARB_vertex_array_bgra');
  ARB_vertex_array_object := CheckExtension('GL_ARB_vertex_array_object');
  ARB_vertex_blend := CheckExtension('GL_ARB_vertex_blend');
  ARB_vertex_buffer_object := CheckExtension('GL_ARB_vertex_buffer_object');
  ARB_vertex_program := CheckExtension('GL_ARB_vertex_program');
  ARB_vertex_shader := CheckExtension('GL_ARB_vertex_shader');
  ARB_vertex_type_2_10_10_10_rev := CheckExtension('GL_ARB_vertex_type_2_10_10_10_rev');
  ARB_window_pos := CheckExtension('GL_ARB_window_pos');
  ARB_texture_compression_bptc := CheckExtension('GL_ARB_texture_compression_bptc');
  ARB_get_program_binary := CheckExtension('GL_ARB_get_program_binary');
  ARB_separate_shader_objects := CheckExtension('GL_ARB_separate_shader_objects');
  ARB_shader_stencil_export := CheckExtension('GL_ARB_shader_stencil_export');
  KHR_debug := CheckExtension('GL_KHR_debug');
  ARB_clear_buffer_object := CheckExtension('GL_ARB_clear_buffer_object');
  ARB_compute_shader := CheckExtension('GL_ARB_compute_shader');
  ARB_copy_image := CheckExtension('GL_ARB_copy_image');
  ARB_debug_group := CheckExtension('GL_ARB_debug_group');
  ARB_debug_label := CheckExtension('GL_ARB_debug_label');
  ARB_debug_output2 := CheckExtension('GL_ARB_debug_output2');
  ARB_ES3_compatibility := CheckExtension('GL_ARB_ES3_compatibility');
  ARB_explicit_uniform_location := CheckExtension('GL_ARB_explicit_uniform_location');
  ARB_fragment_layer_viewport := CheckExtension('GL_ARB_fragment_layer_viewport');
  ARB_framebuffer_no_attachments := CheckExtension('GL_ARB_framebuffer_no_attachments');
  ARB_internalformat_query2 := CheckExtension('GL_ARB_internalformat_query2');
  ARB_invalidate_subdata := CheckExtension('GL_ARB_invalidate_subdata');
  ARB_multi_draw_indirect := CheckExtension('GL_ARB_multi_draw_indirect');
  ARB_program_interface_query := CheckExtension('GL_ARB_program_interface_query');
  ARB_shader_image_size := CheckExtension('GL_ARB_shader_image_size');
  ARB_shader_storage_buffer_object := CheckExtension('GL_ARB_shader_storage_buffer_object');
  ARB_stencil_texturing := CheckExtension('GL_ARB_stencil_texturing');
  ARB_texture_buffer_range := CheckExtension('GL_ARB_texture_buffer_range');
  ARB_texture_query_levels := CheckExtension('GL_ARB_texture_query_levels');
  ARB_texture_storage_multisample := CheckExtension('GL_ARB_texture_storage_multisample');
  ARB_texture_view := CheckExtension('GL_ARB_texture_view');
  ARB_vertex_attrib_binding := CheckExtension('GL_ARB_vertex_attrib_binding');
  ARB_robustness_isolation := CheckExtension('GL_ARB_robustness_isolation');
  ARB_cl_event := CheckExtension('GL_ARB_cl_event');
  // check Vendor/EXT OpenGL extensions
  _3DFX_multisample := CheckExtension('GL_3DFX_multisample');
  _3DFX_tbuffer := CheckExtension('GL_3DFX_tbuffer');
  _3DFX_texture_compression_FXT1 := CheckExtension('GL_3DFX_texture_compression_FXT1');
  ATI_draw_buffers := CheckExtension('GL_ATI_draw_buffers');
  ATI_texture_compression_3dc := CheckExtension('GL_ATI_texture_compression_3dc');
  ATI_texture_float := CheckExtension('GL_ATI_texture_float');
  ATI_texture_mirror_once := CheckExtension('GL_ATI_texture_mirror_once');

  S3_s3tc := CheckExtension('GL_S3_s3tc');

  EXT_abgr := CheckExtension('GL_EXT_abgr');
  EXT_bgra := CheckExtension('GL_EXT_bgra');
  EXT_bindable_uniform := CheckExtension('GL_EXT_bindable_uniform');
  EXT_blend_color := CheckExtension('GL_EXT_blend_color');
  EXT_blend_equation_separate := CheckExtension('GL_EXT_blend_equation_separate');
  EXT_blend_func_separate := CheckExtension('GL_EXT_blend_func_separate');
  EXT_blend_logic_op := CheckExtension('GL_EXT_blend_logic_op');
  EXT_blend_minmax := CheckExtension('GL_EXT_blend_minmax');
  EXT_blend_subtract := CheckExtension('GL_EXT_blend_subtract');
  EXT_Cg_shader := CheckExtension('GL_EXT_Cg_shader');
  EXT_clip_volume_hint := CheckExtension('GL_EXT_clip_volume_hint');
  EXT_compiled_vertex_array := CheckExtension('GL_EXT_compiled_vertex_array');
  EXT_copy_texture := CheckExtension('GL_EXT_copy_texture');
  EXT_depth_bounds_test := CheckExtension('GL_EXT_depth_bounds_test');
  EXT_draw_buffers2 := CheckExtension('GL_EXT_draw_buffers2');
  EXT_draw_instanced := CheckExtension('GL_EXT_draw_instanced');
  EXT_draw_range_elements := CheckExtension('GL_EXT_draw_range_elements');
  EXT_fog_coord := CheckExtension('GL_EXT_fog_coord');
  EXT_framebuffer_blit := CheckExtension('GL_EXT_framebuffer_blit');
  EXT_framebuffer_multisample := CheckExtension('GL_EXT_framebuffer_multisample');
  EXT_framebuffer_object := CheckExtension('GL_EXT_framebuffer_object');
  EXT_framebuffer_sRGB := CheckExtension('GL_EXT_framebuffer_sRGB');
  EXT_geometry_shader4 := CheckExtension('GL_EXT_geometry_shader4');
  EXT_gpu_program_parameters := CheckExtension('GL_EXT_gpu_program_parameters');
  EXT_gpu_shader4 := CheckExtension('GL_EXT_gpu_shader4');
  EXT_multi_draw_arrays := CheckExtension('GL_EXT_multi_draw_arrays');
  EXT_multisample := CheckExtension('GL_EXT_multisample');
  EXT_packed_depth_stencil := CheckExtension('GL_EXT_packed_depth_stencil');
  EXT_packed_float := CheckExtension('GL_EXT_packed_float');
  EXT_packed_pixels := CheckExtension('GL_EXT_packed_pixels');
  EXT_paletted_texture := CheckExtension('GL_EXT_paletted_texture');
  EXT_pixel_buffer_object := CheckExtension('GL_EXT_pixel_buffer_object');
  EXT_polygon_offset := CheckExtension('GL_EXT_polygon_offset');
  EXT_rescale_normal := CheckExtension('GL_EXT_rescale_normal');
  EXT_secondary_color := CheckExtension('GL_EXT_secondary_color');
  EXT_separate_specular_color := CheckExtension('GL_EXT_separate_specular_color');
  EXT_shadow_funcs := CheckExtension('GL_EXT_shadow_funcs');
  EXT_shared_texture_palette := CheckExtension('GL_EXT_shared_texture_palette');
  EXT_stencil_clear_tag := CheckExtension('GL_EXT_stencil_clear_tag');
  EXT_stencil_two_side := CheckExtension('GL_EXT_stencil_two_side');
  EXT_stencil_wrap := CheckExtension('GL_EXT_stencil_wrap');
  EXT_texture3D := CheckExtension('GL_EXT_texture3D');
  EXT_texture_array := CheckExtension('GL_EXT_texture_array');
  EXT_texture_buffer_object := CheckExtension('GL_EXT_texture_buffer_object');
  EXT_texture_compression_latc := CheckExtension('GL_EXT_texture_compression_latc');
  EXT_texture_compression_rgtc := CheckExtension('GL_EXT_texture_compression_rgtc');
  EXT_texture_compression_s3tc := CheckExtension('GL_EXT_texture_compression_s3tc');
  EXT_texture_cube_map := CheckExtension('GL_EXT_texture_cube_map');
  EXT_texture_edge_clamp := CheckExtension('GL_EXT_texture_edge_clamp');
  EXT_texture_env_add := CheckExtension('GL_EXT_texture_env_add');
  EXT_texture_env_combine := CheckExtension('GL_EXT_texture_env_combine');
  EXT_texture_env_dot3 := CheckExtension('GL_EXT_texture_env_dot3');
  EXT_texture_filter_anisotropic :=
    CheckExtension('GL_EXT_texture_filter_anisotropic');
  EXT_texture_integer := CheckExtension('GL_EXT_texture_integer');
  EXT_texture_lod := CheckExtension('GL_EXT_texture_lod');
  EXT_texture_lod_bias := CheckExtension('GL_EXT_texture_lod_bias');
  EXT_texture_mirror_clamp := CheckExtension('GL_EXT_texture_mirror_clamp');
  EXT_texture_object := CheckExtension('GL_EXT_texture_object');
  EXT_texture_rectangle := CheckExtension('GL_EXT_texture_rectangle');
  EXT_texture_sRGB := CheckExtension('GL_EXT_texture_sRGB');
  EXT_texture_shared_exponent := CheckExtension('GL_EXT_texture_shared_exponent');
  EXT_timer_query := CheckExtension('GL_EXT_timer_query');
  EXT_transform_feedback := CheckExtension('GL_EXT_transform_feedback');
  EXT_vertex_array := CheckExtension('GL_EXT_vertex_array');
  EXT_texture_sRGB_decode := CheckExtension('GL_EXT_texture_sRGB_decode');
  EXT_direct_state_access := CheckExtension('EXT_direct_state_access');
  EXT_texture_swizzle := CheckExtension('EXT_texture_swizzle');
  HP_occlusion_test := CheckExtension('GL_HP_occlusion_test');
  IBM_rasterpos_clip := CheckExtension('GL_IBM_rasterpos_clip');
  KTX_buffer_region := CheckExtension('GL_KTX_buffer_region');
  MESA_resize_buffers := CheckExtension('GL_MESA_resize_buffers');
  NV_blend_square := CheckExtension('GL_NV_blend_square');
  NV_conditional_render := CheckExtension('GL_NV_conditional_render');
  NV_copy_image := CheckExtension('GL_NV_copy_image');
  NV_depth_buffer_float := CheckExtension('GL_NV_depth_buffer_float');
  NV_fence := CheckExtension('GL_NV_fence');
  NV_float_buffer := CheckExtension('GL_NV_float_buffer');
  NV_fog_distance := CheckExtension('GL_NV_fog_distance');
  NV_geometry_program4 := CheckExtension('GL_NV_geometry_program4');
  NV_light_max_exponent := CheckExtension('GL_NV_light_max_exponent');
  NV_multisample_filter_hint := CheckExtension('GL_NV_multisample_filter_hint');
  NV_occlusion_query := CheckExtension('GL_NV_occlusion_query');
  NV_point_sprite := CheckExtension('GL_NV_point_sprite');
  NV_primitive_restart := CheckExtension('GL_NV_primitive_restart');
  NV_register_combiners := CheckExtension('GL_NV_register_combiners');
  NV_shader_buffer_load := CheckExtension('GL_NV_shader_buffer_load');
  NV_texgen_reflection := CheckExtension('GL_NV_texgen_reflection');
  NV_texture_compression_vtc := CheckExtension('GL_NV_texture_compression_vtc');
  NV_texture_env_combine4 := CheckExtension('GL_NV_texture_env_combine4');
  NV_texture_rectangle := CheckExtension('GL_NV_texture_rectangle');
  NV_texture_shader := CheckExtension('GL_NV_texture_shader');
  NV_texture_shader2 := CheckExtension('GL_NV_texture_shader2');
  NV_texture_shader3 := CheckExtension('GL_NV_texture_shader3');
  NV_transform_feedback := CheckExtension('GL_NV_transform_feedback');
  NV_vertex_array_range := CheckExtension('GL_NV_vertex_array_range');
  NV_vertex_array_range2 := CheckExtension('GL_NV_vertex_array_range2');
  NV_vertex_buffer_unified_memory := CheckExtension('GL_NV_vertex_buffer_unified_memory');
  NV_vertex_program := CheckExtension('GL_NV_vertex_program');
  SGI_color_matrix := CheckExtension('GL_SGI_color_matrix');
  SGIS_generate_mipmap := CheckExtension('GL_SGIS_generate_mipmap');
  SGIS_multisample := CheckExtension('GL_SGIS_multisample');
  SGIS_texture_border_clamp := CheckExtension('GL_SGIS_texture_border_clamp');
  SGIS_texture_color_mask := CheckExtension('GL_SGIS_texture_color_mask');
  SGIS_texture_edge_clamp := CheckExtension('GL_SGIS_texture_edge_clamp');
  SGIS_texture_lod := CheckExtension('GL_SGIS_texture_lod');
  SGIX_depth_texture := CheckExtension('GL_SGIX_depth_texture');
  SGIX_shadow := CheckExtension('GL_SGIX_shadow');
  SGIX_shadow_ambient := CheckExtension('GL_SGIX_shadow_ambient');
  AMD_vertex_shader_tessellator := CheckExtension('GL_AMD_vertex_shader_tessellator');
  WIN_swap_hint := CheckExtension('GL_WIN_swap_hint');
  ATI_meminfo := CheckExtension('GL_ATI_meminfo');
  NVX_gpu_memory_info := CheckExtension('GL_NVX_gpu_memory_info');
  NV_vdpau_interop := CheckExtension('GL_NV_vdpau_interop');
  NV_path_rendering := CheckExtension('GL_NV_path_rendering');
  GREMEDY_frame_terminator := CheckExtension('GL_GREMEDY_frame_terminator');
  GREMEDY_string_marker := CheckExtension('GL_GREMEDY_string_marker');
  AMDX_debug_output := CheckExtension('AMDX_debug_output');
  ARB_debug_output := CheckExtension('GL_ARB_debug_output');
  BindTexture := GetAddress('BindTexture');
  BlendFunc := GetAddress('BlendFunc');
  Clear := GetAddress('Clear');
  ClearColor := GetAddress('ClearColor');
  ClearDepth := GetAddress('ClearDepth');
  ClearStencil := GetAddress('ClearStencil');
  ColorMask := GetAddress('ColorMask');
  CopyTexImage1D := GetAddress('CopyTexImage1D');
  CopyTexImage2D := GetAddress('CopyTexImage2D');
  CopyTexSubImage1D := GetAddress('CopyTexSubImage1D');
  CopyTexSubImage2D := GetAddress('CopyTexSubImage2D');
  CullFace := GetAddress('CullFace');
  DeleteTextures := GetAddress('DeleteTextures');
  DepthFunc := GetAddress('DepthFunc');
  DepthMask := GetAddress('DepthMask');
  DepthRange := GetAddress('DepthRange');
  Disable := GetAddress('Disable');
  DrawArrays := GetAddress('DrawArrays');
  DrawBuffer := GetAddress('DrawBuffer');
  DrawElements := GetAddress('DrawElements');
  Enable := GetAddress('Enable');
  Finish := GetAddress('Finish');
  Flush := GetAddress('Flush');
  FrontFace := GetAddress('FrontFace');
  GenTextures := GetAddress('GenTextures');
  GetBooleanv := GetAddress('GetBooleanv');
  GetDoublev := GetAddress('GetDoublev');
  GetFloatv := GetAddress('GetFloatv');
  GetPointerv := GetAddress('GetPointerv');
  GetString := GetAddress('GetString');
  GetTexImage := GetAddress('GetTexImage');
  GetTexLevelParameterfv := GetAddress('GetTexLevelParameterfv');
  GetTexLevelParameteriv := GetAddress('GetTexLevelParameteriv');
  GetTexParameterfv := GetAddress('GetTexParameterfv');
  GetTexParameteriv := GetAddress('GetTexParameteriv');
  Hint := GetAddress('Hint');
  IsEnabled := GetAddress('IsEnabled');
  IsTexture := GetAddress('IsTexture');
  LineWidth := GetAddress('LineWidth');
  LogicOp := GetAddress('LogicOp');
  PixelStoref := GetAddress('PixelStoref');
  PixelStorei := GetAddress('PixelStorei');
  PointSize := GetAddress('PointSize');
  PolygonMode := GetAddress('PolygonMode');
  PolygonOffset := GetAddress('PolygonOffset');
  ReadBuffer := GetAddress('ReadBuffer');
  ReadPixels := GetAddress('ReadPixels');
  Scissor := GetAddress('Scissor');
  StencilFunc := GetAddress('StencilFunc');
  StencilMask := GetAddress('StencilMask');
  StencilOp := GetAddress('StencilOp');
  TexImage1D := GetAddress('TexImage1D');
  TexImage2D := GetAddress('TexImage2D');
  TexParameterf := GetAddress('TexParameterf');
  TexParameterfv := GetAddress('TexParameterfv');
  TexParameteri := GetAddress('TexParameteri');
  TexParameteriv := GetAddress('TexParameteriv');
  TexSubImage1D := GetAddress('TexSubImage1D');
  TexSubImage2D := GetAddress('TexSubImage2D');
  Viewport := GetAddress('Viewport');
  Accum := GetAddress('Accum');
  AlphaFunc := GetAddress('AlphaFunc');
  AreTexturesResident := GetAddress('AreTexturesResident');
  ArrayElement := GetAddress('ArrayElement');
  Begin_ := GetAddress('Begin');
  Bitmap := GetAddress('Bitmap');
  CallList := GetAddress('CallList');
  CallLists := GetAddress('CallLists');
  ClearAccum := GetAddress('ClearAccum');
  ClearIndex := GetAddress('ClearIndex');
  ClipPlane := GetAddress('ClipPlane');
  Color3b := GetAddress('Color3b');
  Color3bv := GetAddress('Color3bv');
  Color3d := GetAddress('Color3d');
  Color3dv := GetAddress('Color3dv');
  Color3f := GetAddress('Color3f');
  Color3fv := GetAddress('Color3fv');
  Color3i := GetAddress('Color3i');
  Color3iv := GetAddress('Color3iv');
  Color3s := GetAddress('Color3s');
  Color3sv := GetAddress('Color3sv');
  Color3ub := GetAddress('Color3ub');
  Color3ubv := GetAddress('Color3ubv');
  Color3ui := GetAddress('Color3ui');
  Color3uiv := GetAddress('Color3uiv');
  Color3us := GetAddress('Color3us');
  Color3usv := GetAddress('Color3usv');
  Color4b := GetAddress('Color4b');
  Color4bv := GetAddress('Color4bv');
  Color4d := GetAddress('Color4d');
  Color4dv := GetAddress('Color4dv');
  Color4f := GetAddress('Color4f');
  Color4fv := GetAddress('Color4fv');
  Color4i := GetAddress('Color4i');
  Color4iv := GetAddress('Color4iv');
  Color4s := GetAddress('Color4s');
  Color4sv := GetAddress('Color4sv');
  Color4ub := GetAddress('Color4ub');
  Color4ubv := GetAddress('Color4ubv');
  Color4ui := GetAddress('Color4ui');
  Color4uiv := GetAddress('Color4uiv');
  Color4us := GetAddress('Color4us');
  Color4usv := GetAddress('Color4usv');
  ColorMaterial := GetAddress('ColorMaterial');
  ColorPointer := GetAddress('ColorPointer');
  CopyPixels := GetAddress('CopyPixels');
  DeleteLists := GetAddress('DeleteLists');
  DisableClientState := GetAddress('DisableClientState');
  DrawPixels := GetAddress('DrawPixels');
  EdgeFlag := GetAddress('EdgeFlag');
  EdgeFlagPointer := GetAddress('EdgeFlagPointer');
  EdgeFlagv := GetAddress('EdgeFlagv');
  EnableClientState := GetAddress('EnableClientState');
  End_ := GetAddress('End');
  EndList := GetAddress('EndList');
  EvalCoord1d := GetAddress('EvalCoord1d');
  EvalCoord1dv := GetAddress('EvalCoord1dv');
  EvalCoord1f := GetAddress('EvalCoord1f');
  EvalCoord1fv := GetAddress('EvalCoord1fv');
  EvalCoord2d := GetAddress('EvalCoord2d');
  EvalCoord2dv := GetAddress('EvalCoord2dv');
  EvalCoord2f := GetAddress('EvalCoord2f');
  EvalCoord2fv := GetAddress('EvalCoord2fv');
  EvalMesh1 := GetAddress('EvalMesh1');
  EvalMesh2 := GetAddress('EvalMesh2');
  EvalPoint1 := GetAddress('EvalPoint1');
  EvalPoint2 := GetAddress('EvalPoint2');
  FeedbackBuffer := GetAddress('FeedbackBuffer');
  Fogf := GetAddress('Fogf');
  Fogfv := GetAddress('Fogfv');
  Fogi := GetAddress('Fogi');
  Fogiv := GetAddress('Fogiv');
  Frustum := GetAddress('Frustum');
  GenLists := GetAddress('GenLists');
  GetClipPlane := GetAddress('GetClipPlane');
  GetLightfv := GetAddress('GetLightfv');
  GetLightiv := GetAddress('GetLightiv');
  GetMapdv := GetAddress('GetMapdv');
  GetMapfv := GetAddress('GetMapfv');
  GetMapiv := GetAddress('GetMapiv');
  GetMaterialfv := GetAddress('GetMaterialfv');
  GetMaterialiv := GetAddress('GetMaterialiv');
  GetPixelMapfv := GetAddress('GetPixelMapfv');
  GetPixelMapuiv := GetAddress('GetPixelMapuiv');
  GetPixelMapusv := GetAddress('GetPixelMapusv');
  GetPolygonStipple := GetAddress('GetPolygonStipple');
  GetTexEnvfv := GetAddress('GetTexEnvfv');
  GetTexEnviv := GetAddress('GetTexEnviv');
  GetTexGendv := GetAddress('GetTexGendv');
  GetTexGenfv := GetAddress('GetTexGenfv');
  GetTexGeniv := GetAddress('GetTexGeniv');
  IndexMask := GetAddress('IndexMask');
  IndexPointer := GetAddress('IndexPointer');
  Indexd := GetAddress('Indexd');
  Indexdv := GetAddress('Indexdv');
  Indexf := GetAddress('Indexf');
  Indexfv := GetAddress('Indexfv');
  Indexi := GetAddress('Indexi');
  Indexiv := GetAddress('Indexiv');
  Indexs := GetAddress('Indexs');
  Indexsv := GetAddress('Indexsv');
  Indexub := GetAddress('Indexub');
  Indexubv := GetAddress('Indexubv');
  InitNames := GetAddress('InitNames');
  InterleavedArrays := GetAddress('InterleavedArrays');
  IsList := GetAddress('IsList');
  LightModelf := GetAddress('LightModelf');
  LightModelfv := GetAddress('LightModelfv');
  LightModeli := GetAddress('LightModeli');
  LightModeliv := GetAddress('LightModeliv');
  Lightf := GetAddress('Lightf');
  Lightfv := GetAddress('Lightfv');
  Lighti := GetAddress('Lighti');
  Lightiv := GetAddress('Lightiv');
  LineStipple := GetAddress('LineStipple');
  ListBase := GetAddress('ListBase');
  LoadIdentity := GetAddress('LoadIdentity');
  LoadMatrixd := GetAddress('LoadMatrixd');
  LoadMatrixf := GetAddress('LoadMatrixf');
  LoadName := GetAddress('LoadName');
  Map1d := GetAddress('Map1d');
  Map1f := GetAddress('Map1f');
  Map2d := GetAddress('Map2d');
  Map2f := GetAddress('Map2f');
  MapGrid1d := GetAddress('MapGrid1d');
  MapGrid1f := GetAddress('MapGrid1f');
  MapGrid2d := GetAddress('MapGrid2d');
  MapGrid2f := GetAddress('MapGrid2f');
  Materialf := GetAddress('Materialf');
  Materialfv := GetAddress('Materialfv');
  Materiali := GetAddress('Materiali');
  Materialiv := GetAddress('Materialiv');
  MatrixMode := GetAddress('MatrixMode');
  MultMatrixd := GetAddress('MultMatrixd');
  MultMatrixf := GetAddress('MultMatrixf');
  NewList := GetAddress('NewList');
  Normal3b := GetAddress('Normal3b');
  Normal3bv := GetAddress('Normal3bv');
  Normal3d := GetAddress('Normal3d');
  Normal3dv := GetAddress('Normal3dv');
  Normal3f := GetAddress('Normal3f');
  Normal3fv := GetAddress('Normal3fv');
  Normal3i := GetAddress('Normal3i');
  Normal3iv := GetAddress('Normal3iv');
  Normal3s := GetAddress('Normal3s');
  Normal3sv := GetAddress('Normal3sv');
  NormalPointer := GetAddress('NormalPointer');
  Ortho := GetAddress('Ortho');
  PassThrough := GetAddress('PassThrough');
  PixelMapfv := GetAddress('PixelMapfv');
  PixelMapuiv := GetAddress('PixelMapuiv');
  PixelMapusv := GetAddress('PixelMapusv');
  PixelTransferf := GetAddress('PixelTransferf');
  PixelTransferi := GetAddress('PixelTransferi');
  PixelZoom := GetAddress('PixelZoom');
  PolygonStipple := GetAddress('PolygonStipple');
  PopAttrib := GetAddress('PopAttrib');
  PopClientAttrib := GetAddress('PopClientAttrib');
  PopMatrix := GetAddress('PopMatrix');
  PopName := GetAddress('PopName');
  PrioritizeTextures := GetAddress('PrioritizeTextures');
  PushAttrib := GetAddress('PushAttrib');
  PushClientAttrib := GetAddress('PushClientAttrib');
  PushMatrix := GetAddress('PushMatrix');
  PushName := GetAddress('PushName');
  RasterPos2d := GetAddress('RasterPos2d');
  RasterPos2dv := GetAddress('RasterPos2dv');
  RasterPos2f := GetAddress('RasterPos2f');
  RasterPos2fv := GetAddress('RasterPos2fv');
  RasterPos2i := GetAddress('RasterPos2i');
  RasterPos2iv := GetAddress('RasterPos2iv');
  RasterPos2s := GetAddress('RasterPos2s');
  RasterPos2sv := GetAddress('RasterPos2sv');
  RasterPos3d := GetAddress('RasterPos3d');
  RasterPos3dv := GetAddress('RasterPos3dv');
  RasterPos3f := GetAddress('RasterPos3f');
  RasterPos3fv := GetAddress('RasterPos3fv');
  RasterPos3i := GetAddress('RasterPos3i');
  RasterPos3iv := GetAddress('RasterPos3iv');
  RasterPos3s := GetAddress('RasterPos3s');
  RasterPos3sv := GetAddress('RasterPos3sv');
  RasterPos4d := GetAddress('RasterPos4d');
  RasterPos4dv := GetAddress('RasterPos4dv');
  RasterPos4f := GetAddress('RasterPos4f');
  RasterPos4fv := GetAddress('RasterPos4fv');
  RasterPos4i := GetAddress('RasterPos4i');
  RasterPos4iv := GetAddress('RasterPos4iv');
  RasterPos4s := GetAddress('RasterPos4s');
  RasterPos4sv := GetAddress('RasterPos4sv');
  Rectd := GetAddress('Rectd');
  Rectdv := GetAddress('Rectdv');
  Rectf := GetAddress('Rectf');
  Rectfv := GetAddress('Rectfv');
  Recti := GetAddress('Recti');
  Rectiv := GetAddress('Rectiv');
  Rects := GetAddress('Rects');
  Rectsv := GetAddress('Rectsv');
  RenderMode := GetAddress('RenderMode');
  Rotated := GetAddress('Rotated');
  Rotatef := GetAddress('Rotatef');
  Scaled := GetAddress('Scaled');
  Scalef := GetAddress('Scalef');
  SelectBuffer := GetAddress('SelectBuffer');
  ShadeModel := GetAddress('ShadeModel');
  TexCoord1d := GetAddress('TexCoord1d');
  TexCoord1dv := GetAddress('TexCoord1dv');
  TexCoord1f := GetAddress('TexCoord1f');
  TexCoord1fv := GetAddress('TexCoord1fv');
  TexCoord1i := GetAddress('TexCoord1i');
  TexCoord1iv := GetAddress('TexCoord1iv');
  TexCoord1s := GetAddress('TexCoord1s');
  TexCoord1sv := GetAddress('TexCoord1sv');
  TexCoord2d := GetAddress('TexCoord2d');
  TexCoord2dv := GetAddress('TexCoord2dv');
  TexCoord2f := GetAddress('TexCoord2f');
  TexCoord2fv := GetAddress('TexCoord2fv');
  TexCoord2i := GetAddress('TexCoord2i');
  TexCoord2iv := GetAddress('TexCoord2iv');
  TexCoord2s := GetAddress('TexCoord2s');
  TexCoord2sv := GetAddress('TexCoord2sv');
  TexCoord3d := GetAddress('TexCoord3d');
  TexCoord3dv := GetAddress('TexCoord3dv');
  TexCoord3f := GetAddress('TexCoord3f');
  TexCoord3fv := GetAddress('TexCoord3fv');
  TexCoord3i := GetAddress('TexCoord3i');
  TexCoord3iv := GetAddress('TexCoord3iv');
  TexCoord3s := GetAddress('TexCoord3s');
  TexCoord3sv := GetAddress('TexCoord3sv');
  TexCoord4d := GetAddress('TexCoord4d');
  TexCoord4dv := GetAddress('TexCoord4dv');
  TexCoord4f := GetAddress('TexCoord4f');
  TexCoord4fv := GetAddress('TexCoord4fv');
  TexCoord4i := GetAddress('TexCoord4i');
  TexCoord4iv := GetAddress('TexCoord4iv');
  TexCoord4s := GetAddress('TexCoord4s');
  TexCoord4sv := GetAddress('TexCoord4sv');
  TexCoordPointer := GetAddress('TexCoordPointer');
  TexEnvf := GetAddress('TexEnvf');
  TexEnvfv := GetAddress('TexEnvfv');
  TexEnvi := GetAddress('TexEnvi');
  TexEnviv := GetAddress('TexEnviv');
  TexGend := GetAddress('TexGend');
  TexGendv := GetAddress('TexGendv');
  TexGenf := GetAddress('TexGenf');
  TexGenfv := GetAddress('TexGenfv');
  TexGeni := GetAddress('TexGeni');
  TexGeniv := GetAddress('TexGeniv');
  Translated := GetAddress('Translated');
  Translatef := GetAddress('Translatef');
  Vertex2d := GetAddress('Vertex2d');
  Vertex2dv := GetAddress('Vertex2dv');
  Vertex2f := GetAddress('Vertex2f');
  Vertex2fv := GetAddress('Vertex2fv');
  Vertex2i := GetAddress('Vertex2i');
  Vertex2iv := GetAddress('Vertex2iv');
  Vertex2s := GetAddress('Vertex2s');
  Vertex2sv := GetAddress('Vertex2sv');
  Vertex3d := GetAddress('Vertex3d');
  Vertex3dv := GetAddress('Vertex3dv');
  Vertex3f := GetAddress('Vertex3f');
  Vertex3fv := GetAddress('Vertex3fv');
  Vertex3i := GetAddress('Vertex3i');
  Vertex3iv := GetAddress('Vertex3iv');
  Vertex3s := GetAddress('Vertex3s');
  Vertex3sv := GetAddress('Vertex3sv');
  Vertex4d := GetAddress('Vertex4d');
  Vertex4dv := GetAddress('Vertex4dv');
  Vertex4f := GetAddress('Vertex4f');
  Vertex4fv := GetAddress('Vertex4fv');
  Vertex4i := GetAddress('Vertex4i');
  Vertex4iv := GetAddress('Vertex4iv');
  Vertex4s := GetAddress('Vertex4s');
  Vertex4sv := GetAddress('Vertex4sv');
  VertexPointer := GetAddress('VertexPointer');
  BlendColor := GetAddress('BlendColor');
  BlendEquation := GetAddress('BlendEquation');
  DrawRangeElements := GetAddress('DrawRangeElements');
  TexImage3D := GetAddress('TexImage3D');
  TexSubImage3D := GetAddress('TexSubImage3D');
  CopyTexSubImage3D := GetAddress('CopyTexSubImage3D');

  IsRenderbuffer := GetAddress('IsRenderbuffer');
  BindRenderbuffer := GetAddress('BindRenderbuffer');
  DeleteRenderbuffers := GetAddress('DeleteRenderbuffers');
  GenRenderbuffers := GetAddress('GenRenderbuffers');
  RenderbufferStorage := GetAddress('RenderbufferStorage');
  RenderbufferStorageMultisample :=
    GetAddress('RenderbufferStorageMultisample');
  GetRenderbufferParameteriv := GetAddress('GetRenderbufferParameteriv');
  IsFramebuffer := GetAddress('IsFramebuffer');
  BindFramebuffer := GetAddress('BindFramebuffer');
  DeleteFramebuffers := GetAddress('DeleteFramebuffers');
  GenFramebuffers := GetAddress('GenFramebuffers');
  CheckFramebufferStatus := GetAddress('CheckFramebufferStatus');
  FramebufferTexture := GetAddress('FramebufferTexture');
  FramebufferTexture1D := GetAddress('FramebufferTexture1D');
  FramebufferTexture2D := GetAddress('FramebufferTexture2D');
  FramebufferTexture3D := GetAddress('FramebufferTexture3D');
  FramebufferTextureLayer := GetAddress('FramebufferTextureLayer');
  FramebufferTextureFace := GetAddress('FramebufferTextureFace');
  FramebufferRenderbuffer := GetAddress('FramebufferRenderbuffer');
  GetFramebufferAttachmentParameteriv :=
    GetAddress('GetFramebufferAttachmentParameteriv');
  BlitFramebuffer := GetAddress('BlitFramebuffer');
  GenerateMipmap := GetAddress('GenerateMipmap');
  ClearBufferiv := GetAddress('ClearBufferiv');
  ClearBufferuiv := GetAddress('ClearBufferuiv');
  ClearBufferfv := GetAddress('ClearBufferfv');
  ClearBufferfi := GetAddress('ClearBufferfi');
  BlendFuncSeparate := GetAddress('BlendFuncSeparate');
  MultiDrawArrays := GetAddress('MultiDrawArrays');
  MultiDrawElements := GetAddress('MultiDrawElements');
  PointParameterf := GetAddress('PointParameterf');
  PointParameterfv := GetAddress('PointParameterfv');
  PointParameteri := GetAddress('PointParameteri');
  PointParameteriv := GetAddress('PointParameteriv');
  LockArrays := GetAddress('LockArrays');
  UnlockArrays := GetAddress('UnlockArrays');
  BindBuffer := GetAddress('BindBuffer');
  DeleteBuffers := GetAddress('DeleteBuffers');
  GenBuffers := GetAddress('GenBuffers');
  IsBuffer := GetAddress('IsBuffer');
  BufferData := GetAddress('BufferData');
  BufferSubData := GetAddress('BufferSubData');
  GetBufferSubData := GetAddress('GetBufferSubData');
  MapBuffer := GetAddress('MapBuffer');
  UnmapBuffer := GetAddress('UnmapBuffer');
  GetBufferParameteriv := GetAddress('GetBufferParameteriv');
  GetBufferPointerv := GetAddress('GetBufferPointerv');
  MapBufferRange := GetAddress('MapBufferRange');
  FlushMappedBufferRange := GetAddress('FlushMappedBufferRange');
  BindBufferRange := GetAddress('BindBufferRange');
  BindBufferOffset := GetAddress('BindBufferOffset');
  BindBufferBase := GetAddress('BindBufferBase');
  BeginTransformFeedback := GetAddress('BeginTransformFeedback');
  EndTransformFeedback := GetAddress('EndTransformFeedback');
  TransformFeedbackVaryings := GetAddress('TransformFeedbackVaryings');
  GetTransformFeedbackVarying := GetAddress('GetTransformFeedbackVarying');
  TransformFeedbackAttribs := GetAddress('TransformFeedbackAttribs');
  TransformFeedbackVaryingsNV := GetAddressNoSuffixes('TransformFeedbackVaryingsNV');
  TexBuffer := GetAddress('TexBuffer');
  BindVertexArray := GetAddress('BindVertexArray');
  DeleteVertexArrays := GetAddress('DeleteVertexArrays');
  GenVertexArrays := GetAddress('GenVertexArrays');
  IsVertexArray := GetAddress('IsVertexArray');
  FlushVertexArrayRangeNV := GetAddressNoSuffixes('FlushVertexArrayRangeNV');
  VertexArrayRangeNV := GetAddressNoSuffixes('VertexArrayRangeNV');
  CopyBufferSubData := GetAddress('CopyBufferSubData');
  UniformBuffer := GetAddress('UniformBuffer');
  GetUniformBufferSize := GetAddress('GetUniformBufferSize');
  GetUniformOffset := GetAddress('GetUniformOffset');
  PrimitiveRestartIndex := GetAddress('PrimitiveRestartIndex');

  DrawElementsBaseVertex := GetAddress('DrawElementsBaseVertex');
  DrawRangeElementsBaseVertex := GetAddress('DrawRangeElementsBaseVertex');
  DrawElementsInstancedBaseVertex :=
    GetAddress('DrawElementsInstancedBaseVertex');
  MultiDrawElementsBaseVertex := GetAddress('MultiDrawElementsBaseVertex');
  DrawArraysInstanced := GetAddress('DrawArraysInstanced');
  DrawElementsInstanced := GetAddress('DrawElementsInstanced');

  VertexAttrib1d := GetAddress('VertexAttrib1d');
  VertexAttrib1dv := GetAddress('VertexAttrib1dv');
  VertexAttrib1f := GetAddress('VertexAttrib1f');
  VertexAttrib1fv := GetAddress('VertexAttrib1fv');
  VertexAttrib1s := GetAddress('VertexAttrib1s');
  VertexAttrib1sv := GetAddress('VertexAttrib1sv');
  VertexAttrib2d := GetAddress('VertexAttrib2d');
  VertexAttrib2dv := GetAddress('VertexAttrib2dv');
  VertexAttrib2f := GetAddress('VertexAttrib2f');
  VertexAttrib2fv := GetAddress('VertexAttrib2fv');
  VertexAttrib2s := GetAddress('VertexAttrib2s');
  VertexAttrib2sv := GetAddress('VertexAttrib2sv');
  VertexAttrib3d := GetAddress('VertexAttrib3d');
  VertexAttrib3dv := GetAddress('VertexAttrib3dv');
  VertexAttrib3f := GetAddress('VertexAttrib3f');
  VertexAttrib3fv := GetAddress('VertexAttrib3fv');
  VertexAttrib3s := GetAddress('VertexAttrib3s');
  VertexAttrib3sv := GetAddress('VertexAttrib3sv');
  VertexAttrib4Nbv := GetAddress('VertexAttrib4Nbv');
  VertexAttrib4Niv := GetAddress('VertexAttrib4Niv');
  VertexAttrib4Nsv := GetAddress('VertexAttrib4Nsv');
  VertexAttrib4Nub := GetAddress('VertexAttrib4Nub');
  VertexAttrib4Nubv := GetAddress('VertexAttrib4Nubv');
  VertexAttrib4Nuiv := GetAddress('VertexAttrib4Nuiv');
  VertexAttrib4Nusv := GetAddress('VertexAttrib4Nusv');
  VertexAttrib4bv := GetAddress('VertexAttrib4bv');
  VertexAttrib4d := GetAddress('VertexAttrib4d');
  VertexAttrib4dv := GetAddress('VertexAttrib4dv');
  VertexAttrib4f := GetAddress('VertexAttrib4f');
  VertexAttrib4fv := GetAddress('VertexAttrib4fv');
  VertexAttrib4iv := GetAddress('VertexAttrib4iv');
  VertexAttrib4s := GetAddress('VertexAttrib4s');
  VertexAttrib4sv := GetAddress('VertexAttrib4sv');
  VertexAttrib4ubv := GetAddress('VertexAttrib4ubv');
  VertexAttrib4uiv := GetAddress('VertexAttrib4uiv');
  VertexAttrib4usv := GetAddress('VertexAttrib4usv');
  VertexAttribPointer := GetAddress('VertexAttribPointer');
  VertexAttribI1i := GetAddress('VertexAttribI1i');
  VertexAttribI2i := GetAddress('VertexAttribI2i');
  VertexAttribI3i := GetAddress('VertexAttribI3i');
  VertexAttribI4i := GetAddress('VertexAttribI4i');
  VertexAttribI1ui := GetAddress('VertexAttribI1ui');
  VertexAttribI2ui := GetAddress('VertexAttribI2ui');
  VertexAttribI3ui := GetAddress('VertexAttribI3ui');
  VertexAttribI4ui := GetAddress('VertexAttribI4ui');
  VertexAttribI1iv := GetAddress('VertexAttribI1iv');
  VertexAttribI2iv := GetAddress('VertexAttribI2iv');
  VertexAttribI3iv := GetAddress('VertexAttribI3iv');
  VertexAttribI4iv := GetAddress('VertexAttribI4iv');
  VertexAttribI1uiv := GetAddress('VertexAttribI1uiv');
  VertexAttribI2uiv := GetAddress('VertexAttribI2uiv');
  VertexAttribI3uiv := GetAddress('VertexAttribI3uiv');
  VertexAttribI4uiv := GetAddress('VertexAttribI4uiv');
  VertexAttribI4bv := GetAddress('VertexAttribI4bv');
  VertexAttribI4sv := GetAddress('VertexAttribI4sv');
  VertexAttribI4ubv := GetAddress('VertexAttribI4ubv');
  VertexAttribI4usv := GetAddress('VertexAttribI4usv');
  VertexAttribIPointer := GetAddress('VertexAttribIPointer');
  GetVertexAttribIiv := GetAddress('GetVertexAttribIiv');
  GetVertexAttribIuiv := GetAddress('GetVertexAttribIuiv');
  EnableVertexAttribArray := GetAddress('EnableVertexAttribArray');
  DisableVertexAttribArray := GetAddress('DisableVertexAttribArray');
  VertexAttribDivisor := GetAddress('VertexAttribDivisor');
  GenQueries := GetAddress('GenQueries');
  DeleteQueries := GetAddress('DeleteQueries');
  IsQuery := GetAddress('IsQuery');
  BeginQuery := GetAddress('BeginQuery');
  EndQuery := GetAddress('EndQuery');
  GetQueryiv := GetAddress('GetQueryiv');
  GetQueryObjectiv := GetAddress('GetQueryObjectiv');
  GetQueryObjectuiv := GetAddress('GetQueryObjectuiv');
  QueryCounter := GetAddress('QueryCounter');
  GetQueryObjecti64v := GetAddress('GetQueryObjecti64v');
  GetQueryObjectui64v := GetAddress('GetQueryObjectui64v');
  DeleteObject := GetAddress('DeleteObject');
  GetHandle := GetAddress('GetHandle');
  DetachShader := GetAddressAlt('DetachShader', 'DetachObject');
  CreateShader := GetAddressAlt('CreateShader', 'CreateShaderObject');
  ShaderSource := GetAddress('ShaderSource');
  CompileShader := GetAddress('CompileShader');
  CreateProgram := GetAddressAlt('CreateProgram', 'CreateProgramObject');
  AttachShader := GetAddressAlt('AttachShader', 'AttachObject');
  LinkProgram := GetAddress('LinkProgram');
  UseProgram := GetAddressAlt('UseProgram', 'UseProgramObject');
  ValidateProgram := GetAddress('ValidateProgram');
  Uniform1f := GetAddress('Uniform1f');
  Uniform2f := GetAddress('Uniform2f');
  Uniform3f := GetAddress('Uniform3f');
  Uniform4f := GetAddress('Uniform4f');
  Uniform1i := GetAddress('Uniform1i');
  Uniform2i := GetAddress('Uniform2i');
  Uniform3i := GetAddress('Uniform3i');
  Uniform4i := GetAddress('Uniform4i');
  Uniform1fv := GetAddress('Uniform1fv');
  Uniform2fv := GetAddress('Uniform2fv');
  Uniform3fv := GetAddress('Uniform3fv');
  Uniform4fv := GetAddress('Uniform4fv');
  Uniform1iv := GetAddress('Uniform1iv');
  Uniform2iv := GetAddress('Uniform2iv');
  Uniform3iv := GetAddress('Uniform3iv');
  Uniform4iv := GetAddress('Uniform4iv');
  Uniform1ui := GetAddress('Uniform1ui');
  Uniform2ui := GetAddress('Uniform2ui');
  Uniform3ui := GetAddress('Uniform3ui');
  Uniform4ui := GetAddress('Uniform4ui');
  Uniform1uiv := GetAddress('Uniform1uiv');
  Uniform2uiv := GetAddress('Uniform2uiv');
  Uniform3uiv := GetAddress('Uniform3uiv');
  Uniform4uiv := GetAddress('Uniform4uiv');
  GetUniformuiv := GetAddress('GetUniformuiv');
  UniformMatrix2fv := GetAddress('UniformMatrix2fv');
  UniformMatrix3fv := GetAddress('UniformMatrix3fv');
  UniformMatrix4fv := GetAddress('UniformMatrix4fv');
  BindFragDataLocation := GetAddress('BindFragDataLocation');
  GetFragDataLocation := GetAddress('GetFragDataLocation');
  ClampColor := GetAddress('ClampColor');
  ColorMaski := GetAddress('ColorMaski');
  GetBooleani_v := GetAddress('GetBooleani_v');
  GetIntegeri_v := GetAddress('GetIntegeri_v');
  Enablei := GetAddress('Enablei');
  Disablei := GetAddress('Disablei');
  IsEnabledi := GetAddress('IsEnabledi');
  BindFragDataLocationIndexed := GetAddress('BindFragDataLocationIndexed');
  GetFragDataIndex := GetAddress('GetFragDataIndex');
  GetObjectParameterfv := GetAddress('GetObjectParameterfv');
  GetObjectParameteriv := GetAddress('GetObjectParameteriv');
  GetAttachedObjects := GetAddress('GetAttachedObjects');
  GetActiveAttrib := GetAddress('GetActiveAttrib');
  GetActiveUniform := GetAddress('GetActiveUniform');
  GetAttachedShaders := GetAddress('GetAttachedShaders');
  GetAttribLocation := GetAddress('GetAttribLocation');
  GetProgramiv := GetAddressAlt('GetProgramiv', 'GetObjectParameteriv');
  GetProgramInfoLog := GetAddress('GetProgramInfoLog');
  GetShaderiv := GetAddressAlt('GetShaderiv', 'GetObjectParameteriv');
  GetInfoLog := GetAddress('GetInfoLog');
  GetShaderInfoLog := GetAddress('GetShaderInfoLog');
  GetShaderSource := GetAddress('GetShaderSource');
  GetUniformLocation := GetAddress('GetUniformLocation');
  GetUniformfv := GetAddress('GetUniformfv');
  GetUniformiv := GetAddress('GetUniformiv');
  GetVertexAttribdv := GetAddress('GetVertexAttribdv');
  GetVertexAttribfv := GetAddress('GetVertexAttribfv');
  GetVertexAttribiv := GetAddress('GetVertexAttribiv');
  GetVertexAttribPointerv := GetAddress('GetVertexAttribPointerv');
  IsProgram := GetAddress('IsProgram');
  IsShader := GetAddress('IsShader');
  GetUniformLocation := GetAddress('GetUniformLocation');
  BindAttribLocation := GetAddress('BindAttribLocation');
  GetVaryingLocation := GetAddress('GetVaryingLocation');
  GetActiveVarying := GetAddress('GetActiveVarying');
  ActiveVarying := GetAddress('ActiveVarying');
  GetUniformIndices := GetAddress('GetUniformIndices');
  GetActiveUniformsiv := GetAddress('GetActiveUniformsiv');
  GetActiveUniformName := GetAddress('GetActiveUniformName');
  GetUniformBlockIndex := GetAddress('GetUniformBlockIndex');
  GetActiveUniformBlockiv := GetAddress('GetActiveUniformBlockiv');
  GetActiveUniformBlockName := GetAddress('GetActiveUniformBlockName');
  UniformBlockBinding := GetAddress('UniformBlockBinding');
  GetProgramBinary := GetAddress('GetProgramBinary');
  ProgramBinary := GetAddress('ProgramBinary');
  UseProgramStages := GetAddress('UseProgramStages');
  ActiveShaderProgram := GetAddress('ActiveShaderProgram');
  CreateShaderProgramv := GetAddress('CreateShaderProgramv');
  BindProgramPipeline := GetAddress('BindProgramPipeline');
  DeleteProgramPipelines := GetAddress('DeleteProgramPipelines');
  GenProgramPipelines := GetAddress('GenProgramPipelines');
  IsProgramPipeline := GetAddress('IsProgramPipeline');
  GetProgramPipelineiv := GetAddress('GetProgramPipelineiv');
  ProgramUniform1i := GetAddress('ProgramUniform1i');
  ProgramUniform1iv := GetAddress('ProgramUniform1iv');
  ProgramUniform1f := GetAddress('ProgramUniform1f');
  ProgramUniform1fv := GetAddress('ProgramUniform1fv');
  ProgramUniform1d := GetAddress('ProgramUniform1d');
  ProgramUniform1dv := GetAddress('ProgramUniform1dv');
  ProgramUniform1ui := GetAddress('ProgramUniform1ui');
  ProgramUniform1uiv := GetAddress('ProgramUniform1uiv');
  ProgramUniform2i := GetAddress('ProgramUniform2i');
  ProgramUniform2iv := GetAddress('ProgramUniform2iv');
  ProgramUniform2f := GetAddress('ProgramUniform2f');
  ProgramUniform2fv := GetAddress('ProgramUniform2fv');
  ProgramUniform2d := GetAddress('ProgramUniform2d');
  ProgramUniform2dv := GetAddress('ProgramUniform2dv');
  ProgramUniform2ui := GetAddress('ProgramUniform2ui');
  ProgramUniform2uiv := GetAddress('ProgramUniform2uiv');
  ProgramUniform3i := GetAddress('ProgramUniform3i');
  ProgramUniform3iv := GetAddress('ProgramUniform3iv');
  ProgramUniform3f := GetAddress('ProgramUniform3f');
  ProgramUniform3fv := GetAddress('ProgramUniform3fv');
  ProgramUniform3d := GetAddress('ProgramUniform3d');
  ProgramUniform3dv := GetAddress('ProgramUniform3dv');
  ProgramUniform3ui := GetAddress('ProgramUniform3ui');
  ProgramUniform3uiv := GetAddress('ProgramUniform3uiv');
  ProgramUniform4i := GetAddress('ProgramUniform4i');
  ProgramUniform4iv := GetAddress('ProgramUniform4iv');
  ProgramUniform4f := GetAddress('ProgramUniform4f');
  ProgramUniform4fv := GetAddress('ProgramUniform4fv');
  ProgramUniform4d := GetAddress('ProgramUniform4d');
  ProgramUniform4dv := GetAddress('ProgramUniform4dv');
  ProgramUniform4ui := GetAddress('ProgramUniform4ui');
  ProgramUniform4uiv := GetAddress('ProgramUniform4uiv');
  ProgramUniformMatrix2fv := GetAddress('ProgramUniformMatrix2fv');
  ProgramUniformMatrix3fv := GetAddress('ProgramUniformMatrix3fv');
  ProgramUniformMatrix4fv := GetAddress('ProgramUniformMatrix4fv');
  ProgramUniformMatrix2dv := GetAddress('ProgramUniformMatrix2dv');
  ProgramUniformMatrix3dv := GetAddress('ProgramUniformMatrix3dv');
  ProgramUniformMatrix4dv := GetAddress('ProgramUniformMatrix4dv');
  ProgramUniformMatrix2x3fv := GetAddress('ProgramUniformMatrix2x3fv');
  ProgramUniformMatrix3x2fv := GetAddress('ProgramUniformMatrix3x2fv');
  ProgramUniformMatrix2x4fv := GetAddress('ProgramUniformMatrix2x4fv');
  ProgramUniformMatrix4x2fv := GetAddress('ProgramUniformMatrix4x2fv');
  ProgramUniformMatrix3x4fv := GetAddress('ProgramUniformMatrix3x4fv');
  ProgramUniformMatrix4x3fv := GetAddress('ProgramUniformMatrix4x3fv');
  ProgramUniformMatrix2x3dv := GetAddress('ProgramUniformMatrix2x3dv');
  ProgramUniformMatrix3x2dv := GetAddress('ProgramUniformMatrix3x2dv');
  ProgramUniformMatrix2x4dv := GetAddress('ProgramUniformMatrix2x4dv');
  ProgramUniformMatrix4x2dv := GetAddress('ProgramUniformMatrix4x2dv');
  ProgramUniformMatrix3x4dv := GetAddress('ProgramUniformMatrix3x4dv');
  ProgramUniformMatrix4x3dv := GetAddress('ProgramUniformMatrix4x3dv');
  ValidateProgramPipeline := GetAddress('ValidateProgramPipeline');
  GetProgramPipelineInfoLog := GetAddress('GetProgramPipelineInfoLog');
  BlendEquationSeparate := GetAddress('BlendEquationSeparate');
  DrawBuffers := GetAddress('DrawBuffers');
  StencilOpSeparate := GetAddress('StencilOpSeparate');
  StencilFuncSeparate := GetAddress('StencilFuncSeparate');
  StencilMaskSeparate := GetAddress('StencilMaskSeparate');
  ActiveTexture := GetAddress('ActiveTexture');
  CompressedTexImage3D := GetAddress('CompressedTexImage3D');
  CompressedTexImage2D := GetAddress('CompressedTexImage2D');
  CompressedTexImage1D := GetAddress('CompressedTexImage1D');
  CompressedTexSubImage3D := GetAddress('CompressedTexSubImage3D');
  CompressedTexSubImage2D := GetAddress('CompressedTexSubImage2D');
  CompressedTexSubImage1D := GetAddress('CompressedTexSubImage1D');
  GetCompressedTexImage := GetAddress('GetCompressedTexImage');
  ClientActiveTexture := GetAddress('ClientActiveTexture');
  MultiTexCoord1d := GetAddress('MultiTexCoord1d');
  MultiTexCoord1dV := GetAddress('MultiTexCoord1dv');
  MultiTexCoord1f := GetAddress('MultiTexCoord1f');
  MultiTexCoord1fv := GetAddress('MultiTexCoord1fv');
  MultiTexCoord1i := GetAddress('MultiTexCoord1i');
  MultiTexCoord1iv := GetAddress('MultiTexCoord1iv');
  MultiTexCoord1s := GetAddress('MultiTexCoord1s');
  MultiTexCoord1sv := GetAddress('MultiTexCoord1sv');
  MultiTexCoord2d := GetAddress('MultiTexCoord2d');
  MultiTexCoord2dv := GetAddress('MultiTexCoord2dv');
  MultiTexCoord2f := GetAddress('MultiTexCoord2f');
  MultiTexCoord2fv := GetAddress('MultiTexCoord2fv');
  MultiTexCoord2i := GetAddress('MultiTexCoord2i');
  MultiTexCoord2iv := GetAddress('MultiTexCoord2iv');
  MultiTexCoord2s := GetAddress('MultiTexCoord2s');
  MultiTexCoord2sv := GetAddress('MultiTexCoord2sv');
  MultiTexCoord3d := GetAddress('MultiTexCoord3d');
  MultiTexCoord3dv := GetAddress('MultiTexCoord3dv');
  MultiTexCoord3f := GetAddress('MultiTexCoord3f');
  MultiTexCoord3fv := GetAddress('MultiTexCoord3fv');
  MultiTexCoord3i := GetAddress('MultiTexCoord3i');
  MultiTexCoord3iv := GetAddress('MultiTexCoord3iv');
  MultiTexCoord3s := GetAddress('MultiTexCoord3s');
  MultiTexCoord3sv := GetAddress('MultiTexCoord3sv');
  MultiTexCoord4d := GetAddress('MultiTexCoord4d');
  MultiTexCoord4dv := GetAddress('MultiTexCoord4dv');
  MultiTexCoord4f := GetAddress('MultiTexCoord4f');
  MultiTexCoord4fv := GetAddress('MultiTexCoord4fv');
  MultiTexCoord4i := GetAddress('MultiTexCoord4i');
  MultiTexCoord4iv := GetAddress('MultiTexCoord4iv');
  MultiTexCoord4s := GetAddress('MultiTexCoord4s');
  MultiTexCoord4sv := GetAddress('MultiTexCoord4sv');
  GetInteger64i_v := GetAddress('GetInteger64i_v');
  GetBufferParameteri64v := GetAddress('GetBufferParameteri64v');
  ProgramParameteri := GetAddress('ProgramParameteri');
  ProgramString := GetAddress('ProgramString');
  BindProgram := GetAddress('BindProgram');
  DeletePrograms := GetAddress('DeletePrograms');
  GenPrograms := GetAddress('GenPrograms');
  ProgramEnvParameter4d := GetAddress('ProgramEnvParameter4d');
  ProgramEnvParameter4dv := GetAddress('ProgramEnvParameter4dv');
  ProgramEnvParameter4f := GetAddress('ProgramEnvParameter4f');
  ProgramEnvParameter4fv := GetAddress('ProgramEnvParameter4fv');
  ProgramLocalParameter4d := GetAddress('ProgramLocalParameter4d');
  ProgramLocalParameter4dv := GetAddress('ProgramLocalParameter4dv');
  ProgramLocalParameter4f := GetAddress('ProgramLocalParameter4f');
  ProgramLocalParameter4fv := GetAddress('ProgramLocalParameter4fv');
  GetProgramEnvParameterdv := GetAddress('GetProgramEnvParameterdv');
  GetProgramEnvParameterfv := GetAddress('GetProgramEnvParameterfv');
  GetProgramLocalParameterdv := GetAddress('GetProgramLocalParameterdv');
  GetProgramLocalParameterfv := GetAddress('GetProgramLocalParameterfv');
  ClearColorIi := GetAddress('ClearColorIi');
  ClearColorIui := GetAddress('ClearColorIui');
  TexParameterIiv := GetAddress('TexParameterIiv');
  TexParameterIuiv := GetAddress('TexParameterIuiv');
  GetTexParameterIiv := GetAddress('GetTexParameterIiv');
  GetTexParameterIuiv := GetAddress('GetTexParameterIuiv');
  PatchParameteri := GetAddress('PatchParameteri');
  PatchParameterfv := GetAddress('PatchParameterfv');
  BufferAddressRangeNV := GetAddressNoSuffixes('BufferAddressRangeNV');
  VertexFormatNV := GetAddressNoSuffixes('VertexFormatNV');
  NormalFormatNV := GetAddressNoSuffixes('NormalFormatNV');
  ColorFormatNV := GetAddressNoSuffixes('ColorFormatNV');
  IndexFormatNV := GetAddressNoSuffixes('IndexFormatNV');
  TexCoordFormatNV := GetAddressNoSuffixes('TexCoordFormatNV');
  EdgeFlagFormatNV := GetAddressNoSuffixes('EdgeFlagFormatNV');
  SecondaryColorFormatNV := GetAddressNoSuffixes('SecondaryColorFormatNV');
  FogCoordFormatNV := GetAddressNoSuffixes('FogCoordFormatNV');
  VertexAttribFormatNV := GetAddressNoSuffixes('VertexAttribFormatNV');
  VertexAttribIFormatNV := GetAddressNoSuffixes('VertexAttribIFormatNV');
  GetIntegerui64i_vNV := GetAddressNoSuffixes('GetIntegerui64i_vNV');
  GetBufferParameterui64vNV := GetAddressNoSuffixes('GetBufferParameterui64vNV');
  MakeBufferResidentNV := GetAddressNoSuffixes('MakeBufferResidentNV');
  MakeBufferNonResidentNV := GetAddressNoSuffixes('MakeBufferNonResidentNV');
  IsBufferResidentNV := GetAddressNoSuffixes('IsBufferResidentNV');
  MakeNamedBufferResidentNV := GetAddressNoSuffixes('MakeNamedBufferResidentNV');
  MakeNamedBufferNonResidentNV := GetAddressNoSuffixes('MakeNamedBufferNonResidentNV');
  IsNamedBufferResidentNV := GetAddressNoSuffixes('IsNamedBufferResidentNV');
  GetNamedBufferParameterui64vNV := GetAddressNoSuffixes('GetNamedBufferParameterui64vNV');
  GetIntegerui64vNV := GetAddressNoSuffixes('GetIntegerui64vNV');
  Uniformui64NV := GetAddressNoSuffixes('Uniformui64NV');
  Uniformui64vNV := GetAddressNoSuffixes('Uniformui64vNV');
  GetUniformui64vNV := GetAddressNoSuffixes('GetUniformui64vNV');
  ProgramUniformui64NV := GetAddressNoSuffixes('ProgramUniformui64NV');
  ProgramUniformui64vNV := GetAddressNoSuffixes('ProgramUniformui64vNV');
  TexImage2DMultisample := GetAddress('TexImage2DMultisample');
  TexImage3DMultisample := GetAddress('TexImage3DMultisample');
  GetMultisamplefv := GetAddress('GetMultisamplefv');
  SampleMaski := GetAddress('SampleMaski');
  ProvokingVertex := GetAddress('ProvokingVertex');
  FenceSync := GetAddress('FenceSync');
  IsSync := GetAddress('IsSync');
  DeleteSync := GetAddress('DeleteSync');
  ClientWaitSync := GetAddress('ClientWaitSync');
  WaitSync := GetAddress('WaitSync');
  GetInteger64v := GetAddress('GetInteger64v');
  GetSynciv := GetAddress('GetSynciv');
  BlendEquationi := GetAddress('BlendEquationi');
  BlendEquationSeparatei := GetAddress('BlendEquationSeparatei');
  BlendFunci := GetAddress('BlendFunci');
  BlendFuncSeparatei := GetAddress('BlendFuncSeparatei');
  MinSampleShading := GetAddress('MinSampleShading');
  GenSamplers := GetAddress('GenSamplers');
  DeleteSamplers := GetAddress('DeleteSamplers');
  IsSampler := GetAddress('IsSampler');
  BindSampler := GetAddress('BindSampler');
  SamplerParameteri := GetAddress('SamplerParameteri');
  SamplerParameteriv := GetAddress('SamplerParameteriv');
  SamplerParameterf := GetAddress('SamplerParameterf');
  SamplerParameterfv := GetAddress('SamplerParameterfv');
  SamplerParameterIiv := GetAddress('SamplerParameterIiv');
  SamplerParameterIuiv := GetAddress('SamplerParameterIuiv');
  GetSamplerParameteriv := GetAddress('GetSamplerParameteriv');
  GetSamplerParameterIiv := GetAddress('GetSamplerParameterIiv');
  GetSamplerParameterfv := GetAddress('GetSamplerParameterfv');
  GetSamplerParameterIfv := GetAddress('GetSamplerParameterIfv');
  ClientAttribDefault := GetAddress('ClientAttribDefault');
  PushClientAttribDefault := GetAddress('PushClientAttribDefault');
  MatrixLoadf := GetAddress('MatrixLoadf');
  MatrixLoadd := GetAddress('MatrixLoadd');
  MatrixMultf := GetAddress('MatrixMultf');
  MatrixMultd := GetAddress('MatrixMultd');
  MatrixLoadIdentity := GetAddress('MatrixLoadIdentity');
  MatrixRotatef := GetAddress('MatrixRotatef');
  MatrixRotated := GetAddress('MatrixRotated');
  MatrixScalef := GetAddress('MatrixScalef');
  MatrixScaled := GetAddress('MatrixScaled');
  MatrixTranslatef := GetAddress('MatrixTranslatef');
  MatrixTranslated := GetAddress('MatrixTranslated');
  MatrixFrustum := GetAddress('MatrixFrustum');
  MatrixOrtho := GetAddress('MatrixOrtho');
  MatrixPop := GetAddress('MatrixPop');
  MatrixPush := GetAddress('MatrixPush');
  MatrixLoadTransposef := GetAddress('MatrixLoadTransposef');
  MatrixLoadTransposed := GetAddress('MatrixLoadTransposed');
  MatrixMultTransposef := GetAddress('MatrixMultTransposef');
  MatrixMultTransposed := GetAddress('MatrixMultTransposed');
  TextureParameterfv := GetAddress('TextureParameterfv');
  TextureParameteri := GetAddress('TextureParameteri');
  TextureParameteriv := GetAddress('TextureParameteriv');
  TextureImage1D := GetAddress('TextureImage1D');
  TextureImage2D := GetAddress('TextureImage2D');
  TextureSubImage1D := GetAddress('TextureSubImage1D');
  TextureSubImage2D := GetAddress('TextureSubImage2D');
  CopyTextureImage1D := GetAddress('CopyTextureImage1D');
  CopyTextureImage2D := GetAddress('CopyTextureImage2D');
  CopyTextureSubImage1D := GetAddress('CopyTextureSubImage1D');
  CopyTextureSubImage2D := GetAddress('CopyTextureSubImage2D');
  GetTextureImage := GetAddress('GetTextureImage');
  GetTextureParameterfv := GetAddress('GetTextureParameterfv');
  GetTextureParameteriv := GetAddress('GetTextureParameteriv');
  GetTextureLevelParameterfv := GetAddress('GetTextureLevelParameterfv');
  GetTextureLevelParameteriv := GetAddress('GetTextureLevelParameteriv');
  TextureImage3D := GetAddress('TextureImage3D');
  TextureSubImage3D := GetAddress('TextureSubImage3D');
  CopyTextureSubImage3D := GetAddress('CopyTextureSubImage3D');
  MultiTexParameterf := GetAddress('MultiTexParameterf');
  MultiTexParameterfv := GetAddress('MultiTexParameterfv');
  MultiTexParameteri := GetAddress('MultiTexParameteri');
  MultiTexParameteriv := GetAddress('MultiTexParameteriv');
  MultiTexImage1D := GetAddress('MultiTexImage1D');
  MultiTexImage2D := GetAddress('MultiTexImage2D');
  MultiTexSubImage1D := GetAddress('MultiTexSubImage1D');
  MultiTexSubImage2D := GetAddress('MultiTexSubImage2D');
  CopyMultiTexImage1D := GetAddress('CopyMultiTexImage1D');
  CopyMultiTexImage2D := GetAddress('CopyMultiTexImage2D');
  CopyMultiTexSubImage1D := GetAddress('CopyMultiTexSubImage1D');
  CopyMultiTexSubImage2D := GetAddress('CopyMultiTexSubImage2D');
  GetMultiTexImage := GetAddress('GetMultiTexImage');
  GetMultiTexParameterfv := GetAddress('GetMultiTexParameterfv');
  GetMultiTexParameteriv := GetAddress('GetMultiTexParameteriv');
  GetMultiTexLevelParameterfv := GetAddress('GetMultiTexLevelParameterfv');
  GetMultiTexLevelParameteriv := GetAddress('GetMultiTexLevelParameteriv');
  MultiTexImage3D := GetAddress('MultiTexImage3D');
  MultiTexSubImage3D := GetAddress('MultiTexSubImage3D');
  CopyMultiTexSubImage3D := GetAddress('CopyMultiTexSubImage3D');
  BindMultiTexture := GetAddress('BindMultiTexture');
  EnableClientStateIndexed := GetAddress('EnableClientStateIndexed');
  DisableClientStateIndexed := GetAddress('DisableClientStateIndexed');
  MultiTexCoordPointer := GetAddress('MultiTexCoordPointer');
  MultiTexEnvf := GetAddress('MultiTexEnvf');
  MultiTexEnvfv := GetAddress('MultiTexEnvfv');
  MultiTexEnvi := GetAddress('MultiTexEnvi');
  MultiTexEnviv := GetAddress('MultiTexEnviv');
  MultiTexGend := GetAddress('MultiTexGend');
  MultiTexGendv := GetAddress('MultiTexGendv');
  MultiTexGenf := GetAddress('MultiTexGenf');
  MultiTexGenfv := GetAddress('MultiTexGenfv');
  MultiTexGeni := GetAddress('MultiTexGeni');
  MultiTexGeniv := GetAddress('MultiTexGeniv');
  GetMultiTexEnvfv := GetAddress('GetMultiTexEnvfv');
  GetMultiTexEnviv := GetAddress('GetMultiTexEnviv');
  GetMultiTexGendv := GetAddress('GetMultiTexGendv');
  GetMultiTexGenfv := GetAddress('GetMultiTexGenfv');
  GetMultiTexGeniv := GetAddress('GetMultiTexGeniv');
  GetFloatIndexedv := GetAddress('GetFloatIndexedv');
  GetDoubleIndexedv := GetAddress('GetDoubleIndexedv');
  GetPointerIndexedv := GetAddress('GetPointerIndexedv');
  CompressedTextureImage3D := GetAddress('CompressedTextureImage3D');
  CompressedTextureImage2D := GetAddress('CompressedTextureImage2D');
  CompressedTextureImage1D := GetAddress('CompressedTextureImage1D');
  CompressedTextureSubImage3D := GetAddress('CompressedTextureSubImage3D');
  CompressedTextureSubImage2D := GetAddress('CompressedTextureSubImage2D');
  CompressedTextureSubImage1D := GetAddress('CompressedTextureSubImage1D');
  GetCompressedTextureImage := GetAddress('GetCompressedTextureImage');
  CompressedMultiTexImage3D := GetAddress('CompressedMultiTexImage3D');
  CompressedMultiTexImage2D := GetAddress('CompressedMultiTexImage2D');
  CompressedMultiTexImage1D := GetAddress('CompressedMultiTexImage1D');
  CompressedMultiTexSubImage3D := GetAddress('CompressedMultiTexSubImage3D');
  CompressedMultiTexSubImage2D := GetAddress('CompressedMultiTexSubImage2D');
  CompressedMultiTexSubImage1D := GetAddress('CompressedMultiTexSubImage1D');
  GetCompressedMultiTexImage := GetAddress('GetCompressedMultiTexImage');
  NamedProgramString := GetAddress('NamedProgramString');
  NamedProgramLocalParameter4d := GetAddress('NamedProgramLocalParameter4d');
  NamedProgramLocalParameter4dv := GetAddress('NamedProgramLocalParameter4dv');
  NamedProgramLocalParameter4f := GetAddress('NamedProgramLocalParameter4f');
  NamedProgramLocalParameter4fv := GetAddress('NamedProgramLocalParameter4fv');
  GetNamedProgramLocalParameterdv := GetAddress('GetNamedProgramLocalParameterdv');
  GetNamedProgramLocalParameterfv := GetAddress('GetNamedProgramLocalParameterfv');
  GetNamedProgramiv := GetAddress('GetNamedProgramiv');
  GetNamedProgramString := GetAddress('GetNamedProgramString');
  NamedProgramLocalParameters4fv := GetAddress('NamedProgramLocalParameters4fv');
  NamedProgramLocalParameterI4i := GetAddress('NamedProgramLocalParameterI4i');
  NamedProgramLocalParameterI4iv := GetAddress('NamedProgramLocalParameterI4iv');
  NamedProgramLocalParametersI4iv := GetAddress('NamedProgramLocalParametersI4iv');
  NamedProgramLocalParameterI4ui := GetAddress('NamedProgramLocalParameterI4ui');
  NamedProgramLocalParameterI4uiv := GetAddress('NamedProgramLocalParameterI4uiv');
  NamedProgramLocalParametersI4uiv := GetAddress('NamedProgramLocalParametersI4uiv');
  GetNamedProgramLocalParameterIiv := GetAddress('GetNamedProgramLocalParameterIiv');
  GetNamedProgramLocalParameterIuiv := GetAddress('GetNamedProgramLocalParameterIuiv');
  TextureParameterIiv := GetAddress('TextureParameterIiv');
  TextureParameterIuiv := GetAddress('TextureParameterIuiv');
  GetTextureParameterIiv := GetAddress('GetTextureParameterIiv');
  GetTextureParameterIuiv := GetAddress('GetTextureParameterIuiv');
  MultiTexParameterIiv := GetAddress('MultiTexParameterIiv');
  MultiTexParameterIuiv := GetAddress('MultiTexParameterIuiv');
  GetMultiTexParameterIiv := GetAddress('GetMultiTexParameterIiv');
  GetMultiTexParameterIuiv := GetAddress('GetMultiTexParameterIuiv');
  NamedBufferData := GetAddress('NamedBufferData');
  NamedBufferSubData := GetAddress('NamedBufferSubData');
  MapNamedBuffer := GetAddress('MapNamedBuffer');
  UnmapNamedBuffer := GetAddress('UnmapNamedBuffer');
  MapNamedBufferRange := GetAddress('MapNamedBufferRange');
  FlushMappedNamedBufferRange := GetAddress('FlushMappedNamedBufferRange');
  NamedCopyBufferSubData := GetAddress('NamedCopyBufferSubData');
  GetNamedBufferParameteriv := GetAddress('GetNamedBufferParameteriv');
  GetNamedBufferPointerv := GetAddress('GetNamedBufferPointerv');
  GetNamedBufferSubData := GetAddress('GetNamedBufferSubData');
  TextureBuffer := GetAddress('TextureBuffer');
  MultiTexBuffer := GetAddress('MultiTexBuffer');
  NamedRenderbufferStorage := GetAddress('NamedRenderbufferStorage');
  GetNamedRenderbufferParameteriv :=
    GetAddress('GetNamedRenderbufferParameteriv');
  CheckNamedFramebufferStatus := GetAddress('CheckNamedFramebufferStatus');
  NamedFramebufferTexture1D := GetAddress('NamedFramebufferTexture1D');
  NamedFramebufferTexture2D := GetAddress('NamedFramebufferTexture2D');
  NamedFramebufferTexture3D := GetAddress('NamedFramebufferTexture3D');
  NamedFramebufferRenderbuffer := GetAddress('NamedFramebufferRenderbuffer');
  GetNamedFramebufferAttachmentParameteriv := GetAddress('GetNamedFramebufferAttachmentParameteriv');
  GenerateTextureMipmap := GetAddress('GenerateTextureMipmap');
  GenerateMultiTexMipmap := GetAddress('GenerateMultiTexMipmap');
  FramebufferDrawBuffer := GetAddress('FramebufferDrawBuffer');
  FramebufferDrawBuffers := GetAddress('FramebufferDrawBuffers');
  FramebufferReadBuffer := GetAddress('FramebufferReadBuffer');
  GetFramebufferParameteriv := GetAddress('GetFramebufferParameteriv');
  NamedRenderbufferStorageMultisample := GetAddress('NamedRenderbufferStorageMultisample');
  NamedRenderbufferStorageMultisampleCoverage := GetAddress('NamedRenderbufferStorageMultisampleCoverage');
  NamedFramebufferTexture := GetAddress('NamedFramebufferTexture');
  NamedFramebufferTextureLayer := GetAddress('NamedFramebufferTextureLayer');
  NamedFramebufferTextureFace := GetAddress('NamedFramebufferTextureFace');
  TextureRenderbuffer := GetAddress('TextureRenderbuffer');
  MultiTexRenderbuffer := GetAddress('MultiTexRenderbuffer');
  FrameTerminatorGREMEDY := GetAddress('FrameTerminatorGREMEDY');
  StringMarkerGREMEDY := GetAddress('StringMarkerGREMEDY');
  DebugMessageEnableAMDX := GetAddressNoSuffixes('DebugMessageEnableAMDX');
  DebugMessageCallbackAMDX := GetAddressNoSuffixes('DebugMessageCallbackAMDX');
  DebugMessageControl := GetAddress('DebugMessageControl');
  DebugMessageInsert := GetAddress('DebugMessageInsert');
  DebugMessageCallback := GetAddress('DebugMessageCallback');
  GetDebugMessageLog := GetAddress('GetDebugMessageLog');
  PushDebugGroup := GetAddress('PushDebugGroup');
  PopDebugGroup := GetAddress('PopDebugGroup');
  ObjectLabel := GetAddress('ObjectLabel');
  GetObjectLabel := GetAddress('GetObjectLabel');
  ObjectPtrLabel := GetAddress('ObjectPtrLabel');
  GetObjectPtrLabel := GetAddress('GetObjectPtrLabel');
  ClearBufferData := GetAddress('ClearBufferData');
  ClearBufferSubData := GetAddress('ClearBufferSubData');
  ClearNamedBufferData := GetAddress('ClearNamedBufferData');
  ClearNamedBufferSubData := GetAddress('ClearNamedBufferSubData');
  DispatchCompute := GetAddress('DispatchCompute');
  DispatchComputeIndirect := GetAddress('DispatchComputeIndirect');
  CopyImageSubData := GetAddress('CopyImageSubData');
  FramebufferParameteri := GetAddress('FramebufferParameteri');
  NamedFramebufferParameteri := GetAddress('NamedFramebufferParameteri');
  GetNamedFramebufferParameteriv := GetAddress('GetNamedFramebufferParameteriv');
  GetInternalformati64v := GetAddress('GetInternalformati64v');
  InvalidateTexSubImage := GetAddress('InvalidateTexSubImage');
  InvalidateTexImage := GetAddress('InvalidateTexImage');
  InvalidateBufferSubData := GetAddress('InvalidateBufferSubData');
  InvalidateBufferData := GetAddress('InvalidateBufferData');
  InvalidateFramebuffer := GetAddress('InvalidateFramebuffer');
  InvalidateSubFramebuffer := GetAddress('InvalidateSubFramebuffer');
  MultiDrawArraysIndirect := GetAddress('MultiDrawArraysIndirect');
  MultiDrawElementsIndirect := GetAddress('MultiDrawElementsIndirect');
  GetProgramInterfaceiv := GetAddress('GetProgramInterfaceiv');
  GetProgramResourceIndex := GetAddress('GetProgramResourceIndex');
  GetProgramResourceName := GetAddress('GetProgramResourceName');
  GetProgramResourceiv := GetAddress('GetProgramResourceiv');
  GetProgramResourceLocation := GetAddress('GetProgramResourceLocation');
  GetProgramResourceLocationIndex := GetAddress('GetProgramResourceLocationIndex');
  ShaderStorageBlockBinding := GetAddress('ShaderStorageBlockBinding');
  TexBufferRange := GetAddress('TexBufferRange');
  TextureBufferRange := GetAddress('TextureBufferRange');
  TexStorage2DMultisample := GetAddress('TexStorage2DMultisample');
  TexStorage3DMultisample := GetAddress('TexStorage3DMultisample');
  TextureStorage2DMultisample := GetAddress('TextureStorage2DMultisample');
  TextureStorage3DMultisample := GetAddress('TextureStorage3DMultisample');
  BufferStorage := GetAddress('BufferStorage');
  ClearTexImage := GetAddress('ClearTexImage');
  ClearTexSubImage := GetAddress('ClearTexSubImage');
  BindBuffersBase := GetAddress('BindBuffersBase');
  BindBuffersRange := GetAddress('BindBuffersRange');
  BindTextures := GetAddress('BindTextures');
  BindSamplers := GetAddress('BindSamplers');
  BindImageTextures := GetAddress('BindImageTextures');
  BindVertexBuffers := GetAddress('BindVertexBuffers');
  TextureView := GetAddress('TextureView');
  BindVertexBuffer := GetAddress('BindVertexBuffer');
  VertexAttribFormat := GetAddress('VertexAttribFormat');
  VertexAttribIFormat := GetAddress('VertexAttribIFormat');
  VertexAttribLFormat := GetAddress('VertexAttribLFormat');
  VertexAttribBinding := GetAddress('VertexAttribBinding');
  VertexBindingDivisor := GetAddress('VertexBindingDivisor');
  VertexArrayBindVertexBuffer := GetAddress('VertexArrayBindVertexBuffer');
  VertexArrayVertexAttribFormat := GetAddress('VertexArrayVertexAttribFormat');
  VertexArrayVertexAttribIFormat := GetAddress('VertexArrayVertexAttribIFormat');
  VertexArrayVertexAttribLFormat := GetAddress('VertexArrayVertexAttribLFormat');
  VertexArrayVertexAttribBinding := GetAddress('VertexArrayVertexAttribBinding');
  VertexArrayVertexBindingDivisor := GetAddress('VertexArrayVertexBindingDivisor');
  CreateSyncFromCLevent := GetAddress('CreateSyncFromCLevent');
  GenPathsNV := GetAddressNoSuffixes('GenPathsNV');
  DeletePathsNV := GetAddressNoSuffixes('DeletePathsNV');
  IsPathNV := GetAddressNoSuffixes('IsPathNV');
  PathCommandsNV := GetAddressNoSuffixes('PathCommandsNV');
  PathCoordsNV := GetAddressNoSuffixes('PathCoordsNV');
  PathSubCommandsNV := GetAddressNoSuffixes('PathSubCommandsNV');
  PathSubCoordsNV := GetAddressNoSuffixes('PathSubCoordsNV');
  PathStringNV := GetAddressNoSuffixes('PathStringNV');
  PathGlyphsNV := GetAddressNoSuffixes('PathGlyphsNV');
  PathGlyphRangeNV := GetAddressNoSuffixes('PathGlyphRangeNV');
  WeightPathsNV := GetAddressNoSuffixes('WeightPathsNV');
  CopyPathNV := GetAddressNoSuffixes('CopyPathNV');
  InterpolatePathsNV := GetAddressNoSuffixes('InterpolatePathsNV');
  PathParameterivNV := GetAddressNoSuffixes('PathParameterivNV');
  PathParameteriNV := GetAddressNoSuffixes('PathParameteriNV');
  PathParameterfvNV := GetAddressNoSuffixes('PathParameterfvNV');
  PathParameterfNV := GetAddressNoSuffixes('PathParameterfNV');
  PathDashArrayNV := GetAddressNoSuffixes('PathDashArrayNV');
  PathStencilFuncNV := GetAddressNoSuffixes('PathStencilFuncNV');
  StencilFillPathNV := GetAddressNoSuffixes('StencilFillPathNV');
  StencilStrokePathNV := GetAddressNoSuffixes('StencilStrokePathNV');
  StencilFillPathInstancedNV := GetAddressNoSuffixes('StencilFillPathInstancedNV');
  StencilStrokePathInstancedNV := GetAddressNoSuffixes('StencilStrokePathInstancedNV');
  PathColorGenNV := GetAddressNoSuffixes('PathColorGenNV');
  PathTexGenNV := GetAddressNoSuffixes('PathTexGenNV');
  PathFogGenNV := GetAddressNoSuffixes('PathFogGenNV');
  CoverFillPathNV := GetAddressNoSuffixes('CoverFillPathNV');
  CoverStrokePathNV := GetAddressNoSuffixes('CoverStrokePathNV');
  CoverFillPathInstancedNV := GetAddressNoSuffixes('CoverFillPathInstancedNV');
  CoverStrokePathInstancedNV := GetAddressNoSuffixes('CoverStrokePathInstancedNV');
  GetPathParameterivNV := GetAddressNoSuffixes('GetPathParameterivNV');
  GetPathParameterfvNV := GetAddressNoSuffixes('GetPathParameterfvNV');
  GetPathCommandsNV := GetAddressNoSuffixes('GetPathCommandsNV');
  GetPathCoordsNV := GetAddressNoSuffixes('GetPathCoordsNV');
  GetPathDashArrayNV := GetAddressNoSuffixes('GetPathDashArrayNV');
  GetPathMetricsNV := GetAddressNoSuffixes('GetPathMetricsNV');
  GetPathMetricRangeNV := GetAddressNoSuffixes('GetPathMetricRangeNV');
  GetPathSpacingNV := GetAddressNoSuffixes('GetPathSpacingNV');
  GetPathColorGenivNV := GetAddressNoSuffixes('GetPathColorGenivNV');
  GetPathColorGenfvNV := GetAddressNoSuffixes('GetPathColorGenfvNV');
  GetPathTexGenivNV := GetAddressNoSuffixes('GetPathTexGenivNV');
  GetPathTexGenfvNV := GetAddressNoSuffixes('GetPathTexGenfvNV');
  IsPointInFillPathNV := GetAddressNoSuffixes('IsPointInFillPathNV');
  IsPointInStrokePathNV := GetAddressNoSuffixes('IsPointInStrokePathNV');
  GetPathLengthNV := GetAddressNoSuffixes('GetPathLengthNV');
  PointAlongPathNV := GetAddressNoSuffixes('PointAlongPathNV');
  PathStencilDepthOffsetNV := GetAddressNoSuffixes('PathStencilDepthOffsetNV');
  PathCoverDepthFuncNV := GetAddressNoSuffixes('PathCoverDepthFuncNV');

  if FDebug then
    if ARB_debug_output then
    begin
      DebugMessageCallback(DebugCallBack, nil);
      DebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0,
        FDebugIds, True);
      Enable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
    end
    else if AMDX_debug_output then
    begin
      DebugMessageCallbackAMDX(DebugCallBackAMD, nil);
      DebugMessageEnableAMDX(0, 0, 0, FDebugIds, True);
    end
    else
      FDebug := False;

  SetLength(FBuffer, 0);
  FInitialized := True;
end;

procedure TGLExtensionsAndEntryPoints.Close;
begin
  if FDebug then

    if ARB_debug_output then
    begin
      DebugMessageCallback(nil, nil);
      DebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0,
        FDebugIds, False);
    end
    else if AMDX_debug_output then
    begin
      DebugMessageCallbackAMDX(nil, nil);
      DebugMessageEnableAMDX(0, 0, 0, FDebugIds, False);
    end;

  VERSION_1_0 := False;
  VERSION_1_1 := False;
  VERSION_1_2 := False;
  VERSION_1_3 := False;
  VERSION_1_4 := False;
  VERSION_1_5 := False;
  VERSION_2_0 := False;
  VERSION_2_1 := False;
  VERSION_3_0 := False;
  VERSION_3_1 := False;
  VERSION_3_2 := False;
  VERSION_3_3 := False;
  VERSION_4_0 := False;
  VERSION_4_1 := False;

  ARB_blend_func_extended := False;
  ARB_color_buffer_float := False;
  ARB_compatibility := False;
  ARB_copy_buffer := False;
  ARB_depth_buffer_float := False;
  ARB_depth_clamp := False;
  ARB_depth_texture := False;
  ARB_draw_buffers := False;
  ARB_draw_buffers_blend := False;
  ARB_draw_elements_base_vertex := False;
  ARB_draw_indirect := False;
  ARB_draw_instanced := False;
  ARB_explicit_attrib_location := False;
  ARB_fragment_coord_conventions := False;
  ARB_fragment_program := False;
  ARB_fragment_program_shadow := False;
  ARB_fragment_shader := False;
  ARB_framebuffer_object := False;
  ARB_framebuffer_sRGB := False;
  ARB_geometry_shader4 := False;
  ARB_gpu_shader_fp64 := False;
  ARB_gpu_shader5 := False;
  ARB_half_float_pixel := False;
  ARB_half_float_vertex := False;
  ARB_imaging := False;
  ARB_instanced_arrays := False;
  ARB_map_buffer_range := False;
  ARB_matrix_palette := False;
  ARB_multisample := False;
  // ' ' to avoid collision with WGL variant
  ARB_multitexture := False;
  ARB_occlusion_query := False;
  ARB_occlusion_query2 := False;
  ARB_pixel_buffer_object := False;
  ARB_point_parameters := False;
  ARB_point_sprite := False;
  ARB_provoking_vertex := False;
  ARB_sample_shading := False;
  ARB_sampler_objects := False;
  ARB_seamless_cube_map := False;
  ARB_shader_bit_encoding := False;
  ARB_shader_objects := False;
  ARB_shader_subroutine := False;
  ARB_shader_texture_lod := False;
  ARB_shading_language_100 := False;
  ARB_shadow := False;
  ARB_shadow_ambient := False;
  ARB_sync := False;
  ARB_tessellation_shader := False;
  ARB_texture_border_clamp := False;
  ARB_texture_buffer_object := False;
  ARB_texture_buffer_object_rgb32 := False;
  ARB_texture_compression := False;
  ARB_texture_compression_rgtc := False;
  ARB_texture_cube_map := False;
  ARB_texture_cube_map_array := False;
  ARB_texture_env_add := False;
  ARB_texture_env_combine := False;
  ARB_texture_env_crossbar := False;
  ARB_texture_env_dot3 := False;
  ARB_texture_float := False;
  ARB_texture_gather := False;
  ARB_texture_mirrored_repeat := False;
  ARB_texture_multisample := False;
  ARB_texture_non_power_of_two := False;
  ARB_texture_query_lod := False;
  ARB_texture_rectangle := False;
  ARB_texture_rg := False;
  ARB_texture_rgb10_a2ui := False;
  ARB_texture_swizzle := False;
  ARB_timer_query := False;
  ARB_transform_feedback2 := False;
  ARB_transform_feedback3 := False;
  ARB_transpose_matrix := False;
  ARB_uniform_buffer_object := False;
  ARB_vertex_array_bgra := False;
  ARB_vertex_array_object := False;
  ARB_vertex_blend := False;
  ARB_vertex_buffer_object := False;
  ARB_vertex_program := False;
  ARB_vertex_shader := False;
  ARB_vertex_type_2_10_10_10_rev := False;
  ARB_window_pos := False;
  ARB_texture_compression_bptc := False;
  ARB_get_program_binary := False;
  ARB_separate_shader_objects := False;
  ARB_shader_stencil_export := False;
  KHR_debug := False;
  ARB_clear_buffer_object := False;
  ARB_compute_shader := False;
  ARB_copy_image := False;
  ARB_debug_group := False;
  ARB_debug_label := False;
  ARB_debug_output2 := False;
  ARB_ES3_compatibility := False;
  ARB_explicit_uniform_location := False;
  ARB_fragment_layer_viewport := False;
  ARB_framebuffer_no_attachments := False;
  ARB_internalformat_query2 := False;
  ARB_invalidate_subdata := False;
  ARB_multi_draw_indirect := False;
  ARB_program_interface_query := False;
  ARB_shader_image_size := False;
  ARB_shader_storage_buffer_object := False;
  ARB_stencil_texturing := False;
  ARB_texture_buffer_range := False;
  ARB_texture_query_levels := False;
  ARB_texture_storage_multisample := False;
  ARB_texture_view := False;
  ARB_vertex_attrib_binding := False;
  ARB_robustness_isolation := False;
  ARB_cl_event := False;

  // check Vendor/EXT OpenGL extensions
  _3DFX_multisample := False;
  _3DFX_tbuffer := False;
  _3DFX_texture_compression_FXT1 := False;
  ATI_draw_buffers := False;
  ATI_texture_compression_3dc := False;
  ATI_texture_float := False;
  ATI_texture_mirror_once := False;

  S3_s3tc := False;

  EXT_abgr := False;
  EXT_bgra := False;
  EXT_bindable_uniform := False;
  EXT_blend_color := False;
  EXT_blend_equation_separate := False;
  EXT_blend_func_separate := False;
  EXT_blend_logic_op := False;
  EXT_blend_minmax := False;
  EXT_blend_subtract := False;
  EXT_Cg_shader := False;
  EXT_clip_volume_hint := False;
  EXT_compiled_vertex_array := False;
  EXT_copy_texture := False;
  EXT_depth_bounds_test := False;
  EXT_draw_buffers2 := False;
  EXT_draw_instanced := False;
  EXT_draw_range_elements := False;
  EXT_fog_coord := False;
  EXT_framebuffer_blit := False;
  EXT_framebuffer_multisample := False;
  EXT_framebuffer_object := False;
  EXT_framebuffer_sRGB := False;
  EXT_geometry_shader4 := False;
  EXT_gpu_program_parameters := False;
  EXT_gpu_shader4 := False;
  EXT_multi_draw_arrays := False;
  EXT_multisample := False;
  EXT_packed_depth_stencil := False;
  EXT_packed_float := False;
  EXT_packed_pixels := False;
  EXT_paletted_texture := False;
  EXT_pixel_buffer_object := False;
  EXT_polygon_offset := False;
  EXT_rescale_normal := False;
  EXT_secondary_color := False;
  EXT_separate_specular_color := False;
  EXT_shadow_funcs := False;
  EXT_shared_texture_palette := False;
  EXT_stencil_clear_tag := False;
  EXT_stencil_two_side := False;
  EXT_stencil_wrap := False;
  EXT_texture3D := False;
  EXT_texture_array := False;
  EXT_texture_buffer_object := False;
  EXT_texture_compression_latc := False;
  EXT_texture_compression_rgtc := False;
  EXT_texture_compression_s3tc := False;
  EXT_texture_cube_map := False;
  EXT_texture_edge_clamp := False;
  EXT_texture_env_add := False;
  EXT_texture_env_combine := False;
  EXT_texture_env_dot3 := False;
  EXT_texture_filter_anisotropic := False;
  EXT_texture_integer := False;
  EXT_texture_lod := False;
  EXT_texture_lod_bias := False;
  EXT_texture_mirror_clamp := False;
  EXT_texture_object := False;
  EXT_texture_rectangle := False;
  EXT_texture_sRGB := False;
  EXT_texture_shared_exponent := False;
  EXT_timer_query := False;
  EXT_transform_feedback := False;
  EXT_vertex_array := False;
  EXT_texture_sRGB_decode := False;
  EXT_direct_state_access := False;
  EXT_texture_swizzle := False;

  HP_occlusion_test := False;

  IBM_rasterpos_clip := False;

  KTX_buffer_region := False;

  MESA_resize_buffers := False;

  NV_blend_square := False;
  NV_conditional_render := False;
  NV_copy_image := False;
  NV_depth_buffer_float := False;
  NV_fence := False;
  NV_float_buffer := False;
  NV_fog_distance := False;
  NV_geometry_program4 := False;
  NV_light_max_exponent := False;
  NV_multisample_filter_hint := False;
  NV_occlusion_query := False;
  NV_point_sprite := False;
  NV_primitive_restart := False;
  NV_register_combiners := False;
  NV_shader_buffer_load := False;
  NV_texgen_reflection := False;
  NV_texture_compression_vtc := False;
  NV_texture_env_combine4 := False;
  NV_texture_rectangle := False;
  NV_texture_shader := False;
  NV_texture_shader2 := False;
  NV_texture_shader3 := False;
  NV_transform_feedback := False;
  NV_vertex_array_range := False;
  NV_vertex_array_range2 := False;
  NV_vertex_buffer_unified_memory := False;
  NV_vertex_program := False;

  SGI_color_matrix := False;

  SGIS_generate_mipmap := False;
  SGIS_multisample := False;
  SGIS_texture_border_clamp := False;
  SGIS_texture_color_mask := False;
  SGIS_texture_edge_clamp := False;
  SGIS_texture_lod := False;

  SGIX_depth_texture := False;
  SGIX_shadow := False;
  SGIX_shadow_ambient := False;

  AMD_vertex_shader_tessellator := False;

  WIN_swap_hint := False;
  ATI_meminfo := False;
  NVX_gpu_memory_info := False;
  NV_vdpau_interop := False;
  NV_path_rendering := False;

  GREMEDY_frame_terminator := False;
  GREMEDY_string_marker := False;
  ARB_debug_output := False;

  BindTexture := GetCapAddress();
  BlendFunc := GetCapAddress();
  Clear := GetCapAddress();
  ClearColor := GetCapAddress();
  ClearDepth := GetCapAddress();
  ClearStencil := GetCapAddress();
  ColorMask := GetCapAddress();
  CopyTexImage1D := GetCapAddress();
  CopyTexImage2D := GetCapAddress();
  CopyTexSubImage1D := GetCapAddress();
  CopyTexSubImage2D := GetCapAddress();
  CullFace := GetCapAddress();
  DeleteTextures := GetCapAddress();
  DepthFunc := GetCapAddress();
  DepthMask := GetCapAddress();
  DepthRange := GetCapAddress();
  Disable := GetCapAddress();
  DrawArrays := GetCapAddress();
  DrawBuffer := GetCapAddress();
  DrawElements := GetCapAddress();
  Enable := GetCapAddress();
  Finish := GetCapAddress();
  Flush := GetCapAddress();
  FrontFace := GetCapAddress();
  GenTextures := GetCapAddress();
  GetBooleanv := GetCapAddress();
  GetDoublev := GetCapAddress();
  GetError := GetCapAddress();
  GetFloatv := GetCapAddress();
  GetPointerv := GetCapAddress();
  GetString := GetCapAddress();
  GetTexImage := GetCapAddress();
  GetTexLevelParameterfv := GetCapAddress();
  GetTexLevelParameteriv := GetCapAddress();
  GetTexParameterfv := GetCapAddress();
  GetTexParameteriv := GetCapAddress();
  Hint := GetCapAddress();
  IsEnabled := GetCapAddress();
  IsTexture := GetCapAddress();
  LineWidth := GetCapAddress();
  LogicOp := GetCapAddress();
  PixelStoref := GetCapAddress();
  PixelStorei := GetCapAddress();
  PointSize := GetCapAddress();
  PolygonMode := GetCapAddress();
  PolygonOffset := GetCapAddress();
  ReadBuffer := GetCapAddress();
  ReadPixels := GetCapAddress();
  Scissor := GetCapAddress();
  StencilFunc := GetCapAddress();
  StencilMask := GetCapAddress();
  StencilOp := GetCapAddress();
  TexImage1D := GetCapAddress();
  TexImage2D := GetCapAddress();
  TexParameterf := GetCapAddress();
  TexParameterfv := GetCapAddress();
  TexParameteri := GetCapAddress();
  TexParameteriv := GetCapAddress();
  TexSubImage1D := GetCapAddress();
  TexSubImage2D := GetCapAddress();
  Viewport := GetCapAddress();
  Accum := GetCapAddress();
  AlphaFunc := GetCapAddress();
  AreTexturesResident := GetCapAddress();
  ArrayElement := GetCapAddress();
  Begin_ := GetCapAddress();
  Bitmap := GetCapAddress();
  CallList := GetCapAddress();
  CallLists := GetCapAddress();
  ClearAccum := GetCapAddress();
  ClearIndex := GetCapAddress();
  ClipPlane := GetCapAddress();
  Color3b := GetCapAddress();
  Color3bv := GetCapAddress();
  Color3d := GetCapAddress();
  Color3dv := GetCapAddress();
  Color3f := GetCapAddress();
  Color3fv := GetCapAddress();
  Color3i := GetCapAddress();
  Color3iv := GetCapAddress();
  Color3s := GetCapAddress();
  Color3sv := GetCapAddress();
  Color3ub := GetCapAddress();
  Color3ubv := GetCapAddress();
  Color3ui := GetCapAddress();
  Color3uiv := GetCapAddress();
  Color3us := GetCapAddress();
  Color3usv := GetCapAddress();
  Color4b := GetCapAddress();
  Color4bv := GetCapAddress();
  Color4d := GetCapAddress();
  Color4dv := GetCapAddress();
  Color4f := GetCapAddress();
  Color4fv := GetCapAddress();
  Color4i := GetCapAddress();
  Color4iv := GetCapAddress();
  Color4s := GetCapAddress();
  Color4sv := GetCapAddress();
  Color4ub := GetCapAddress();
  Color4ubv := GetCapAddress();
  Color4ui := GetCapAddress();
  Color4uiv := GetCapAddress();
  Color4us := GetCapAddress();
  Color4usv := GetCapAddress();
  ColorMaterial := GetCapAddress();
  ColorPointer := GetCapAddress();
  CopyPixels := GetCapAddress();
  DeleteLists := GetCapAddress();
  DisableClientState := GetCapAddress();
  DrawPixels := GetCapAddress();
  EdgeFlag := GetCapAddress();
  EdgeFlagPointer := GetCapAddress();
  EdgeFlagv := GetCapAddress();
  EnableClientState := GetCapAddress();
  End_ := GetCapAddress();
  EndList := GetCapAddress();
  EvalCoord1d := GetCapAddress();
  EvalCoord1dv := GetCapAddress();
  EvalCoord1f := GetCapAddress();
  EvalCoord1fv := GetCapAddress();
  EvalCoord2d := GetCapAddress();
  EvalCoord2dv := GetCapAddress();
  EvalCoord2f := GetCapAddress();
  EvalCoord2fv := GetCapAddress();
  EvalMesh1 := GetCapAddress();
  EvalMesh2 := GetCapAddress();
  EvalPoint1 := GetCapAddress();
  EvalPoint2 := GetCapAddress();
  FeedbackBuffer := GetCapAddress();
  Fogf := GetCapAddress();
  Fogfv := GetCapAddress();
  Fogi := GetCapAddress();
  Fogiv := GetCapAddress();
  Frustum := GetCapAddress();
  GenLists := GetCapAddress();
  GetClipPlane := GetCapAddress();
  GetLightfv := GetCapAddress();
  GetLightiv := GetCapAddress();
  GetMapdv := GetCapAddress();
  GetMapfv := GetCapAddress();
  GetMapiv := GetCapAddress();
  GetMaterialfv := GetCapAddress();
  GetMaterialiv := GetCapAddress();
  GetPixelMapfv := GetCapAddress();
  GetPixelMapuiv := GetCapAddress();
  GetPixelMapusv := GetCapAddress();
  GetPolygonStipple := GetCapAddress();
  GetTexEnvfv := GetCapAddress();
  GetTexEnviv := GetCapAddress();
  GetTexGendv := GetCapAddress();
  GetTexGenfv := GetCapAddress();
  GetTexGeniv := GetCapAddress();
  IndexMask := GetCapAddress();
  IndexPointer := GetCapAddress();
  Indexd := GetCapAddress();
  Indexdv := GetCapAddress();
  Indexf := GetCapAddress();
  Indexfv := GetCapAddress();
  Indexi := GetCapAddress();
  Indexiv := GetCapAddress();
  Indexs := GetCapAddress();
  Indexsv := GetCapAddress();
  Indexub := GetCapAddress();
  Indexubv := GetCapAddress();
  InitNames := GetCapAddress();
  InterleavedArrays := GetCapAddress();
  IsList := GetCapAddress();
  LightModelf := GetCapAddress();
  LightModelfv := GetCapAddress();
  LightModeli := GetCapAddress();
  LightModeliv := GetCapAddress();
  Lightf := GetCapAddress();
  Lightfv := GetCapAddress();
  Lighti := GetCapAddress();
  Lightiv := GetCapAddress();
  LineStipple := GetCapAddress();
  ListBase := GetCapAddress();
  LoadIdentity := GetCapAddress();
  LoadMatrixd := GetCapAddress();
  LoadMatrixf := GetCapAddress();
  LoadName := GetCapAddress();
  Map1d := GetCapAddress();
  Map1f := GetCapAddress();
  Map2d := GetCapAddress();
  Map2f := GetCapAddress();
  MapGrid1d := GetCapAddress();
  MapGrid1f := GetCapAddress();
  MapGrid2d := GetCapAddress();
  MapGrid2f := GetCapAddress();
  Materialf := GetCapAddress();
  Materialfv := GetCapAddress();
  Materiali := GetCapAddress();
  Materialiv := GetCapAddress();
  MatrixMode := GetCapAddress();
  MultMatrixd := GetCapAddress();
  MultMatrixf := GetCapAddress();
  NewList := GetCapAddress();
  Normal3b := GetCapAddress();
  Normal3bv := GetCapAddress();
  Normal3d := GetCapAddress();
  Normal3dv := GetCapAddress();
  Normal3f := GetCapAddress();
  Normal3fv := GetCapAddress();
  Normal3i := GetCapAddress();
  Normal3iv := GetCapAddress();
  Normal3s := GetCapAddress();
  Normal3sv := GetCapAddress();
  NormalPointer := GetCapAddress();
  Ortho := GetCapAddress();
  PassThrough := GetCapAddress();
  PixelMapfv := GetCapAddress();
  PixelMapuiv := GetCapAddress();
  PixelMapusv := GetCapAddress();
  PixelTransferf := GetCapAddress();
  PixelTransferi := GetCapAddress();
  PixelZoom := GetCapAddress();
  PolygonStipple := GetCapAddress();
  PopAttrib := GetCapAddress();
  PopClientAttrib := GetCapAddress();
  PopMatrix := GetCapAddress();
  PopName := GetCapAddress();
  PrioritizeTextures := GetCapAddress();
  PushAttrib := GetCapAddress();
  PushClientAttrib := GetCapAddress();
  PushMatrix := GetCapAddress();
  PushName := GetCapAddress();
  RasterPos2d := GetCapAddress();
  RasterPos2dv := GetCapAddress();
  RasterPos2f := GetCapAddress();
  RasterPos2fv := GetCapAddress();
  RasterPos2i := GetCapAddress();
  RasterPos2iv := GetCapAddress();
  RasterPos2s := GetCapAddress();
  RasterPos2sv := GetCapAddress();
  RasterPos3d := GetCapAddress();
  RasterPos3dv := GetCapAddress();
  RasterPos3f := GetCapAddress();
  RasterPos3fv := GetCapAddress();
  RasterPos3i := GetCapAddress();
  RasterPos3iv := GetCapAddress();
  RasterPos3s := GetCapAddress();
  RasterPos3sv := GetCapAddress();
  RasterPos4d := GetCapAddress();
  RasterPos4dv := GetCapAddress();
  RasterPos4f := GetCapAddress();
  RasterPos4fv := GetCapAddress();
  RasterPos4i := GetCapAddress();
  RasterPos4iv := GetCapAddress();
  RasterPos4s := GetCapAddress();
  RasterPos4sv := GetCapAddress();
  Rectd := GetCapAddress();
  Rectdv := GetCapAddress();
  Rectf := GetCapAddress();
  Rectfv := GetCapAddress();
  Recti := GetCapAddress();
  Rectiv := GetCapAddress();
  Rects := GetCapAddress();
  Rectsv := GetCapAddress();
  RenderMode := GetCapAddress();
  Rotated := GetCapAddress();
  Rotatef := GetCapAddress();
  Scaled := GetCapAddress();
  Scalef := GetCapAddress();
  SelectBuffer := GetCapAddress();
  ShadeModel := GetCapAddress();
  TexCoord1d := GetCapAddress();
  TexCoord1dv := GetCapAddress();
  TexCoord1f := GetCapAddress();
  TexCoord1fv := GetCapAddress();
  TexCoord1i := GetCapAddress();
  TexCoord1iv := GetCapAddress();
  TexCoord1s := GetCapAddress();
  TexCoord1sv := GetCapAddress();
  TexCoord2d := GetCapAddress();
  TexCoord2dv := GetCapAddress();
  TexCoord2f := GetCapAddress();
  TexCoord2fv := GetCapAddress();
  TexCoord2i := GetCapAddress();
  TexCoord2iv := GetCapAddress();
  TexCoord2s := GetCapAddress();
  TexCoord2sv := GetCapAddress();
  TexCoord3d := GetCapAddress();
  TexCoord3dv := GetCapAddress();
  TexCoord3f := GetCapAddress();
  TexCoord3fv := GetCapAddress();
  TexCoord3i := GetCapAddress();
  TexCoord3iv := GetCapAddress();
  TexCoord3s := GetCapAddress();
  TexCoord3sv := GetCapAddress();
  TexCoord4d := GetCapAddress();
  TexCoord4dv := GetCapAddress();
  TexCoord4f := GetCapAddress();
  TexCoord4fv := GetCapAddress();
  TexCoord4i := GetCapAddress();
  TexCoord4iv := GetCapAddress();
  TexCoord4s := GetCapAddress();
  TexCoord4sv := GetCapAddress();
  TexCoordPointer := GetCapAddress();
  TexEnvf := GetCapAddress();
  TexEnvfv := GetCapAddress();
  TexEnvi := GetCapAddress();
  TexEnviv := GetCapAddress();
  TexGend := GetCapAddress();
  TexGendv := GetCapAddress();
  TexGenf := GetCapAddress();
  TexGenfv := GetCapAddress();
  TexGeni := GetCapAddress();
  TexGeniv := GetCapAddress();
  Translated := GetCapAddress();
  Translatef := GetCapAddress();
  Vertex2d := GetCapAddress();
  Vertex2dv := GetCapAddress();
  Vertex2f := GetCapAddress();
  Vertex2fv := GetCapAddress();
  Vertex2i := GetCapAddress();
  Vertex2iv := GetCapAddress();
  Vertex2s := GetCapAddress();
  Vertex2sv := GetCapAddress();
  Vertex3d := GetCapAddress();
  Vertex3dv := GetCapAddress();
  Vertex3f := GetCapAddress();
  Vertex3fv := GetCapAddress();
  Vertex3i := GetCapAddress();
  Vertex3iv := GetCapAddress();
  Vertex3s := GetCapAddress();
  Vertex3sv := GetCapAddress();
  Vertex4d := GetCapAddress();
  Vertex4dv := GetCapAddress();
  Vertex4f := GetCapAddress();
  Vertex4fv := GetCapAddress();
  Vertex4i := GetCapAddress();
  Vertex4iv := GetCapAddress();
  Vertex4s := GetCapAddress();
  Vertex4sv := GetCapAddress();
  VertexPointer := GetCapAddress();
  BlendColor := GetCapAddress();
  BlendEquation := GetCapAddress();
  DrawRangeElements := GetCapAddress();
  TexImage3D := GetCapAddress();
  TexSubImage3D := GetCapAddress();
  CopyTexSubImage3D := GetCapAddress();

  IsRenderbuffer := GetCapAddress();
  BindRenderbuffer := GetCapAddress();
  DeleteRenderbuffers := GetCapAddress();
  GenRenderbuffers := GetCapAddress();
  RenderbufferStorage := GetCapAddress();
  RenderbufferStorageMultisample := GetCapAddress();
  GetRenderbufferParameteriv := GetCapAddress();
  IsFramebuffer := GetCapAddress();
  BindFramebuffer := GetCapAddress();
  DeleteFramebuffers := GetCapAddress();
  GenFramebuffers := GetCapAddress();
  CheckFramebufferStatus := GetCapAddress();
  FramebufferTexture := GetCapAddress();
  FramebufferTexture1D := GetCapAddress();
  FramebufferTexture2D := GetCapAddress();
  FramebufferTexture3D := GetCapAddress();
  FramebufferTextureLayer := GetCapAddress();
  FramebufferTextureFace := GetCapAddress();
  FramebufferRenderbuffer := GetCapAddress();
  GetFramebufferAttachmentParameteriv := GetCapAddress();
  BlitFramebuffer := GetCapAddress();
  GenerateMipmap := GetCapAddress();
  ClearBufferiv := GetCapAddress();
  ClearBufferuiv := GetCapAddress();
  ClearBufferfv := GetCapAddress();
  ClearBufferfi := GetCapAddress();
  LockArrays := GetCapAddress();
  UnlockArrays := GetCapAddress();
  BindBuffer := GetCapAddress();
  DeleteBuffers := GetCapAddress();
  GenBuffers := GetCapAddress();
  IsBuffer := GetCapAddress();
  BufferData := GetCapAddress();
  BufferSubData := GetCapAddress();
  GetBufferSubData := GetCapAddress();
  MapBuffer := GetCapAddress();
  UnmapBuffer := GetCapAddress();
  GetBufferParameteriv := GetCapAddress();
  GetBufferPointerv := GetCapAddress();
  MapBufferRange := GetCapAddress();
  FlushMappedBufferRange := GetCapAddress();
  BindBufferRange := GetCapAddress();
  BindBufferOffset := GetCapAddress();
  BindBufferBase := GetCapAddress();
  BeginTransformFeedback := GetCapAddress();
  EndTransformFeedback := GetCapAddress();
  TransformFeedbackVaryings := GetCapAddress();
  GetTransformFeedbackVarying := GetCapAddress();

  TransformFeedbackAttribs := GetCapAddress();
  TransformFeedbackVaryingsNV := GetCapAddress();
  TexBuffer := GetCapAddress();
  BindVertexArray := GetCapAddress();
  DeleteVertexArrays := GetCapAddress();
  GenVertexArrays := GetCapAddress();
  IsVertexArray := GetCapAddress();
  FlushVertexArrayRangeNV := GetCapAddress();
  VertexArrayRangeNV := GetCapAddress();
  CopyBufferSubData := GetCapAddress();
  UniformBuffer := GetCapAddress();
  GetUniformBufferSize := GetCapAddress();
  GetUniformOffset := GetCapAddress();
  PrimitiveRestartIndex := GetCapAddress();

  DrawElementsBaseVertex := GetCapAddress();
  DrawRangeElementsBaseVertex := GetCapAddress();
  DrawElementsInstancedBaseVertex := GetCapAddress();
  MultiDrawElementsBaseVertex := GetCapAddress();
  DrawArraysInstanced := GetCapAddress();
  DrawElementsInstanced := GetCapAddress();

  VertexAttrib1d := GetCapAddress();
  VertexAttrib1dv := GetCapAddress();
  VertexAttrib1f := GetCapAddress();
  VertexAttrib1fv := GetCapAddress();
  VertexAttrib1s := GetCapAddress();
  VertexAttrib1sv := GetCapAddress();
  VertexAttrib2d := GetCapAddress();
  VertexAttrib2dv := GetCapAddress();
  VertexAttrib2f := GetCapAddress();
  VertexAttrib2fv := GetCapAddress();
  VertexAttrib2s := GetCapAddress();
  VertexAttrib2sv := GetCapAddress();
  VertexAttrib3d := GetCapAddress();
  VertexAttrib3dv := GetCapAddress();
  VertexAttrib3f := GetCapAddress();
  VertexAttrib3fv := GetCapAddress();
  VertexAttrib3s := GetCapAddress();
  VertexAttrib3sv := GetCapAddress();
  VertexAttrib4Nbv := GetCapAddress();
  VertexAttrib4Niv := GetCapAddress();
  VertexAttrib4Nsv := GetCapAddress();
  VertexAttrib4Nub := GetCapAddress();
  VertexAttrib4Nubv := GetCapAddress();
  VertexAttrib4Nuiv := GetCapAddress();
  VertexAttrib4Nusv := GetCapAddress();
  VertexAttrib4bv := GetCapAddress();
  VertexAttrib4d := GetCapAddress();
  VertexAttrib4dv := GetCapAddress();
  VertexAttrib4f := GetCapAddress();
  VertexAttrib4fv := GetCapAddress();
  VertexAttrib4iv := GetCapAddress();
  VertexAttrib4s := GetCapAddress();
  VertexAttrib4sv := GetCapAddress();
  VertexAttrib4ubv := GetCapAddress();
  VertexAttrib4uiv := GetCapAddress();
  VertexAttrib4usv := GetCapAddress();
  VertexAttribPointer := GetCapAddress();
  VertexAttribI1i := GetCapAddress();
  VertexAttribI2i := GetCapAddress();
  VertexAttribI3i := GetCapAddress();
  VertexAttribI4i := GetCapAddress();
  VertexAttribI1ui := GetCapAddress();
  VertexAttribI2ui := GetCapAddress();
  VertexAttribI3ui := GetCapAddress();
  VertexAttribI4ui := GetCapAddress();
  VertexAttribI1iv := GetCapAddress();
  VertexAttribI2iv := GetCapAddress();
  VertexAttribI3iv := GetCapAddress();
  VertexAttribI4iv := GetCapAddress();
  VertexAttribI1uiv := GetCapAddress();
  VertexAttribI2uiv := GetCapAddress();
  VertexAttribI3uiv := GetCapAddress();
  VertexAttribI4uiv := GetCapAddress();
  VertexAttribI4bv := GetCapAddress();
  VertexAttribI4sv := GetCapAddress();
  VertexAttribI4ubv := GetCapAddress();
  VertexAttribI4usv := GetCapAddress();
  VertexAttribIPointer := GetCapAddress();
  GetVertexAttribIiv := GetCapAddress();
  GetVertexAttribIuiv := GetCapAddress();
  EnableVertexAttribArray := GetCapAddress();
  DisableVertexAttribArray := GetCapAddress();
  VertexAttribDivisor := GetCapAddress();

  GenQueries := GetCapAddress();
  DeleteQueries := GetCapAddress();
  IsQuery := GetCapAddress();
  BeginQuery := GetCapAddress();
  EndQuery := GetCapAddress();
  GetQueryiv := GetCapAddress();
  GetQueryObjectiv := GetCapAddress();
  GetQueryObjectuiv := GetCapAddress();
  QueryCounter := GetCapAddress();
  GetQueryObjecti64v := GetCapAddress();
  GetQueryObjectui64v := GetCapAddress();

  DeleteObject := GetCapAddress();
  GetHandle := GetCapAddress();
  DetachShader := GetCapAddress();
  CreateShader := GetCapAddress();
  ShaderSource := GetCapAddress();
  CompileShader := GetCapAddress();
  CreateProgram := GetCapAddress();
  AttachShader := GetCapAddress();
  LinkProgram := GetCapAddress();
  UseProgram := GetCapAddress();
  ValidateProgram := GetCapAddress();
  Uniform1f := GetCapAddress();
  Uniform2f := GetCapAddress();
  Uniform3f := GetCapAddress();
  Uniform4f := GetCapAddress();
  Uniform1i := GetCapAddress();
  Uniform2i := GetCapAddress();
  Uniform3i := GetCapAddress();
  Uniform4i := GetCapAddress();
  Uniform1fv := GetCapAddress();
  Uniform2fv := GetCapAddress();
  Uniform3fv := GetCapAddress();
  Uniform4fv := GetCapAddress();
  Uniform1iv := GetCapAddress();
  Uniform2iv := GetCapAddress();
  Uniform3iv := GetCapAddress();
  Uniform4iv := GetCapAddress();
  Uniform1ui := GetCapAddress();
  Uniform2ui := GetCapAddress();
  Uniform3ui := GetCapAddress();
  Uniform4ui := GetCapAddress();
  Uniform1uiv := GetCapAddress();
  Uniform2uiv := GetCapAddress();
  Uniform3uiv := GetCapAddress();
  Uniform4uiv := GetCapAddress();
  GetUniformuiv := GetCapAddress();
  UniformMatrix2fv := GetCapAddress();
  UniformMatrix3fv := GetCapAddress();
  UniformMatrix4fv := GetCapAddress();
  BindFragDataLocation := GetCapAddress();
  GetFragDataLocation := GetCapAddress();
  ClampColor := GetCapAddress();
  ColorMaski := GetCapAddress();
  GetBooleani_v := GetCapAddress();
  GetIntegeri_v := GetCapAddress();
  Enablei := GetCapAddress();
  Disablei := GetCapAddress();
  IsEnabledi := GetCapAddress();
  BindFragDataLocationIndexed := GetCapAddress();
  GetFragDataIndex := GetCapAddress();
  GetObjectParameterfv := GetCapAddress();
  GetObjectParameteriv := GetCapAddress();
  GetAttachedObjects := GetCapAddress();
  GetActiveAttrib := GetCapAddress();
  GetActiveUniform := GetCapAddress();
  GetAttachedShaders := GetCapAddress();
  GetAttribLocation := GetCapAddress();
  GetProgramiv := GetCapAddress();
  GetProgramInfoLog := GetCapAddress();
  GetShaderiv := GetCapAddress();
  GetInfoLog := GetCapAddress();
  GetShaderInfoLog := GetCapAddress();
  GetShaderSource := GetCapAddress();
  GetUniformLocation := GetCapAddress();
  GetUniformfv := GetCapAddress();
  GetUniformiv := GetCapAddress();
  GetVertexAttribdv := GetCapAddress();
  GetVertexAttribfv := GetCapAddress();
  GetVertexAttribiv := GetCapAddress();
  GetVertexAttribPointerv := GetCapAddress();
  IsProgram := GetCapAddress();
  IsShader := GetCapAddress();
  GetUniformLocation := GetCapAddress();
  BindAttribLocation := GetCapAddress();
  GetVaryingLocation := GetCapAddress();
  GetActiveVarying := GetCapAddress();
  ActiveVarying := GetCapAddress();
  GetUniformIndices := GetCapAddress();
  GetActiveUniformsiv := GetCapAddress();
  GetActiveUniformName := GetCapAddress();
  GetUniformBlockIndex := GetCapAddress();
  GetActiveUniformBlockiv := GetCapAddress();
  GetActiveUniformBlockName := GetCapAddress();
  UniformBlockBinding := GetCapAddress();
  GetProgramBinary := GetCapAddress();
  ProgramBinary := GetCapAddress();
  UseProgramStages := GetCapAddress();
  ActiveShaderProgram := GetCapAddress();
  CreateShaderProgramv := GetCapAddress();
  BindProgramPipeline := GetCapAddress();
  DeleteProgramPipelines := GetCapAddress();
  GenProgramPipelines := GetCapAddress();
  IsProgramPipeline := GetCapAddress();
  GetProgramPipelineiv := GetCapAddress();
  ProgramUniform1i := GetCapAddress();
  ProgramUniform1iv := GetCapAddress();
  ProgramUniform1f := GetCapAddress();
  ProgramUniform1fv := GetCapAddress();
  ProgramUniform1d := GetCapAddress();
  ProgramUniform1dv := GetCapAddress();
  ProgramUniform1ui := GetCapAddress();
  ProgramUniform1uiv := GetCapAddress();
  ProgramUniform2i := GetCapAddress();
  ProgramUniform2iv := GetCapAddress();
  ProgramUniform2f := GetCapAddress();
  ProgramUniform2fv := GetCapAddress();
  ProgramUniform2d := GetCapAddress();
  ProgramUniform2dv := GetCapAddress();
  ProgramUniform2ui := GetCapAddress();
  ProgramUniform2uiv := GetCapAddress();
  ProgramUniform3i := GetCapAddress();
  ProgramUniform3iv := GetCapAddress();
  ProgramUniform3f := GetCapAddress();
  ProgramUniform3fv := GetCapAddress();
  ProgramUniform3d := GetCapAddress();
  ProgramUniform3dv := GetCapAddress();
  ProgramUniform3ui := GetCapAddress();
  ProgramUniform3uiv := GetCapAddress();
  ProgramUniform4i := GetCapAddress();
  ProgramUniform4iv := GetCapAddress();
  ProgramUniform4f := GetCapAddress();
  ProgramUniform4fv := GetCapAddress();
  ProgramUniform4d := GetCapAddress();
  ProgramUniform4dv := GetCapAddress();
  ProgramUniform4ui := GetCapAddress();
  ProgramUniform4uiv := GetCapAddress();
  ProgramUniformMatrix2fv := GetCapAddress();
  ProgramUniformMatrix3fv := GetCapAddress();
  ProgramUniformMatrix4fv := GetCapAddress();
  ProgramUniformMatrix2dv := GetCapAddress();
  ProgramUniformMatrix3dv := GetCapAddress();
  ProgramUniformMatrix4dv := GetCapAddress();
  ProgramUniformMatrix2x3fv := GetCapAddress();
  ProgramUniformMatrix3x2fv := GetCapAddress();
  ProgramUniformMatrix2x4fv := GetCapAddress();
  ProgramUniformMatrix4x2fv := GetCapAddress();
  ProgramUniformMatrix3x4fv := GetCapAddress();
  ProgramUniformMatrix4x3fv := GetCapAddress();
  ProgramUniformMatrix2x3dv := GetCapAddress();
  ProgramUniformMatrix3x2dv := GetCapAddress();
  ProgramUniformMatrix2x4dv := GetCapAddress();
  ProgramUniformMatrix4x2dv := GetCapAddress();
  ProgramUniformMatrix3x4dv := GetCapAddress();
  ProgramUniformMatrix4x3dv := GetCapAddress();
  ValidateProgramPipeline := GetCapAddress();
  GetProgramPipelineInfoLog := GetCapAddress();

  DrawBuffers := GetCapAddress();

  ActiveTexture := GetCapAddress();
  CompressedTexImage3D := GetCapAddress();
  CompressedTexImage2D := GetCapAddress();
  CompressedTexImage1D := GetCapAddress();
  CompressedTexSubImage3D := GetCapAddress();
  CompressedTexSubImage2D := GetCapAddress();
  CompressedTexSubImage1D := GetCapAddress();
  GetCompressedTexImage := GetCapAddress();
  ClientActiveTexture := GetCapAddress();
  MultiTexCoord1d := GetCapAddress();
  MultiTexCoord1dV := GetCapAddress();
  MultiTexCoord1f := GetCapAddress();
  MultiTexCoord1fv := GetCapAddress();
  MultiTexCoord1i := GetCapAddress();
  MultiTexCoord1iv := GetCapAddress();
  MultiTexCoord1s := GetCapAddress();
  MultiTexCoord1sv := GetCapAddress();
  MultiTexCoord2d := GetCapAddress();
  MultiTexCoord2dv := GetCapAddress();
  MultiTexCoord2f := GetCapAddress();
  MultiTexCoord2fv := GetCapAddress();
  MultiTexCoord2i := GetCapAddress();
  MultiTexCoord2iv := GetCapAddress();
  MultiTexCoord2s := GetCapAddress();
  MultiTexCoord2sv := GetCapAddress();
  MultiTexCoord3d := GetCapAddress();
  MultiTexCoord3dv := GetCapAddress();
  MultiTexCoord3f := GetCapAddress();
  MultiTexCoord3fv := GetCapAddress();
  MultiTexCoord3i := GetCapAddress();
  MultiTexCoord3iv := GetCapAddress();
  MultiTexCoord3s := GetCapAddress();
  MultiTexCoord3sv := GetCapAddress();
  MultiTexCoord4d := GetCapAddress();
  MultiTexCoord4dv := GetCapAddress();
  MultiTexCoord4f := GetCapAddress();
  MultiTexCoord4fv := GetCapAddress();
  MultiTexCoord4i := GetCapAddress();
  MultiTexCoord4iv := GetCapAddress();
  MultiTexCoord4s := GetCapAddress();
  MultiTexCoord4sv := GetCapAddress();

  GetInteger64i_v := GetCapAddress();
  GetBufferParameteri64v := GetCapAddress();
  ProgramParameteri := GetCapAddress();

  ProgramString := GetCapAddress();
  BindProgram := GetCapAddress();
  DeletePrograms := GetCapAddress();
  GenPrograms := GetCapAddress();
  ProgramEnvParameter4d := GetCapAddress();
  ProgramEnvParameter4dv := GetCapAddress();
  ProgramEnvParameter4f := GetCapAddress();
  ProgramEnvParameter4fv := GetCapAddress();
  ProgramLocalParameter4d := GetCapAddress();
  ProgramLocalParameter4dv := GetCapAddress();
  ProgramLocalParameter4f := GetCapAddress();
  ProgramLocalParameter4fv := GetCapAddress();
  GetProgramEnvParameterdv := GetCapAddress();
  GetProgramEnvParameterfv := GetCapAddress();
  GetProgramLocalParameterdv := GetCapAddress();
  GetProgramLocalParameterfv := GetCapAddress();

  ClearColorIi := GetCapAddress();
  ClearColorIui := GetCapAddress();
  TexParameterIiv := GetCapAddress();
  TexParameterIuiv := GetCapAddress();
  GetTexParameterIiv := GetCapAddress();
  GetTexParameterIuiv := GetCapAddress();
  PatchParameteri := GetCapAddress();
  PatchParameterfv := GetCapAddress();

  BufferAddressRangeNV := GetCapAddress();
  VertexFormatNV := GetCapAddress();
  NormalFormatNV := GetCapAddress();
  ColorFormatNV := GetCapAddress();
  IndexFormatNV := GetCapAddress();
  TexCoordFormatNV := GetCapAddress();
  EdgeFlagFormatNV := GetCapAddress();
  SecondaryColorFormatNV := GetCapAddress();
  FogCoordFormatNV := GetCapAddress();
  VertexAttribFormatNV := GetCapAddress();
  VertexAttribIFormatNV := GetCapAddress();
  GetIntegerui64i_vNV := GetCapAddress();
  GetBufferParameterui64vNV := GetCapAddress();
  MakeBufferResidentNV := GetCapAddress();
  MakeBufferNonResidentNV := GetCapAddress();
  IsBufferResidentNV := GetCapAddress();
  MakeNamedBufferResidentNV := GetCapAddress();
  MakeNamedBufferNonResidentNV := GetCapAddress();
  IsNamedBufferResidentNV := GetCapAddress();
  GetNamedBufferParameterui64vNV := GetCapAddress();
  GetIntegerui64vNV := GetCapAddress();
  Uniformui64NV := GetCapAddress();
  Uniformui64vNV := GetCapAddress();
  GetUniformui64vNV := GetCapAddress();
  ProgramUniformui64NV := GetCapAddress();
  ProgramUniformui64vNV := GetCapAddress();

  TexImage2DMultisample := GetCapAddress();
  TexImage3DMultisample := GetCapAddress();
  GetMultisamplefv := GetCapAddress();
  SampleMaski := GetCapAddress();

  ProvokingVertex := GetCapAddress();

  FenceSync := GetCapAddress();
  IsSync := GetCapAddress();
  DeleteSync := GetCapAddress();
  ClientWaitSync := GetCapAddress();
  WaitSync := GetCapAddress();
  GetInteger64v := GetCapAddress();
  GetSynciv := GetCapAddress();

  BlendEquationi := GetCapAddress();
  BlendEquationSeparatei := GetCapAddress();
  BlendFunci := GetCapAddress();
  BlendFuncSeparatei := GetCapAddress();
  MinSampleShading := GetCapAddress();

  GenSamplers := GetCapAddress();
  DeleteSamplers := GetCapAddress();
  IsSampler := GetCapAddress();
  BindSampler := GetCapAddress();
  SamplerParameteri := GetCapAddress();
  SamplerParameteriv := GetCapAddress();
  SamplerParameterf := GetCapAddress();
  SamplerParameterfv := GetCapAddress();
  SamplerParameterIiv := GetCapAddress();
  SamplerParameterIuiv := GetCapAddress();
  GetSamplerParameteriv := GetCapAddress();
  GetSamplerParameterIiv := GetCapAddress();
  GetSamplerParameterfv := GetCapAddress();
  GetSamplerParameterIfv := GetCapAddress();

  ClientAttribDefault := GetCapAddress();
  PushClientAttribDefault := GetCapAddress();
  MatrixLoadf := GetCapAddress();
  MatrixLoadd := GetCapAddress();
  MatrixMultf := GetCapAddress();
  MatrixMultd := GetCapAddress();
  MatrixLoadIdentity := GetCapAddress();
  MatrixRotatef := GetCapAddress();
  MatrixRotated := GetCapAddress();
  MatrixScalef := GetCapAddress();
  MatrixScaled := GetCapAddress();
  MatrixTranslatef := GetCapAddress();
  MatrixTranslated := GetCapAddress();
  MatrixFrustum := GetCapAddress();
  MatrixOrtho := GetCapAddress();
  MatrixPop := GetCapAddress();
  MatrixPush := GetCapAddress();
  MatrixLoadTransposef := GetCapAddress();
  MatrixLoadTransposed := GetCapAddress();
  MatrixMultTransposef := GetCapAddress();
  MatrixMultTransposed := GetCapAddress();
  TextureParameterfv := GetCapAddress();
  TextureParameteri := GetCapAddress();
  TextureParameteriv := GetCapAddress();
  TextureImage1D := GetCapAddress();
  TextureImage2D := GetCapAddress();
  TextureSubImage1D := GetCapAddress();
  TextureSubImage2D := GetCapAddress();
  CopyTextureImage1D := GetCapAddress();
  CopyTextureImage2D := GetCapAddress();
  CopyTextureSubImage1D := GetCapAddress();
  CopyTextureSubImage2D := GetCapAddress();
  GetTextureImage := GetCapAddress();
  GetTextureParameterfv := GetCapAddress();
  GetTextureParameteriv := GetCapAddress();
  GetTextureLevelParameterfv := GetCapAddress();
  GetTextureLevelParameteriv := GetCapAddress();
  TextureImage3D := GetCapAddress();
  TextureSubImage3D := GetCapAddress();
  CopyTextureSubImage3D := GetCapAddress();
  MultiTexParameterf := GetCapAddress();
  MultiTexParameterfv := GetCapAddress();
  MultiTexParameteri := GetCapAddress();
  MultiTexParameteriv := GetCapAddress();
  MultiTexImage1D := GetCapAddress();
  MultiTexImage2D := GetCapAddress();
  MultiTexSubImage1D := GetCapAddress();
  MultiTexSubImage2D := GetCapAddress();
  CopyMultiTexImage1D := GetCapAddress();
  CopyMultiTexImage2D := GetCapAddress();
  CopyMultiTexSubImage1D := GetCapAddress();
  CopyMultiTexSubImage2D := GetCapAddress();
  GetMultiTexImage := GetCapAddress();
  GetMultiTexParameterfv := GetCapAddress();
  GetMultiTexParameteriv := GetCapAddress();
  GetMultiTexLevelParameterfv := GetCapAddress();
  GetMultiTexLevelParameteriv := GetCapAddress();
  MultiTexImage3D := GetCapAddress();
  MultiTexSubImage3D := GetCapAddress();
  CopyMultiTexSubImage3D := GetCapAddress();
  BindMultiTexture := GetCapAddress();
  EnableClientStateIndexed := GetCapAddress();
  DisableClientStateIndexed := GetCapAddress();
  MultiTexCoordPointer := GetCapAddress();
  MultiTexEnvf := GetCapAddress();
  MultiTexEnvfv := GetCapAddress();
  MultiTexEnvi := GetCapAddress();
  MultiTexEnviv := GetCapAddress();
  MultiTexGend := GetCapAddress();
  MultiTexGendv := GetCapAddress();
  MultiTexGenf := GetCapAddress();
  MultiTexGenfv := GetCapAddress();
  MultiTexGeni := GetCapAddress();
  MultiTexGeniv := GetCapAddress();
  GetMultiTexEnvfv := GetCapAddress();
  GetMultiTexEnviv := GetCapAddress();
  GetMultiTexGendv := GetCapAddress();
  GetMultiTexGenfv := GetCapAddress();
  GetMultiTexGeniv := GetCapAddress();
  GetFloatIndexedv := GetCapAddress();
  GetDoubleIndexedv := GetCapAddress();
  GetPointerIndexedv := GetCapAddress();
  CompressedTextureImage3D := GetCapAddress();
  CompressedTextureImage2D := GetCapAddress();
  CompressedTextureImage1D := GetCapAddress();
  CompressedTextureSubImage3D := GetCapAddress();
  CompressedTextureSubImage2D := GetCapAddress();
  CompressedTextureSubImage1D := GetCapAddress();
  GetCompressedTextureImage := GetCapAddress();
  CompressedMultiTexImage3D := GetCapAddress();
  CompressedMultiTexImage2D := GetCapAddress();
  CompressedMultiTexImage1D := GetCapAddress();
  CompressedMultiTexSubImage3D := GetCapAddress();
  CompressedMultiTexSubImage2D := GetCapAddress();
  CompressedMultiTexSubImage1D := GetCapAddress();
  GetCompressedMultiTexImage := GetCapAddress();
  NamedProgramString := GetCapAddress();
  NamedProgramLocalParameter4d := GetCapAddress();
  NamedProgramLocalParameter4dv := GetCapAddress();
  NamedProgramLocalParameter4f := GetCapAddress();
  NamedProgramLocalParameter4fv := GetCapAddress();
  GetNamedProgramLocalParameterdv := GetCapAddress();
  GetNamedProgramLocalParameterfv := GetCapAddress();
  GetNamedProgramiv := GetCapAddress();
  GetNamedProgramString := GetCapAddress();
  NamedProgramLocalParameters4fv := GetCapAddress();
  NamedProgramLocalParameterI4i := GetCapAddress();
  NamedProgramLocalParameterI4iv := GetCapAddress();
  NamedProgramLocalParametersI4iv := GetCapAddress();
  NamedProgramLocalParameterI4ui := GetCapAddress();
  NamedProgramLocalParameterI4uiv := GetCapAddress();
  NamedProgramLocalParametersI4uiv := GetCapAddress();
  GetNamedProgramLocalParameterIiv := GetCapAddress();
  GetNamedProgramLocalParameterIuiv := GetCapAddress();
  TextureParameterIiv := GetCapAddress();
  TextureParameterIuiv := GetCapAddress();
  GetTextureParameterIiv := GetCapAddress();
  GetTextureParameterIuiv := GetCapAddress();
  MultiTexParameterIiv := GetCapAddress();
  MultiTexParameterIuiv := GetCapAddress();
  GetMultiTexParameterIiv := GetCapAddress();
  GetMultiTexParameterIuiv := GetCapAddress();
  NamedBufferData := GetCapAddress();
  NamedBufferSubData := GetCapAddress();
  MapNamedBuffer := GetCapAddress();
  UnmapNamedBuffer := GetCapAddress();
  MapNamedBufferRange := GetCapAddress();
  FlushMappedNamedBufferRange := GetCapAddress();
  NamedCopyBufferSubData := GetCapAddress();
  GetNamedBufferParameteriv := GetCapAddress();
  GetNamedBufferPointerv := GetCapAddress();
  GetNamedBufferSubData := GetCapAddress();
  TextureBuffer := GetCapAddress();
  MultiTexBuffer := GetCapAddress();
  NamedRenderbufferStorage := GetCapAddress();
  GetNamedRenderbufferParameteriv := GetCapAddress();
  CheckNamedFramebufferStatus := GetCapAddress();
  NamedFramebufferTexture1D := GetCapAddress();
  NamedFramebufferTexture2D := GetCapAddress();
  NamedFramebufferTexture3D := GetCapAddress();
  NamedFramebufferRenderbuffer := GetCapAddress();
  GetNamedFramebufferAttachmentParameteriv := GetCapAddress();
  GenerateTextureMipmap := GetCapAddress();
  GenerateMultiTexMipmap := GetCapAddress();
  FramebufferDrawBuffer := GetCapAddress();
  FramebufferDrawBuffers := GetCapAddress();
  FramebufferReadBuffer := GetCapAddress();
  GetFramebufferParameteriv := GetCapAddress();
  NamedRenderbufferStorageMultisample := GetCapAddress();
  NamedRenderbufferStorageMultisampleCoverage := GetCapAddress();
  NamedFramebufferTexture := GetCapAddress();
  NamedFramebufferTextureLayer := GetCapAddress();
  NamedFramebufferTextureFace := GetCapAddress();
  TextureRenderbuffer := GetCapAddress();
  MultiTexRenderbuffer := GetCapAddress();

  FrameTerminatorGREMEDY := GetCapAddress();
  StringMarkerGREMEDY := GetCapAddress();
  DebugMessageControl := GetCapAddress();
  DebugMessageInsert := GetCapAddress();
  DebugMessageCallback := GetCapAddress();
  GetDebugMessageLog := GetCapAddress();

  PushDebugGroup := GetCapAddress();
  PopDebugGroup := GetCapAddress();
  ObjectLabel := GetCapAddress();
  GetObjectLabel := GetCapAddress();
  ObjectPtrLabel := GetCapAddress();
  GetObjectPtrLabel := GetCapAddress();

  ClearBufferData := GetCapAddress();
  ClearBufferSubData := GetCapAddress();
  ClearNamedBufferData := GetCapAddress();
  ClearNamedBufferSubData := GetCapAddress();

  DispatchCompute := GetCapAddress();
  DispatchComputeIndirect := GetCapAddress();

  CopyImageSubData := GetCapAddress();

  FramebufferParameteri := GetCapAddress();
  NamedFramebufferParameteri := GetCapAddress();
  GetNamedFramebufferParameteriv := GetCapAddress();

  GetInternalformati64v := GetCapAddress();

  InvalidateTexSubImage := GetCapAddress();
  InvalidateTexImage := GetCapAddress();
  InvalidateBufferSubData := GetCapAddress();
  InvalidateBufferData := GetCapAddress();
  InvalidateFramebuffer := GetCapAddress();
  InvalidateSubFramebuffer := GetCapAddress();

  MultiDrawArraysIndirect := GetCapAddress();
  MultiDrawElementsIndirect := GetCapAddress();

  GetProgramInterfaceiv := GetCapAddress();
  GetProgramResourceIndex := GetCapAddress();
  GetProgramResourceName := GetCapAddress();
  GetProgramResourceiv := GetCapAddress();
  GetProgramResourceLocation := GetCapAddress();
  GetProgramResourceLocationIndex := GetCapAddress();

  ShaderStorageBlockBinding := GetCapAddress();

  TexBufferRange := GetCapAddress();
  TextureBufferRange := GetCapAddress();

  TexStorage2DMultisample := GetCapAddress();
  TexStorage3DMultisample := GetCapAddress();
  TextureStorage2DMultisample := GetCapAddress();
  TextureStorage3DMultisample := GetCapAddress();

  BufferStorage := GetCapAddress();
  ClearTexImage := GetCapAddress();
  ClearTexSubImage := GetCapAddress();
  BindBuffersBase := GetCapAddress();
  BindBuffersRange := GetCapAddress();
  BindTextures := GetCapAddress();
  BindSamplers := GetCapAddress();
  BindImageTextures := GetCapAddress();
  BindVertexBuffers := GetCapAddress();

  TextureView := GetCapAddress();

  BindVertexBuffer := GetCapAddress();
  VertexAttribFormat := GetCapAddress();
  VertexAttribIFormat := GetCapAddress();
  VertexAttribLFormat := GetCapAddress();
  VertexAttribBinding := GetCapAddress();
  VertexBindingDivisor := GetCapAddress();
  VertexArrayBindVertexBuffer := GetCapAddress();
  VertexArrayVertexAttribFormat := GetCapAddress();
  VertexArrayVertexAttribIFormat := GetCapAddress();
  VertexArrayVertexAttribLFormat := GetCapAddress();
  VertexArrayVertexAttribBinding := GetCapAddress();
  VertexArrayVertexBindingDivisor := GetCapAddress();

  CreateSyncFromCLevent := GetCapAddress();

  GenPathsNV := GetCapAddress();
  DeletePathsNV := GetCapAddress();
  IsPathNV := GetCapAddress();
  PathCommandsNV := GetCapAddress();
  PathCoordsNV := GetCapAddress();
  PathSubCommandsNV := GetCapAddress();
  PathSubCoordsNV := GetCapAddress();
  PathStringNV := GetCapAddress();
  PathGlyphsNV := GetCapAddress();
  PathGlyphRangeNV := GetCapAddress();
  WeightPathsNV := GetCapAddress();
  CopyPathNV := GetCapAddress();
  InterpolatePathsNV := GetCapAddress();
  PathParameterivNV := GetCapAddress();
  PathParameteriNV := GetCapAddress();
  PathParameterfvNV := GetCapAddress();
  PathParameterfNV := GetCapAddress();
  PathDashArrayNV := GetCapAddress();
  PathStencilFuncNV := GetCapAddress();
  StencilFillPathNV := GetCapAddress();
  StencilStrokePathNV := GetCapAddress();
  StencilFillPathInstancedNV := GetCapAddress();
  StencilStrokePathInstancedNV := GetCapAddress();
  PathColorGenNV := GetCapAddress();
  PathTexGenNV := GetCapAddress();
  PathFogGenNV := GetCapAddress();
  CoverFillPathNV := GetCapAddress();
  CoverStrokePathNV := GetCapAddress();
  CoverFillPathInstancedNV := GetCapAddress();
  CoverStrokePathInstancedNV := GetCapAddress();
  GetPathParameterivNV := GetCapAddress();
  GetPathParameterfvNV := GetCapAddress();
  GetPathCommandsNV := GetCapAddress();
  GetPathCoordsNV := GetCapAddress();
  GetPathDashArrayNV := GetCapAddress();
  GetPathMetricsNV := GetCapAddress();
  GetPathMetricRangeNV := GetCapAddress();
  GetPathSpacingNV := GetCapAddress();
  GetPathColorGenivNV := GetCapAddress();
  GetPathColorGenfvNV := GetCapAddress();
  GetPathTexGenivNV := GetCapAddress();
  GetPathTexGenfvNV := GetCapAddress();
  IsPointInFillPathNV := GetCapAddress();
  IsPointInStrokePathNV := GetCapAddress();
  GetPathLengthNV := GetCapAddress();
  PointAlongPathNV := GetCapAddress();
  PathStencilDepthOffsetNV := GetCapAddress();
  PathCoverDepthFuncNV := GetCapAddress();

  FInitialized := False;
end;

{$IFDEF SUPPORT_WGL}
// ReadWGLImplementationProperties

procedure TGLExtensionsAndEntryPoints.ReadWGLImplementationProperties;
begin
  // ARB wgl extensions
  if Assigned(WGetExtensionsStringARB) then
    FBuffer := string(WGetExtensionsStringARB(wglGetCurrentDC))
  else
    FBuffer := '';
  W_ARB_buffer_region := CheckExtension('WGL_ARB_buffer_region');
  W_ARB_create_context := CheckExtension('WGL_ARB_create_context');
  W_ARB_create_context_profile :=
    CheckExtension('WGL_ARB_create_context_profile');
  W_ARB_extensions_string := CheckExtension('WGL_ARB_extensions_string');
  W_ARB_framebuffer_sRGB := CheckExtension('WGL_ARB_framebuffer_sRGB');
  W_ARB_make_current_read := CheckExtension('WGL_ARB_make_current_read');
  W_ARB_multisample := CheckExtension('WGL_ARB_multisample');
  W_ARB_pbuffer := CheckExtension('WGL_ARB_pbuffer');
  W_ARB_pixel_format := CheckExtension('WGL_ARB_pixel_format');
  W_ARB_pixel_format_float := CheckExtension('WGL_ARB_pixel_format_float');
  W_ARB_render_texture := CheckExtension('WGL_ARB_render_texture');
  // Vendor/EXT wgl extensions
  W_ATI_pixel_format_float := CheckExtension('WGL_ATI_pixel_format_float');
  W_EXT_framebuffer_sRGB := CheckExtension('WGL_EXT_framebuffer_sRGB');
  W_EXT_pixel_format_packed_float :=
    CheckExtension('WGL_EXT_pixel_format_packed_float');
  W_EXT_swap_control := CheckExtension('WGL_EXT_swap_control');
  W_NV_gpu_affinity := CheckExtension('WGL_NV_gpu_affinity');
  W_NV_DX_interop := CheckExtension('WGL_NV_DX_interop');
  W_NV_DX_interop2 := CheckExtension('WGL_NV_DX_interop2');
  W_EXT_create_context_es2_profile :=
    CheckExtension('WGL_EXT_create_context_es2_profile');
end;

procedure TGLExtensionsAndEntryPoints.ReadWGLExtensions;
begin
  // ARB wgl extensions

  // ###########################################################
  // locating functions and procedures for
  // ARB approved WGL extensions
  // ###########################################################

  // WGL_buffer_region (ARB #4)
  WCreateBufferRegionARB := GetProcAddressGLS('wglCreateBufferRegionARB');
  WDeleteBufferRegionARB := GetProcAddressGLS('wglDeleteBufferRegionARB');
  WSaveBufferRegionARB := GetProcAddressGLS('wglSaveBufferRegionARB');
  WRestoreBufferRegionARB := GetProcAddressGLS('wglRestoreBufferRegionARB');

  // WGL_ARB_extensions_string (ARB #8)
  WGetExtensionsStringARB := GetProcAddressGLS('wglGetExtensionsStringARB');

  // WGL_ARB_pixel_format (ARB #9)
  WGetPixelFormatAttribivARB :=
    GetProcAddressGLS('wglGetPixelFormatAttribivARB');
  WGetPixelFormatAttribfvARB :=
    GetProcAddressGLS('wglGetPixelFormatAttribfvARB');
  WChoosePixelFormatARB := GetProcAddressGLS('wglChoosePixelFormatARB');

  // WGL_make_current_read (ARB #10)
  WMakeContextCurrentARB := GetProcAddressGLS('wglMakeContextCurrentARB');
  WGetCurrentReadDCARB := GetProcAddressGLS('wglGetCurrentReadDCARB');

  // WGL_ARB_pbuffer (ARB #11)
  WCreatePbufferARB := GetProcAddressGLS('wglCreatePbufferARB');
  WGetPbufferDCARB := GetProcAddressGLS('wglGetPbufferDCARB');
  WReleasePbufferDCARB := GetProcAddressGLS('wglReleasePbufferDCARB');
  WDestroyPbufferARB := GetProcAddressGLS('wglDestroyPbufferARB');
  WQueryPbufferARB := GetProcAddressGLS('wglQueryPbufferARB');

  // WGL_ARB_render_texture (ARB #20)
  WBindTexImageARB := GetProcAddressGLS('wglBindTexImageARB');
  WReleaseTexImageARB := GetProcAddressGLS('wglReleaseTexImageARB');
  WSetPbufferAttribARB := GetProcAddressGLS('wglSetPbufferAttribARB');

  // WGL_ARB_create_context (ARB #55)
  WCreateContextAttribsARB := GetProcAddressGLS('wglCreateContextAttribsARB');

  // ###########################################################
  // locating functions and procedures for
  // Vendor/EXT WGL extensions
  // ###########################################################

  // WGL_EXT_swap_control (EXT #172)
  WSwapIntervalEXT := GetProcAddressGLS('wglSwapIntervalEXT');
  WGetSwapIntervalEXT := GetProcAddressGLS('wglGetSwapIntervalEXT');

  // GL_NV_vertex_array_range (EXT #190)
  WAllocateMemoryNV := GetProcAddressGLS('wglAllocateMemoryNV');
  WFreeMemoryNV := GetProcAddressGLS('wglFreeMemoryNV');

  // WGL_NV_gpu_affinity
  WEnumGpusNV := GetProcAddressGLS('wglEnumGpusNV');
  WEnumGpuDevicesNV := GetProcAddressGLS('wglEnumGpuDevicesNV');
  WCreateAffinityDCNV := GetProcAddressGLS('wglCreateAffinityDCNV');
  WEnumGpusFromAffinityDCNV := GetProcAddressGLS('wglEnumGpusFromAffinityDCNV');
  WDeleteDCNV := GetProcAddressGLS('wglDeleteDCNV');

  // WGL_NV_DX_interop
  WDXSetResourceShareHandleNV :=
    GetProcAddressGLS('wglDXSetResourceShareHandleNV');
  WDXOpenDeviceNV := GetProcAddressGLS('wglDXOpenDeviceNV');
  WDXCloseDeviceNV := GetProcAddressGLS('wglDXCloseDeviceNV');
  WDXRegisterObjectNV := GetProcAddressGLS('wglDXRegisterObjectNV');
  WDXUnregisterObjectNV := GetProcAddressGLS('wglDXUnregisterObjectNV');
  WDXObjectAccessNV := GetProcAddressGLS('wglDXObjectAccessNV');
  WDXLockObjectsNV := GetProcAddressGLS('wglDXLockObjectsNV');
  WDXUnlockObjectsNV := GetProcAddressGLS('wglDXUnlockObjectsNV');
end;

{$ENDIF}

procedure TrimAndSplitVersionString(buffer: string; var max, min: integer);
// Peels out the X.Y form from the given Buffer which must contain a version string like "text Minor.Major.Build text"
// at least however "Major.Minor".
var
  Separator: integer;
begin
  try
    // There must be at least one dot to separate major and minor version number.
    Separator := Pos('.', buffer);
    // At least one number must be before and one after the dot.
    if (Separator > 1) and (Separator < length(buffer)) and
      (AnsiChar(buffer[Separator - 1]) in ['0' .. '9']) and
      (AnsiChar(buffer[Separator + 1]) in ['0' .. '9']) then
    begin
      // OK, it's a valid version string. Now remove unnecessary parts.
      Dec(Separator);
      // Find last non-numeric character before version number.
      while (Separator > 0) and (AnsiChar(buffer[Separator]) in ['0' .. '9']) do
        Dec(Separator);
      // Delete leading characters which do not belong to the version string.
      Delete(buffer, 1, Separator);
      Separator := Pos('.', buffer) + 1;
      // Find first non-numeric character after version number
      while (Separator <= length(buffer)) and
        (AnsiChar(buffer[Separator]) in ['0' .. '9']) do
        Inc(Separator);
      // delete trailing characters not belonging to the version string
      Delete(buffer, Separator, 255);
      // Now translate the numbers.
      Separator := Pos('.', buffer);
      // This is necessary because the buffer length might have changed.
      max := StrToInt(Copy(buffer, 1, Separator - 1));
      min := StrToInt(Copy(buffer, Separator + 1, 255));
    end
    else
      Abort;
  except
    min := 0;
    max := 0;
  end;
end;

function IsVersionMet(MajorVersion, MinorVersion, actualMajorVersion,
  actualMinorVersion: integer): boolean;
begin
  Result := (actualMajorVersion > MajorVersion) or
    ((actualMajorVersion = MajorVersion) and
    (actualMinorVersion >= MinorVersion));
end;

function InitOpenGL: boolean;
begin
  if (GLHandle = INVALID_MODULEHANDLE) or (GLUHandle = INVALID_MODULEHANDLE)
  then
    Result := InitOpenGLFromLibrary(opengl32, glu32)
  else
    Result := True;
end;

function InitOpenGLFromLibrary(const GLName, GLUName: string): boolean;
begin
  Result := False;
  CloseOpenGL;

  GLHandle := LoadLibrary(pchar(GLName));
  GLUHandle := LoadLibrary(pchar(GLUName));
  if (GLHandle <> INVALID_MODULEHANDLE) and (GLUHandle <> INVALID_MODULEHANDLE)
  then
  begin
    Result := True;
  end
  else
    CloseOpenGL;
end;

function IsOpenGLInitialized: boolean;
begin
  Result := (GLHandle <> INVALID_MODULEHANDLE);
end;

procedure CloseOpenGL;
begin
  if GLHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(GLHandle);
    GLHandle := INVALID_MODULEHANDLE;
  end;
  if GLUHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(GLUHandle);
    GLUHandle := INVALID_MODULEHANDLE;
  end;
end;

procedure UnloadOpenGL;
begin
  CloseOpenGL;
end;

function LoadOpenGL: boolean;
begin
  Result := InitOpenGL;
end;

function LoadOpenGLFromLibrary(GLName, GLUName: string): boolean;
begin
  Result := InitOpenGLFromLibrary(GLName, GLUName);
end;

function IsOpenGLLoaded: boolean;
begin
  Result := IsOpenGLInitialized();
end;

function IsMesaGL: boolean;
begin
  Result := GetProcAddressGLS('glResizeBuffersMESA') <> nil;
end;

//--------------------------------------
initialization
//--------------------------------------

{$IFNDEF CROSSVCL}
Set8087CW($133F);
{$ENDIF}

finalization

CloseOpenGL;

end.
