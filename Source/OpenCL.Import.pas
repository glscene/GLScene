//
// The graphics platform GLScene https://github.com/glscene
//
unit OpenCL.Import;

(*
   Conversion of OpenCL cl.h header file into CL.pas
   from http://www.khronos.org/registry/cl/.
*)

(****************************************************************************
 * Copyright (c) 2008-2020 The Khronos Group Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and/or associated documentation files (the
 * "Materials"), to deal in the Materials without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Materials, and to
 * permit persons to whom the Materials are furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Materials.
 *
 * MODIFICATIONS TO THIS FILE MAY MEAN IT NO LONGER ACCURATELY REFLECTS
 * KHRONOS STANDARDS. THE UNMODIFIED, NORMATIVE VERSIONS OF KHRONOS
 * SPECIFICATIONS AND HEADER INFORMATION ARE LOCATED AT
 *    https://www.khronos.org/registry/
 *
 * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
 **************************************************************************)

interface

uses
  Winapi.Windows,
  OpenCL.Platform;

const
  {$IFDEF MSWINDOWS}
  LibOpenCL = 'OpenCL.dll';
 {$ELSE}
  LibOpenCL = 'OpenCL.so';
 {$ENDIF}

type
  T_cl_platform_id = record end;
  Tcl_platform_id = ^T_cl_platform_id;
  Pcl_platform_id = ^Tcl_platform_id;
  T_cl_device_id = record end;
  Tcl_device_id = ^T_cl_device_id;
  Pcl_device_id = ^Tcl_device_id;
  T_cl_context = record end;
  Tcl_context = ^T_cl_context;
  Pcl_context = ^Tcl_context;
  T_cl_command_queue = record end;
  Tcl_command_queue = ^T_cl_command_queue;
  Pcl_command_queue = ^Tcl_command_queue;
  T_cl_mem = record end;
  Tcl_mem = ^T_cl_mem;
  Pcl_mem = ^Tcl_mem;
  T_cl_program = record end;
  Tcl_program = ^T_cl_program;
  Pcl_program = ^Tcl_program;
  T_cl_kernel = record end;
  Tcl_kernel = ^T_cl_kernel;
  Pcl_kernel = ^Tcl_kernel;
  T_cl_event = record end;
  Tcl_event = ^T_cl_event;
  Pcl_event = ^Tcl_event;
  T_cl_sampler = record end;
  Tcl_sampler = ^T_cl_sampler;
  Pcl_sampler = ^Tcl_sampler;

  Tcl_bool = Tcl_uint;  //* WARNING!  Unlike cl_ types in cl_platform.h, cl_bool is not guaranteed to be the same size as the bool in kernels. *//
  Pcl_bool = ^Tcl_bool;
  Tcl_bitfield = Tcl_ulong;
  Pcl_bitfield = ^Tcl_bitfield;
  Tcl_device_type = Tcl_bitfield;
  Pcl_device_type = ^Tcl_device_type;
  Tcl_platform_info = Tcl_uint;
  Pcl_platform_info = ^Tcl_platform_info;
  Tcl_device_info = Tcl_uint;
  Pcl_device_info = ^Tcl_device_info;
  Tcl_device_fp_config = Tcl_bitfield;
  Pcl_device_fp_config = ^Tcl_device_fp_config;
  Tcl_device_mem_cache_type = Tcl_uint;
  Pcl_device_mem_cache_type = ^Tcl_device_mem_cache_type;
  Tcl_device_local_mem_type = Tcl_uint;
  Pcl_device_local_mem_type = ^Tcl_device_local_mem_type;
  Tcl_device_exec_capabilities = Tcl_bitfield;
  Pcl_device_exec_capabilities = ^Tcl_device_exec_capabilities;
  Tcl_device_svm_capabilities = Tcl_bitfield;
  Pcl_device_svm_capabilities = ^Tcl_device_svm_capabilities;
  Tcl_command_queue_properties = Tcl_bitfield;
  Pcl_command_queue_properties = ^Tcl_command_queue_properties;
  Tcl_device_partition_property = intptr_t;
  Pcl_device_partition_property = ^Tcl_device_partition_property;
  Tcl_device_affinity_domain = Tcl_bitfield;
  Pcl_device_affinity_domain = ^Tcl_device_affinity_domain;
  Tcl_context_properties = intptr_t;
  Pcl_context_properties = ^Tcl_context_properties;
  Tcl_context_info = Tcl_uint;
  Pcl_context_info = ^Tcl_context_info;
  Tcl_queue_properties = Tcl_bitfield;
  Pcl_queue_properties = ^Tcl_queue_properties;
  Tcl_command_queue_info = Tcl_uint;
  Pcl_command_queue_info = ^Tcl_command_queue_info;
  Tcl_channel_order = Tcl_uint;
  Pcl_channel_order = ^Tcl_channel_order;
  Tcl_channel_type = Tcl_uint;
  Pcl_channel_type = ^Tcl_channel_type;
  Tcl_mem_flags = Tcl_bitfield;
  Pcl_mem_flags = ^Tcl_mem_flags;
  Tcl_svm_mem_flags = Tcl_bitfield;
  Pcl_svm_mem_flags = ^Tcl_svm_mem_flags;
  Tcl_mem_object_type = Tcl_uint;
  Pcl_mem_object_type = ^Tcl_mem_object_type;
  Tcl_mem_info = Tcl_uint;
  Pcl_mem_info = ^Tcl_mem_info;
  Tcl_mem_migration_flag = Tcl_bitfield;
  Pcl_mem_migration_flag = ^Tcl_mem_migration_flag;
  Tcl_image_info = Tcl_uint;
  Pcl_image_info = ^Tcl_image_info;
  Tcl_buffer_create_type = Tcl_uint;
  Pcl_buffer_create_type = ^Tcl_buffer_create_type;
  Tcl_addressing_mode = Tcl_uint;
  Pcl_addressing_mode = ^Tcl_addressing_mode;
  Tcl_filter_mode = Tcl_uint;
  Pcl_filter_mode = ^Tcl_filter_mode;
  Tcl_sampler_info = Tcl_uint;
  Pcl_sampler_info = ^Tcl_sampler_info;
  Tcl_map_flags = Tcl_bitfield;
  Pcl_map_flags = ^Tcl_map_flags;
  Tcl_pipe_properties = intptr_t;
  Pcl_pipe_properties = ^Tcl_pipe_properties;
  Tcl_pipe_info = Tcl_uint;
  Pcl_pipe_info = ^Tcl_pipe_info;
  Tcl_program_info = Tcl_uint;
  Pcl_program_info = ^Tcl_program_info;
  Tcl_program_build_info = Tcl_uint;
  Pcl_program_build_info = ^Tcl_program_build_info;
  Tcl_program_binary_type = Tcl_uint;
  Pcl_program_binary_type = ^Tcl_program_binary_type;
  Tcl_build_status = Tcl_int;
  Pcl_build_status = ^Tcl_build_status;
  Tcl_kernel_info = Tcl_uint;
  Pcl_kernel_info = ^Tcl_kernel_info;
  Tcl_kernel_arg_info = Tcl_uint;
  Pcl_kernel_arg_info = ^Tcl_kernel_arg_info;
  Tcl_kernel_arg_address_qualifier = Tcl_uint;
  Pcl_kernel_arg_address_qualifier = ^Tcl_kernel_arg_address_qualifier;
  Tcl_kernel_arg_access_qualifier = Tcl_uint;
  Pcl_kernel_arg_access_qualifier = ^Tcl_kernel_arg_access_qualifier;
  Tcl_kernel_arg_type_qualifier = Tcl_bitfield;
  Pcl_kernel_arg_type_qualifier = ^Tcl_kernel_arg_type_qualifier;
  Tcl_kernel_work_group_info = Tcl_uint;
  Pcl_kernel_work_group_info = ^Tcl_kernel_work_group_info;
  Tcl_kernel_sub_group_info = Tcl_uint;
  Pcl_kernel_sub_group_info = ^Tcl_kernel_sub_group_info;
  Tcl_event_info = Tcl_uint;
  Pcl_event_info = ^Tcl_event_info;
  Tcl_command_type = Tcl_uint;
  Pcl_command_type = ^Tcl_command_type;
  Tcl_profiling_info = Tcl_uint;
  Pcl_profiling_info = ^Tcl_profiling_info;
  Tcl_sampler_properties = Tcl_bitfield;
  Pcl_sampler_properties = ^Tcl_sampler_properties;
  Tcl_kernel_exec_info = Tcl_uint;
  Pcl_kernel_exec_info = ^Tcl_kernel_exec_info;

type
  Tcl_image_format = packed record
    image_channel_order: Tcl_channel_order;
    image_channel_data_type: Tcl_channel_type;
  end;
  Pcl_image_format = ^Tcl_image_format;

  Tcl_image_desc = packed record
    image_type: Tcl_mem_object_type;
    image_width: NativeUInt;
    image_height: NativeUInt;
    image_depth: NativeUInt;
    image_array_size: NativeUInt;
    image_row_pitch: NativeUInt;
    image_slice_pitch: NativeUInt;
    num_mip_levels: Tcl_uint;
    num_samples: Tcl_uint;
    case Word of
     1: (buffer: Pcl_mem;);
     2: (mem_object: Pcl_mem;);
  end;
  Pcl_image_desc = ^Tcl_image_desc;

  Tcl_buffer_region = packed record
    origin: NativeUInt;
    size: NativeUInt;
  end;
  Pcl_buffer_region = ^Tcl_buffer_region;

(******************************************************************************)
  
 
(* Error Codes *)
const
  CL_SUCCESS =                                     0;
  CL_DEVICE_NOT_FOUND =                           -1;
  CL_DEVICE_NOT_AVAILABLE =                       -2;
  CL_COMPILER_NOT_AVAILABLE =                     -3;
  CL_MEM_OBJECT_ALLOCATION_FAILURE =              -4;
  CL_OUT_OF_RESOURCES =                           -5;
  CL_OUT_OF_HOST_MEMORY =                         -6;
  CL_PROFILING_INFO_NOT_AVAILABLE =               -7;
  CL_MEM_COPY_OVERLAP =                           -8;
  CL_IMAGE_FORMAT_MISMATCH =                      -9;
  CL_IMAGE_FORMAT_NOT_SUPPORTED =                -10;
  CL_BUILD_PROGRAM_FAILURE =                     -11;
  CL_MAP_FAILURE =                               -12;
  CL_MISALIGNED_SUB_BUFFER_OFFSET =              -13;
  CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST = -14;
  CL_COMPILE_PROGRAM_FAILURE =                   -15;
  CL_LINKER_NOT_AVAILABLE =                      -16;
  CL_LINK_PROGRAM_FAILURE =                      -17;
  CL_DEVICE_PARTITION_FAILED =                   -18;
  CL_KERNEL_ARG_INFO_NOT_AVAILABLE =             -19;

  CL_INVALID_VALUE =                             -30;
  CL_INVALID_DEVICE_TYPE =                       -31;
  CL_INVALID_PLATFORM =                          -32;
  CL_INVALID_DEVICE =                            -33;
  CL_INVALID_CONTEXT =                           -34;
  CL_INVALID_QUEUE_PROPERTIES =                  -35;
  CL_INVALID_COMMAND_QUEUE =                     -36;
  CL_INVALID_HOST_PTR =                          -37;
  CL_INVALID_MEM_OBJECT =                        -38;
  CL_INVALID_IMAGE_FORMAT_DESCRIPTOR =           -39;
  CL_INVALID_IMAGE_SIZE =                        -40;
  CL_INVALID_SAMPLER =                           -41;
  CL_INVALID_BINARY =                            -42;
  CL_INVALID_BUILD_OPTIONS =                     -43;
  CL_INVALID_PROGRAM =                           -44;
  CL_INVALID_PROGRAM_EXECUTABLE =                -45;
  CL_INVALID_KERNEL_NAME =                       -46;
  CL_INVALID_KERNEL_DEFINITION =                 -47;
  CL_INVALID_KERNEL =                            -48;
  CL_INVALID_ARG_INDEX =                         -49;
  CL_INVALID_ARG_VALUE =                         -50;
  CL_INVALID_ARG_SIZE =                          -51;
  CL_INVALID_KERNEL_ARGS =                       -52;
  CL_INVALID_WORK_DIMENSION =                    -53;
  CL_INVALID_WORK_GROUP_SIZE =                   -54;
  CL_INVALID_WORK_ITEM_SIZE =                    -55;
  CL_INVALID_GLOBAL_OFFSET =                     -56;
  CL_INVALID_EVENT_WAIT_LIST =                   -57;
  CL_INVALID_EVENT =                             -58;
  CL_INVALID_OPERATION =                         -59;
  CL_INVALID_GL_OBJECT =                         -60;
  CL_INVALID_BUFFER_SIZE =                       -61;
  CL_INVALID_MIP_LEVEL =                         -62;
  CL_INVALID_GLOBAL_WORK_SIZE =                  -63;
  CL_INVALID_PROPERTY =                          -64;
  CL_INVALID_IMAGE_DESCRIPTOR =                  -65;
  CL_INVALID_COMPILER_OPTIONS =                  -66;
  CL_INVALID_LINKER_OPTIONS =                    -67;
  CL_INVALID_DEVICE_PARTITION_COUNT =            -68;
  CL_INVALID_PIPE_SIZE =                         -69;
  CL_INVALID_DEVICE_QUEUE =                      -70;
  CL_INVALID_SPEC_ID =                           -71;
  CL_MAX_SIZE_RESTRICTION_EXCEEDED =             -72;

  (* OpenCL Version *)
  CL_VERSION_1_0 =                                 1;
  CL_VERSION_1_1 =                                 1;
  CL_VERSION_1_2 =                                 1;
  CL_VERSION_2_0 =                                 1;
  CL_VERSION_2_1 =                                 1;
  CL_VERSION_2_2 =                                 1;

  (* cl_bool *)
  CL_FALSE =                                       0;
  CL_TRUE =                                        1;
  CL_BLOCKING =                                    CL_TRUE;
  CL_NON_BLOCKING =                                CL_FALSE;

  (* cl_platform_info *)
  CL_PLATFORM_PROFILE =                            $0900;
  CL_PLATFORM_VERSION =                            $0901;
  CL_PLATFORM_NAME =                               $0902;
  CL_PLATFORM_VENDOR =                             $0903;
  CL_PLATFORM_EXTENSIONS =                         $0904;
  CL_PLATFORM_HOST_TIMER_RESOLUTION =              $0905;

  (* cl_device_type - bitfield *)
  CL_DEVICE_TYPE_DEFAULT =                         (1 shl 0);
  CL_DEVICE_TYPE_CPU =                             (1 shl 1);
  CL_DEVICE_TYPE_GPU =                             (1 shl 2);
  CL_DEVICE_TYPE_ACCELERATOR =                     (1 shl 3);
  CL_DEVICE_TYPE_CUSTOM =                          (1 shl 4);
  CL_DEVICE_TYPE_ALL =                             $FFFFFFFF;

  (* cl_device_info *)
  CL_DEVICE_TYPE =                                 $1000;
  CL_DEVICE_VENDOR_ID =                            $1001;
  CL_DEVICE_MAX_COMPUTE_UNITS =                    $1002;
  CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS =             $1003;
  CL_DEVICE_MAX_WORK_GROUP_SIZE =                  $1004;
  CL_DEVICE_MAX_WORK_ITEM_SIZES =                  $1005;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR =          $1006;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT =         $1007;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT =           $1008;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG =          $1009;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT =         $100A;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE =        $100B;
  CL_DEVICE_MAX_CLOCK_FREQUENCY =                  $100C;
  CL_DEVICE_ADDRESS_BITS =                         $100D;
  CL_DEVICE_MAX_READ_IMAGE_ARGS =                  $100E;
  CL_DEVICE_MAX_WRITE_IMAGE_ARGS =                 $100F;
  CL_DEVICE_MAX_MEM_ALLOC_SIZE =                   $1010;
  CL_DEVICE_IMAGE2D_MAX_WIDTH =                    $1011;
  CL_DEVICE_IMAGE2D_MAX_HEIGHT =                   $1012;
  CL_DEVICE_IMAGE3D_MAX_WIDTH =                    $1013;
  CL_DEVICE_IMAGE3D_MAX_HEIGHT =                   $1014;
  CL_DEVICE_IMAGE3D_MAX_DEPTH =                    $1015;
  CL_DEVICE_IMAGE_SUPPORT =                        $1016;
  CL_DEVICE_MAX_PARAMETER_SIZE =                   $1017;
  CL_DEVICE_MAX_SAMPLERS =                         $1018;
  CL_DEVICE_MEM_BASE_ADDR_ALIGN =                  $1019;
  CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE =             $101A;
  CL_DEVICE_SINGLE_FP_CONFIG =                     $101B;
  CL_DEVICE_GLOBAL_MEM_CACHE_TYPE =                $101C;
  CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE =            $101D;
  CL_DEVICE_GLOBAL_MEM_CACHE_SIZE =                $101E;
  CL_DEVICE_GLOBAL_MEM_SIZE =                      $101F;
  CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE =             $1020;
  CL_DEVICE_MAX_CONSTANT_ARGS =                    $1021;
  CL_DEVICE_LOCAL_MEM_TYPE =                       $1022;
  CL_DEVICE_LOCAL_MEM_SIZE =                       $1023;
  CL_DEVICE_ERROR_CORRECTION_SUPPORT =             $1024;
  CL_DEVICE_PROFILING_TIMER_RESOLUTION =           $1025;
  CL_DEVICE_ENDIAN_LITTLE =                        $1026;
  CL_DEVICE_AVAILABLE =                            $1027;
  CL_DEVICE_COMPILER_AVAILABLE =                   $1028;
  CL_DEVICE_EXECUTION_CAPABILITIES =               $1029;
  CL_DEVICE_QUEUE_PROPERTIES =                     $102A;  (* deprecated *) 
  CL_DEVICE_QUEUE_ON_HOST_PROPERTIES =             $102A;
  CL_DEVICE_NAME = 	                               $102B;
  CL_DEVICE_VENDOR =                               $102C;
  CL_DRIVER_VERSION =                              $102D;
  CL_DEVICE_PROFILE =                              $102E;
  CL_DEVICE_VERSION =                              $102F;
  CL_DEVICE_EXTENSIONS =                           $1030;
  CL_DEVICE_PLATFORM =                             $1031;
  CL_DEVICE_DOUBLE_FP_CONFIG =                     $1032;
  CL_DEVICE_HALF_FP_CONFIG =                       $1033;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF =          $1034;
  CL_DEVICE_HOST_UNIFIED_MEMORY =                  $1035;  (* deprecated *)
  CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR =             $1036;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT =            $1037;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_INT =              $1038;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG =             $1039;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT =            $103A;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE =           $103B;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF =             $103C;
  CL_DEVICE_OPENCL_C_VERSION =                     $103D;
  CL_DEVICE_LINKER_AVAILABLE =                     $103E;
  CL_DEVICE_BUILT_IN_KERNELS =                     $103F;
  CL_DEVICE_IMAGE_MAX_BUFFER_SIZE =                $1040;
  CL_DEVICE_IMAGE_MAX_ARRAY_SIZE =                 $1041;
  CL_DEVICE_PARENT_DEVICE =                        $1042;
  CL_DEVICE_PARTITION_MAX_SUB_DEVICES =            $1043;
  CL_DEVICE_PARTITION_PROPERTIES =                 $1044;
  CL_DEVICE_PARTITION_AFFINITY_DOMAIN =            $1045;
  CL_DEVICE_PARTITION_TYPE =                       $1046;
  CL_DEVICE_REFERENCE_COUNT =                      $1047;
  CL_DEVICE_PREFERRED_INTEROP_USER_SYNC =          $1048;
  CL_DEVICE_PRINTF_BUFFER_SIZE =                   $1049;
  CL_DEVICE_IMAGE_PITCH_ALIGNMENT =                $104A;
  CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT =         $104B;
  CL_DEVICE_MAX_READ_WRITE_IMAGE_ARGS =            $104C;
  CL_DEVICE_MAX_GLOBAL_VARIABLE_SIZE =             $104D;
  CL_DEVICE_QUEUE_ON_DEVICE_PROPERTIES =           $104E;
  CL_DEVICE_QUEUE_ON_DEVICE_PREFERRED_SIZE =       $104F;
  CL_DEVICE_QUEUE_ON_DEVICE_MAX_SIZE =             $1050;
  CL_DEVICE_MAX_ON_DEVICE_QUEUES =                 $1051;
  CL_DEVICE_MAX_ON_DEVICE_EVENTS =                 $1052;
  CL_DEVICE_SVM_CAPABILITIES =                     $1053;
  CL_DEVICE_GLOBAL_VARIABLE_PREFERRED_TOTAL_SIZE = $1054;
  CL_DEVICE_MAX_PIPE_ARGS =                        $1055;
  CL_DEVICE_PIPE_MAX_ACTIVE_RESERVATIONS =         $1056;
  CL_DEVICE_PIPE_MAX_PACKET_SIZE =                 $1057;
  CL_DEVICE_PREFERRED_PLATFORM_ATOMIC_ALIGNMENT =  $1058;
  CL_DEVICE_PREFERRED_GLOBAL_ATOMIC_ALIGNMENT =    $1059;
  CL_DEVICE_PREFERRED_LOCAL_ATOMIC_ALIGNMENT =     $105A;
  CL_DEVICE_IL_VERSION =                           $105B;
  CL_DEVICE_MAX_NUM_SUB_GROUPS =                   $105C;
  CL_DEVICE_SUB_GROUP_INDEPENDENT_FORWARD_PROGRESS = $105D;

  (* cl_device_fp_config - bitfield *)
  CL_FP_DENORM =                               (1 shl 0);
  CL_FP_INF_NAN =                              (1 shl 1);
  CL_FP_ROUND_TO_NEAREST =                     (1 shl 2);
  CL_FP_ROUND_TO_ZERO =                        (1 shl 3);
  CL_FP_ROUND_TO_INF =                         (1 shl 4);
  CL_FP_FMA =                                  (1 shl 5);
  CL_FP_SOFT_FLOAT =                           (1 shl 6);
  CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT =        (1 shl 7);

  (* cl_device_mem_cache_type *)
  CL_NONE =                                     $0;
  CL_READ_ONLY_CACHE =                          $1;
  CL_READ_WRITE_CACHE =                         $2;

  (* cl_device_local_mem_type *)
  CL_LOCAL =                                    $1;
  CL_GLOBAL =                                   $2;

  (* cl_device_exec_capabilities - bitfield *)
  CL_EXEC_KERNEL =                              (1 shl 0);
  CL_EXEC_NATIVE_KERNEL =                       (1 shl 1);

  (* cl_command_queue_properties - bitfield *)
  CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE =      (1 shl 0);
  CL_QUEUE_PROFILING_ENABLE =                   (1 shl 1);
  CL_QUEUE_ON_DEVICE =                          (1 shl 2);
  CL_QUEUE_ON_DEVICE_DEFAULT =                  (1 shl 3);

  (* cl_context_info *)
  CL_CONTEXT_REFERENCE_COUNT =                      $1080;
  CL_CONTEXT_DEVICES =                              $1081;
  CL_CONTEXT_PROPERTIES =                           $1082;
  CL_CONTEXT_NUM_DEVICES =                          $1083;

  (* cl_context_properties *)
  CL_CONTEXT_PLATFORM =                             $1084;
  CL_CONTEXT_INTEROP_USER_SYNC =                    $1085;
    
  (* cl_device_partition_property *)
  CL_DEVICE_PARTITION_EQUALLY =                     $1086;
  CL_DEVICE_PARTITION_BY_COUNTS =                   $1087;
  CL_DEVICE_PARTITION_BY_COUNTS_LIST_END =          $0;
  CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN =          $1088;
    
  (* cl_device_affinity_domain *)
  CL_DEVICE_AFFINITY_DOMAIN_NUMA =               (1 shl 0);
  CL_DEVICE_AFFINITY_DOMAIN_L4_CACHE =           (1 shl 1);
  CL_DEVICE_AFFINITY_DOMAIN_L3_CACHE =           (1 shl 2);
  CL_DEVICE_AFFINITY_DOMAIN_L2_CACHE =           (1 shl 3);
  CL_DEVICE_AFFINITY_DOMAIN_L1_CACHE =           (1 shl 4);
  CL_DEVICE_AFFINITY_DOMAIN_NEXT_PARTITIONABLE = (1 shl 5);
    
  (* cl_device_svm_capabilities *)
  CL_DEVICE_SVM_COARSE_GRAIN_BUFFER =            (1 shl 0);
  CL_DEVICE_SVM_FINE_GRAIN_BUFFER =              (1 shl 1);
  CL_DEVICE_SVM_FINE_GRAIN_SYSTEM =              (1 shl 2);
  CL_DEVICE_SVM_ATOMICS =                        (1 shl 3);

  (* cl_command_queue_info *)
  CL_QUEUE_CONTEXT =                             $1090;
  CL_QUEUE_DEVICE =                              $1091;
  CL_QUEUE_REFERENCE_COUNT =                     $1092;
  CL_QUEUE_PROPERTIES =                          $1093;
  CL_QUEUE_SIZE =                                $1094;
  CL_QUEUE_DEVICE_DEFAULT =                      $1095;

  (* cl_mem_flags - bitfield *)
  CL_MEM_READ_WRITE =                            (1 shl 0);
  CL_MEM_WRITE_ONLY =                            (1 shl 1);
  CL_MEM_READ_ONLY =                             (1 shl 2);
  CL_MEM_USE_HOST_PTR =                          (1 shl 3);
  CL_MEM_ALLOC_HOST_PTR =                        (1 shl 4);
  CL_MEM_COPY_HOST_PTR =                         (1 shl 5);
  (* reserved                                   (1 shl 6);  *)
  CL_MEM_HOST_WRITE_ONLY =                       (1 shl 7);
  CL_MEM_HOST_READ_ONLY =                        (1 shl 8);
  CL_MEM_HOST_NO_ACCESS =                        (1 shl 9);
  CL_MEM_SVM_FINE_GRAIN_BUFFER =                 (1 shl 10); (* used by cl_svm_mem_flags only *)
  CL_MEM_SVM_ATOMICS =                           (1 shl 11); (* used by cl_svm_mem_flags only *)
  CL_MEM_KERNEL_READ_AND_WRITE =                 (1 shl 12);

  (* cl_mem_migration_flags - bitfield *)
  CL_MIGRATE_MEM_OBJECT_HOST =                   (1 shl 0);
  CL_MIGRATE_MEM_OBJECT_CONTENT_UNDEFINED =      (1 shl 1);

  (* cl_channel_order *)
  CL_R =                                          $10B0;
  CL_A =                                          $10B1;
  CL_RG =                                         $10B2;
  CL_RA =                                         $10B3;
  CL_RGB =                                        $10B4;
  CL_RGBA =                                       $10B5;
  CL_BGRA =                                       $10B6;
  CL_ARGB =                                       $10B7;
  CL_INTENSITY =                                  $10B8;
  CL_LUMINANCE =                                  $10B9;
  CL_Rx =                                         $10BA;
  CL_RGx =                                        $10BB;
  CL_RGBx =                                       $10BC;
  CL_DEPTH =                                      $10BD;
  CL_DEPTH_STENCIL =                              $10BE;
  CL_sRGB =                                       $10BF;
  CL_sRGBx =                                      $10C0;
  CL_sRGBA =                                      $10C1;
  CL_sBGRA =                                      $10C2;
  CL_ABGR =                                       $10C3;

  //* cl_channel_type *//
  CL_SNORM_INT8 =                                 $10D0;
  CL_SNORM_INT16 =                                $10D1;
  CL_UNORM_INT8 =                                 $10D2;
  CL_UNORM_INT16 =                                $10D3;
  CL_UNORM_SHORT_565 =                            $10D4;
  CL_UNORM_SHORT_555 =                            $10D5;
  CL_UNORM_INT_101010 =                           $10D6;
  CL_SIGNED_INT8 =                                $10D7;
  CL_SIGNED_INT16 =                               $10D8;
  CL_SIGNED_INT32 =                               $10D9;
  CL_UNSIGNED_INT8 =                              $10DA;
  CL_UNSIGNED_INT16 =                             $10DB;
  CL_UNSIGNED_INT32 =                             $10DC;
  CL_HALF_FLOAT =                                 $10DD;
  CL_FLOAT =                                      $10DE;
  CL_UNORM_INT24 =                                $10DF;
  CL_UNORM_INT_101010_2 =                         $10E0;

  (* cl_mem_object_type *)
  CL_MEM_OBJECT_BUFFER =                          $10F0;
  CL_MEM_OBJECT_IMAGE2D =                         $10F1;
  CL_MEM_OBJECT_IMAGE3D =                         $10F2;
  CL_MEM_OBJECT_IMAGE2D_ARRAY =                   $10F3;
  CL_MEM_OBJECT_IMAGE1D =                         $10F4;
  CL_MEM_OBJECT_IMAGE1D_ARRAY =                   $10F5;
  CL_MEM_OBJECT_IMAGE1D_BUFFER =                  $10F6;
  CL_MEM_OBJECT_PIPE =                            $10F7;

  (* cl_mem_info *)
  CL_MEM_TYPE =                                   $1100;
  CL_MEM_FLAGS =                                  $1101;
  CL_MEM_SIZE =                                   $1102;
  CL_MEM_HOST_PTR =                               $1103;
  CL_MEM_MAP_COUNT =                              $1104;
  CL_MEM_REFERENCE_COUNT =                        $1105;
  CL_MEM_CONTEXT =                                $1106;
  CL_MEM_ASSOCIATED_MEMOBJECT =                   $1107;
  CL_MEM_OFFSET =                                 $1108;
  CL_MEM_USES_SVM_POINTER =                       $1109;

  (* cl_image_info *)
  CL_IMAGE_FORMAT =                               $1110;
  CL_IMAGE_ELEMENT_SIZE =                         $1111;
  CL_IMAGE_ROW_PITCH =                            $1112;
  CL_IMAGE_SLICE_PITCH =                          $1113;
  CL_IMAGE_WIDTH =                                $1114;
  CL_IMAGE_HEIGHT =                               $1115;
  CL_IMAGE_DEPTH =                                $1116;
  CL_IMAGE_ARRAY_SIZE =                           $1117;
  CL_IMAGE_BUFFER =                               $1118;
  CL_IMAGE_NUM_MIP_LEVELS =                       $1119;
  CL_IMAGE_NUM_SAMPLES =                          $111A;
    
  (* cl_pipe_info *)
  CL_PIPE_PACKET_SIZE =                           $1120;
  CL_PIPE_MAX_PACKETS =                           $1121;

  (* cl_addressing_mode *)
  CL_ADDRESS_NONE =                               $1130;
  CL_ADDRESS_CLAMP_TO_EDGE =                      $1131;
  CL_ADDRESS_CLAMP =                              $1132;
  CL_ADDRESS_REPEAT =                             $1133;
  CL_ADDRESS_MIRRORED_REPEAT =                    $1134;

  (* cl_filter_mode *)
  CL_FILTER_NEAREST =                             $1140;
  CL_FILTER_LINEAR =                              $1141;

  (* cl_sampler_info *)
  CL_SAMPLER_REFERENCE_COUNT =                    $1150;
  CL_SAMPLER_CONTEXT =                            $1151;
  CL_SAMPLER_NORMALIZED_COORDS =                  $1152;
  CL_SAMPLER_ADDRESSING_MODE =                    $1153;
  CL_SAMPLER_FILTER_MODE =                        $1154;
  CL_SAMPLER_MIP_FILTER_MODE =                    $1155;
  CL_SAMPLER_LOD_MIN =                            $1156;
  CL_SAMPLER_LOD_MAX =                            $1157;

  (* cl_map_flags - bitfield *)
  CL_MAP_READ =                                   (1 shl 0);
  CL_MAP_WRITE =                                  (1 shl 1);
  CL_MAP_WRITE_INVALIDATE_REGION  =               (1 shl 2);

  (* cl_program_info *)
  CL_PROGRAM_REFERENCE_COUNT =                    $1160;
  CL_PROGRAM_CONTEXT =                            $1161;
  CL_PROGRAM_NUM_DEVICES =                        $1162;
  CL_PROGRAM_DEVICES =                            $1163;
  CL_PROGRAM_SOURCE =                             $1164;
  CL_PROGRAM_BINARY_SIZES =                       $1165;
  CL_PROGRAM_BINARIES =                           $1166;
  CL_PROGRAM_NUM_KERNELS =                        $1167;
  CL_PROGRAM_KERNEL_NAMES =                       $1168;
  CL_PROGRAM_IL =                                 $1169;
  CL_PROGRAM_SCOPE_GLOBAL_CTORS_PRESENT =         $116A;
  CL_PROGRAM_SCOPE_GLOBAL_DTORS_PRESENT =         $116B;

  (* cl_program_build_info *)
  CL_PROGRAM_BUILD_STATUS =     			      $1181;
  CL_PROGRAM_BUILD_OPTIONS =                      $1182;
  CL_PROGRAM_BUILD_LOG =                          $1183;
  CL_PROGRAM_BINARY_TYPE =                        $1184;
  CL_PROGRAM_BUILD_GLOBAL_VARIABLE_TOTAL_SIZE =   $1185;

  (* cl_program_binary_type *)
  CL_PROGBRAM_BINARY_TYPE_NONE =                  $0;
  CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT =        $1;
  CL_PROGRAM_BINARY_TYPE_LIBRARY =                $2;
  CL_PROGRAM_BINARY_TYPE_EXECUTABLE =             $4;

  (* cl_build_status *)
  CL_BUILD_SUCCESS =                               0;
  CL_BUILD_NONE =                                 -1;
  CL_BUILD_ERROR =                                -2;
  CL_BUILD_IN_PROGRESS =                          -3;

  (* cl_kernel_info *)
  CL_KERNEL_FUNCTION_NAME =                       $1190;
  CL_KERNEL_NUM_ARGS =                            $1191;
  CL_KERNEL_REFERENCE_COUNT =                     $1192;
  CL_KERNEL_CONTEXT =                             $1193;
  CL_KERNEL_PROGRAM =                             $1194;
  CL_KERNEL_ATTRIBUTES =                          $1195;
  CL_KERNEL_MAX_NUM_SUB_GROUPS =                  $11B9;
  CL_KERNEL_COMPILE_NUM_SUB_GROUPS =              $11BA;

  (* cl_kernel_arg_info *)
  CL_KERNEL_ARG_ADDRESS_QUALIFIER =               $1196;
  CL_KERNEL_ARG_ACCESS_QUALIFIER =                $1197;
  CL_KERNEL_ARG_TYPE_NAME =                       $1198;
  CL_KERNEL_ARG_TYPE_QUALIFIER =                  $1199;
  CL_KERNEL_ARG_NAME =                            $119A;

  (* cl_kernel_arg_address_qualifier *)
  CL_KERNEL_ARG_ADDRESS_GLOBAL =                  $119B;
  CL_KERNEL_ARG_ADDRESS_LOCAL =                   $119C;
  CL_KERNEL_ARG_ADDRESS_CONSTANT =                $119D;
  CL_KERNEL_ARG_ADDRESS_PRIVATE =                 $119E;

  (* cl_kernel_arg_access_qualifier *)
  CL_KERNEL_ARG_ACCESS_READ_ONLY =                $11A0;
  CL_KERNEL_ARG_ACCESS_WRITE_ONLY =               $11A1;
  CL_KERNEL_ARG_ACCESS_READ_WRITE =               $11A2;
  CL_KERNEL_ARG_ACCESS_NONE =                     $11A3;

  (* cl_kernel_arg_type_qualifer *)
  CL_KERNEL_ARG_TYPE_NONE =                       0;
  CL_KERNEL_ARG_TYPE_CONST =                   	  (1 shl 0);
  CL_KERNEL_ARG_TYPE_RESTRICT =                   (1 shl 1);
  CL_KERNEL_ARG_TYPE_VOLATILE =                   (1 shl 2);
  CL_KERNEL_ARG_TYPE_PIPE =                       (1 shl 3);

  (* cl_kernel_work_group_info *)
  CL_KERNEL_WORK_GROUP_SIZE =                     $11B0;
  CL_KERNEL_COMPILE_WORK_GROUP_SIZE =             $11B1;
  CL_KERNEL_LOCAL_MEM_SIZE =                      $11B2;
  CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE =  $11B3;
  CL_KERNEL_PRIVATE_MEM_SIZE =                    $11B4;
  CL_KERNEL_GLOBAL_WORK_SIZE =                    $11B5;

  (* cl_kernel_sub_group_info *)
  CL_KERNEL_MAX_SUB_GROUP_SIZE_FOR_NDRANGE =      $2033;
  CL_KERNEL_SUB_GROUP_COUNT_FOR_NDRANGE =         $2034;
  CL_KERNEL_LOCAL_SIZE_FOR_SUB_GROUP_COUNT =      $11B8;

  (* cl_kernel_exec_info *)
  CL_KERNEL_EXEC_INFO_SVM_PTRS =                  $11B6;
  CL_KERNEL_EXEC_INFO_SVM_FINE_GRAIN_SYSTEM =     $11B7;
  (* cl_event_info  *)
  CL_EVENT_COMMAND_QUEUE =                        $11D0;
  CL_EVENT_COMMAND_TYPE =                         $11D1;
  CL_EVENT_REFERENCE_COUNT =                      $11D2;
  CL_EVENT_COMMAND_EXECUTION_STATUS =             $11D3;
  CL_EVENT_CONTEXT =                              $11D4;

  (* cl_command_type *)
  CL_COMMAND_NDRANGE_KERNEL =                     $11F0;
  CL_COMMAND_TASK =                               $11F1;
  CL_COMMAND_NATIVE_KERNEL =                      $11F2;
  CL_COMMAND_READ_BUFFER =                        $11F3;
  CL_COMMAND_WRITE_BUFFER =                       $11F4;
  CL_COMMAND_COPY_BUFFER =                        $11F5;
  CL_COMMAND_READ_IMAGE =                         $11F6;
  CL_COMMAND_WRITE_IMAGE =                        $11F7;
  CL_COMMAND_COPY_IMAGE =                         $11F8;
  CL_COMMAND_COPY_IMAGE_TO_BUFFER =               $11F9;
  CL_COMMAND_COPY_BUFFER_TO_IMAGE =               $11FA;
  CL_COMMAND_MAP_BUFFER =                         $11FB;
  CL_COMMAND_MAP_IMAGE =                          $11FC;
  CL_COMMAND_UNMAP_MEM_OBJECT =                   $11FD;
  CL_COMMAND_MARKER =                             $11FE;
  CL_COMMAND_ACQUIRE_GL_OBJECTS =                 $11FF;
  CL_COMMAND_RELEASE_GL_OBJECTS =                 $1200;
  CL_COMMAND_READ_BUFFER_RECT =                   $1201;
  CL_COMMAND_WRITE_BUFFER_RECT =                  $1202;
  CL_COMMAND_COPY_BUFFER_RECT =                   $1203;
  CL_COMMAND_USER =                               $1204;
  CL_COMMAND_BARRIER =                            $1205;
  CL_COMMAND_MIGRATE_MEM_OBJECTS =                $1206;
  CL_COMMAND_FILL_BUFFER =                        $1207;
  CL_COMMAND_FILL_IMAGE =                         $1208;
  CL_COMMAND_SVM_FREE =                           $1209;
  CL_COMMAND_SVM_MEMCPY =                         $120A;
  CL_COMMAND_SVM_MEMFILL =                        $120B;
  CL_COMMAND_SVM_MAP =                            $120C;
  CL_COMMAND_SVM_UNMAP =                          $120D;

  (* command execution status *)
  CL_COMPLETE =                                   $0;
  CL_RUNNING =                                    $1;
  CL_SUBMITTED =                                  $2;
  CL_QUEUED =                                     $3;

  (* cl_buffer_create_type  *)
  CL_BUFFER_CREATE_TYPE_REGION =                  $1220;

  (* cl_profiling_info *)
  CL_PROFILING_COMMAND_QUEUED =                   $1280;
  CL_PROFILING_COMMAND_SUBMIT =                   $1281;
  CL_PROFILING_COMMAND_START =                    $1282;
  CL_PROFILING_COMMAND_END =                      $1283;
  CL_PROFILING_COMMAND_COMPLETE =                 $1284;

(*************************************************************************)
(*** Functions                                                         ***)
(*************************************************************************)

  (* Platform API *)
  function clGetPlatformIDs(num_entries: Tcl_uint;
    platforms: Pcl_platform_id;
    num_platforms: Pcl_uint): Tcl_int;     // CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetPlatformInfo(cl_platform: Tcl_platform_id;
    param_name: Tcl_platform_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int;  // CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  //* Device APIs *//
  function clGetDeviceIDs(_platform: Pcl_platform_id;
    device_type: Tcl_device_type;
    num_entries: Tcl_uint;
    devices: Pcl_device_id;
    num_devices: Pcl_uint): Tcl_int;  // CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetDeviceInfo(device: Pcl_device_id;
    param_name: Tcl_device_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int;  // CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clCreateSubDevices(in_device: Pcl_device_id;
    properties: Pcl_device_partition_property;
    num_devices: Tcl_uint;
    out_devices: Pcl_device_id;
  	num_devices_ret: Pcl_uint): Tcl_int;    // CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

  function clRetainDevice(device : Pcl_device_id): Tcl_int; // CL_API_SUFFIX__VERSION_1_2
    stdcall; external LibOpenCL;

  function clReleaseDevice(device : Pcl_device_id): Tcl_int; // CL_API_SUFFIX__VERSION_1_2
    stdcall; external LibOpenCL;

  function clSetDefaultDeviceCommandQueue(context: Pcl_context;
    device: Pcl_device_id;
    command_queue: Pcl_command_queue): Tcl_int; //CL_API_SUFFIX__VERSION_2_1
  stdcall; external LibOpenCL;

  function clGetDeviceAndHostTimer(device: Pcl_device_id;
    device_timestamp: Pcl_ulong;
    host_timestamp: Pcl_ulong): Tcl_int;  //CL_API_SUFFIX__VERSION_2_1
  stdcall; external LibOpenCL;

  function clGetHostTimer(device: Pcl_device_id;
    host_timestamp: Pcl_ulong): Tcl_int;  //CL_API_SUFFIX__VERSION_2_1
  stdcall; external LibOpenCL;

  //* Context APIs *//
type
  Tcl_context_notify = procedure(errinfo: PAnsiChar;
    private_info: Pointer;
    size: NativeUInt;
    user_data: Pointer);
   stdcall;


  function clCreateContext(properties: Pcl_context_properties;
    num_devices: Tcl_uint;
    devices: Pcl_device_id;
    pfn_notify: Tcl_context_notify; {const char *, const void *, size_t, void *}
    user_data: Pointer;
    errcode_ret: Pcl_int): Pcl_context;   // CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clCreateContextFromType(properties: Pcl_context_properties;
    device_type: Tcl_device_type;
    pfn_notify: Tcl_context_notify; {const char *, const void *, size_t, void *}
    user_data: Pointer;
    errcode_ret: Pcl_int): Pcl_context;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clRetainContext(context: Pcl_context): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clReleaseContext(context: Pcl_context): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetContextInfo(context: Pcl_context;
    param_name: Tcl_context_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  //* Command Queue APIs *//
  function clCreateCommandQueue(context: Pcl_context;
    device: Pcl_device_id;
    properties: Tcl_command_queue_properties;
    errcode_ret: Pcl_int): Pcl_command_queue; //CL_API_SUFFIX__VERSION_2_0
   stdcall; external LibOpenCL;

  function clRetainCommandQueue(command_queue: Pcl_command_queue): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clReleaseCommandQueue(command_queue: Pcl_command_queue): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
  stdcall; external LibOpenCL;

  function clGetCommandQueueInfo(command_queue: Pcl_command_queue;
    param_name: Tcl_command_queue_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
  stdcall; external LibOpenCL;

  //* Memory Object APIs *//
  function clCreateBuffer(context: Pcl_context;
    flags: Tcl_mem_flags;
    size: NativeUInt;
    host_ptr: Pointer;
    errcode_ret: Pcl_int): Pcl_mem;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clCreateSubBuffer(buffer: Pcl_mem;
    flags: Tcl_mem_flags;
    buffer_create_type: Tcl_buffer_create_type;
    buffer_create_info: Pointer;
    errcode_ret: Pcl_int): Pcl_mem;   //CL_API_SUFFIX__VERSION_1_1
   stdcall; external LibOpenCL;

  function clCreateImage(context: Pcl_context;
    flags: Tcl_mem_flags;
    image_format: Pcl_image_format;
    image_desc: Pcl_image_desc;
    host_ptr: Pointer;
    errcode_ret: Pcl_int): Pcl_mem; //CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

  function clCreatePipe(context: Pcl_context;
    flags: Tcl_mem_flags;
    pipe_packet_size: Tcl_uint;
    pipe_max_packets: Tcl_uint;
    properties: Pcl_pipe_properties;
    errcode_ret: Pcl_int): Pcl_mem;   //CL_API_SUFFIX__VERSION_2_0
   stdcall; external LibOpenCL;

  function clRetainMemObject(memobj: Pcl_mem): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clReleaseMemObject(memobj: Pcl_mem): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetSupportedImageFormats(context: Pcl_context;
    flags: Tcl_mem_flags;
    image_type: Tcl_mem_object_type;
    num_entries: Tcl_uint;
    image_formats: Pcl_image_format;
    num_image_formats: Pcl_uint): Tcl_int;   //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetMemObjectInfo(memobj: Pcl_mem;
    param_name: Tcl_mem_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetImageInfo(image: Pcl_mem;
    param_name: Tcl_image_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetPipeInfo(pipe: Pcl_mem;
    param_name: Tcl_pipe_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_2_0
   stdcall; external LibOpenCL;

type
  Tcl_destructor_notify = procedure(memobj: Pcl_mem;
    user_data: Pointer);
   stdcall;

  function clSetMemObjectDestructorCallback(memobj: Pcl_mem;
     pfn_notify:   Tcl_destructor_notify; //( cl_mem /* memobj */, void* /*user_data*/),
     user_data: Pointer): Tcl_int;     //CL_API_SUFFIX__VERSION_1_1
   stdcall; external LibOpenCL;

   //* SVM Allocation APIs *//
   function clSVMAlloc(context: Pcl_context;
     flags: Tcl_svm_mem_flags;
     size: NativeUInt;
     alignment: Tcl_uint): Pointer; //CL_API_SUFFIX__VERSION_2_0
   stdcall; external LibOpenCL;

   procedure clSVMFree(context: Pcl_context;
     svm_pointer: Pointer);     //CL_API_SUFFIX__VERSION_2_0
   stdcall; external LibOpenCL;

  //* Sampler APIs *//
  function clCreateSamplerWithProperties(context: Pcl_context;
    normalized_coords: Pcl_sampler_properties;
    addressing_mode: Tcl_addressing_mode;
    errcode_ret: Pcl_int): Pcl_sampler;   //CL_API_SUFFIX__VERSION_2_0
   stdcall; external LibOpenCL;

  function clRetainSampler(sampler: Pcl_sampler): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clReleaseSampler(sampler: Pcl_sampler): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetSamplerInfo(sampler: Pcl_sampler;
    param_name: Tcl_sampler_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  (* Program Object APIs *)
  function clCreateProgramWithSource(context: Pcl_context;
    count: Tcl_uint;
    strings: PPAnsiChar;
    lengths: Psize_t;
    errcode_ret: Pcl_int): Pcl_program;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clCreateProgramWithBinary(context: Pcl_context;
    num_devices: Tcl_uint;
    device_list: Pcl_device_id;
    lengths: Psize_t;
    binaries: PByte;
    binary_status: Pcl_int;
    errcode_ret: Pcl_int): Pcl_program;   //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clCreateProgramWithBuiltInKernels(context: Pcl_context;
    num_devices: Tcl_uint;
    device_list: Pcl_device_id;
    kernel_names: Pcl_char;
    errcode_ret: Pcl_int): Pcl_program; //CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

  function clCreateProgramWithIL(context: Pcl_context;
    il: Pointer;
    length: NativeUInt;
    errcode_ret: Pcl_int): Pcl_program; //CL_API_SUFFIX__VERSION_2_1
   stdcall; external LibOpenCL;

  function clRetainProgram(_program: Pcl_program): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clReleaseProgram(_program: Pcl_program): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

type
  Tcl_programbuilt_notify = procedure(_program: Pcl_program;
    user_data: Pointer);
   stdcall;

  function clBuildProgram(_program: Pcl_program;
    num_devices: Tcl_uint;
    device_list: Pcl_device_id;
    options: Pcl_char;
    pfn_notify: Tcl_programbuilt_notify;
    user_data: Pointer): Tcl_int;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clCompileProgram(_program: Pcl_program;
    num_devices: Tcl_uint;
    device_list: Pcl_device_id;
    options: Pcl_char;
  	num_input_headers: Tcl_uint;
  	input_headers: Pcl_program;
  	header_include_names: PPAnsiChar;
    pfn_notify: Tcl_programbuilt_notify;
    user_data: Pointer): Tcl_int; //CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

  function clLinkProgram(context: Pcl_context;
    num_devices: Tcl_uint;
    device_list: Pcl_device_id;
    options: Pcl_char;
    num_input_programs: Pcl_uint;
    input_programs: Pcl_program;
    pfn_notify: Tcl_programbuilt_notify; //(cl_program /* program */, void * /* user_data */)
    user_data: Pointer;
    errcode_ret: Pcl_int): Pcl_program; //CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

  function clUnloadPlatformCompiler: Tcl_int; //CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

  function clGetProgramInfo(_program: Pcl_program;
    param_name: Tcl_program_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetProgramBuildInfo(_program: Pcl_program;
    device: Pcl_device_id;
    param_name: Tcl_program_build_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  (* Kernel Object APIs *)
  function clCreateKernel(_program: Pcl_program;
    kernel_name: PAnsiChar;
    errcode_ret: Pcl_int): Pcl_kernel; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clCreateKernelsInProgram(_program: Pcl_program;
    num_kernels: Tcl_uint;
    kernels: Pcl_kernel;
    num_kernels_ret: Pcl_uint): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clCloneKernel(source_kernel: Pcl_kernel;
    errocode_ret: Pcl_int): Tcl_int;   //CL_API_SUFFIX__VERSION_2_1
   stdcall; external LibOpenCL;

  function clRetainKernel(kernel: Pcl_kernel): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clReleaseKernel(kernel: Pcl_kernel): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clSetKernelArg(kernel: Pcl_kernel;
    arg_index: Tcl_uint;
    arg_size: NativeUInt;
    arg_value: Pointer): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clSetKernelArgSVMPointer(kernel: Pcl_kernel;
    arg_index: Tcl_uint;
    arg_value: Pointer): Tcl_int; //CL_API_SUFFIX__VERSION_2_0
   stdcall; external LibOpenCL;

  function clSetKernelExecInfo(kernel: Pcl_kernel;
    param_name: Tcl_kernel_exec_info;
    param_value_size: NativeUInt;
    param_value: Pointer): Tcl_int; //CL_API_SUFFIX__VERSION_2_0
   stdcall; external LibOpenCL;

  function clGetKernelInfo(kernel: Pcl_kernel;
    param_name: Tcl_kernel_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetKernelArgInfo(kernel: Pcl_kernel;
    arg_indx: Tcl_uint;
    param_name: Tcl_kernel_arg_info;
	param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

  function clGetKernelWorkGroupInfo(kernel: Pcl_kernel;
    device: Pcl_device_id;
    param_name: Tcl_kernel_work_group_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetKernelSubGroupInfo(kernel: Pcl_kernel;
    device: Pcl_device_id;
    param_name: Tcl_kernel_sub_group_info;
    input_value_size: NativeUInt;
    input_value: Pointer;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  //* Event Object APIs *//
  function clWaitForEvents(num_events: Tcl_uint;
    event_list: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clGetEventInfo(event: Pcl_event;
    param_name: Tcl_event_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int;   //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clCreateUserEvent(context: Pcl_context;
    errcode_ret: Pcl_int): Pcl_event;   //CL_API_SUFFIX__VERSION_1_1
   stdcall; external LibOpenCL;

  function clRetainEvent(event: Pcl_event): Tcl_int;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clReleaseEvent(event: Pcl_event): Tcl_int;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clSetUserEventStatus(event: Pcl_event;
    execution_status: Tcl_int): Tcl_int; //CL_API_SUFFIX__VERSION_1_1
   stdcall; external LibOpenCL;

 type
  Tcl_event_notify = procedure(event: Pcl_event;
    num_event: Tcl_int;
    user_data: Pointer);
   stdcall;

  function clSetEventCallback(event: Pcl_event;
    command_exec_callback_type: Tcl_int;
    pfn_notify: Tcl_event_notify;
    user_data: Pointer): Tcl_int; //CL_API_SUFFIX__VERSION_1_1
   stdcall; external LibOpenCL;

  (* Profiling APIs *)
  function clGetEventProfilingInfo(event: Pcl_event;
    param_name: Tcl_profiling_info;
    param_value_size: NativeUInt;
    param_value: Pointer;
    param_value_size_ret: Psize_t): Tcl_int;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  //* Flush and Finish APIs *//
  function clFlush(command_queue: Pcl_command_queue): Tcl_int;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clFinish(command_queue: Pcl_command_queue): Tcl_int;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  //* Enqueued Commands APIs *//
  function clEnqueueReadBuffer(command_queue: Pcl_command_queue;
    buffer: Pcl_mem;
    blocking_read: Tcl_bool;
    offset: NativeUInt;
    size: NativeUInt;
    ptr: Pointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueReadBufferRect(command_queue: Pcl_command_queue;
    buffer: Pcl_mem;
    blocking_read: Tcl_bool;
    buffer_offset: Psize_t;
    host_offset: Psize_t;
    region: Psize_t;
    buffer_row_pitch: NativeUInt;
    buffer_slice_pitch: NativeUInt;
    host_row_pitch: NativeUInt;
    host_slice_pitch: NativeUInt;
    ptr: Pointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int;  //CL_API_SUFFIX__VERSION_1_1
   stdcall; external LibOpenCL;

  function clEnqueueWriteBuffer(command_queue: Pcl_command_queue;
    buffer: Pcl_mem;
    blocking_write: Tcl_bool;
    offset: NativeUInt;
    size: NativeUInt;
    ptr: Pointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueWriteBufferRect(command_queue: Pcl_command_queue;
    buffer: Pcl_mem;
    blocking_write: Tcl_bool;
    buffer_offset: Psize_t;
    host_offset: Psize_t;
    region: Psize_t;
    buffer_row_pitch: NativeUInt;
    buffer_slice_pitch: NativeUInt;
    host_row_pitch: NativeUInt;
    host_slice_pitch: NativeUInt;
    ptr: Pointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_1
   stdcall; external LibOpenCL;

  function clEnqueueFillBuffer(command_queue: Pcl_command_queue;
    buffer: Pcl_mem;
    pattern: Pointer;
    pattern_size: NativeUInt;
    offset: NativeUInt;
    size: NativeUInt;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

  function clEnqueueCopyBuffer(command_queue: Pcl_command_queue;
    src_buffer: Pcl_mem;
    dst_buffer: Pcl_mem;
    src_offset: NativeUInt;
    dst_offset: NativeUInt;
    size: NativeUInt;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueCopyBufferRect(command_queue: Pcl_command_queue;
    src_buffer: Pcl_mem;
    dst_buffer: Pcl_mem;
    src_offset: NativeUInt;
    dst_offset: NativeUInt;
    region: NativeUInt;
    src_row_pitch: NativeUInt;
    src_slice_pitch: NativeUInt;
    dst_row_pitch: NativeUInt;
    dst_slice_pitch: NativeUInt;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_1
   stdcall; external LibOpenCL;

  function clEnqueueReadImage(command_queue: Pcl_command_queue;
    image: Pcl_mem;
    blocking_read: Tcl_bool;
    origin: Psize_t; // x3
    region: Psize_t; // x3
    row_pitch: NativeUInt;
    slice_pitch: NativeUInt;
    ptr: Pointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int;  // CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueWriteImage(command_queue: Pcl_command_queue;
    image: Pcl_mem;
    blocking_write: Tcl_bool;
    origin: Psize_t; // x3
    region: Psize_t; // x3
    input_row_pitch: NativeUInt;
    input_slice_pitch: NativeUInt;
    ptr: Pointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int;  // CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;


  function clEnqueueFillImage(command_queue: Pcl_command_queue;
    image: Pcl_mem;
    fill_color: Pointer;
    origin: Psize_t; //x3
    region: Psize_t; //x3
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; // CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

  function clEnqueueCopyImage(command_queue: Pcl_command_queue;
    src_image: Pcl_mem;
    dst_image: Pcl_mem;
    src_origin: Psize_t; //x3
    dst_origin: Psize_t; //x3
    region: Psize_t; //x3
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int;  // CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueCopyImageToBuffer(command_queue: Pcl_command_queue;
    src_image: Pcl_mem;
    dst_buffer: Pcl_mem;
    src_origin: Psize_t; //x3
    region: Psize_t; //x3
    dst_offset: NativeUInt;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; // CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueCopyBufferToImage(command_queue: Pcl_command_queue;
    src_buffer: Pcl_mem;
    dst_image: Pcl_mem;
    src_offset: NativeUInt;
    dst_origin: Psize_t; //x3
    region: Psize_t; //x3
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueMapBuffer(command_queue: Pcl_command_queue;
    buffer: Pcl_mem;
    blocking_map: Pcl_bool;
    map_flags: Tcl_map_flags;
    offset: NativeUInt;
    cb: NativeUInt;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event;
    errcode_ret: Pcl_int): Pointer; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueMapImage(command_queue: Pcl_command_queue;
    image: Pcl_mem;
    blocking_map: Tcl_bool;
    map_flags: Tcl_map_flags;
    origin: Psize_t; //x3
    region: Psize_t; //x3
    image_row_pitch: Psize_t;
    image_slice_pitch: Psize_t;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event;
    errcode_ret: Pcl_int): Pointer;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueUnmapMemObject(command_queue: Pcl_command_queue;
    memobj: Pcl_mem;
    mapped_ptr: Pointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueMigrateMemObjects(command_queue: Pcl_command_queue;
    num_mem_objects: Tcl_uint;
  	mem_objects: Pcl_mem;
    flags: Tcl_mem_flags;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int;  //CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

  function clEnqueueNDRangeKernel(command_queue: Pcl_command_queue;
    kernel: Pcl_kernel;
    work_dim: Tcl_uint;
    global_work_offset: Psize_t;
    global_work_size: Psize_t;
    local_work_size: Psize_t;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int;  //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

type
  Tcl_EnqueueNativeKernel = procedure();
   stdcall;

  function clEnqueueNativeKernel(command_queue: Pcl_command_queue;
    user_func: Tcl_EnqueueNativeKernel;
    args: Pointer;
    cb_args: NativeUInt;
    num_mem_objects: Tcl_uint;
    mem_list: Pcl_mem;
    args_mem_loc: PPointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueMarkerWithWaitList(command_queue: Pcl_command_queue;
    num_events_in_wait_list: Tcl_uint;
	event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

  function clEnqueueBarrierWithWaitList(command_queue: Pcl_command_queue;
    num_events_in_wait_list: Tcl_uint;
	event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;

type
  Tcl_EnqueueSVM_fn = procedure();
   stdcall;

  function clEnqueueSVMFree(command_queue: Pcl_command_queue;
    num_svm_pointers: Tcl_uint;
    args: Pointer;
    pfn_free_func: Tcl_EnqueueSVM_fn;
    user_data: Pointer;
    mem_list: Pcl_mem;
    args_mem_loc: PPointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_1_0
   stdcall; external LibOpenCL;

  function clEnqueueSVMMemcpy(command_queue: Pcl_command_queue;
    num_svm_pointers: Tcl_uint;
    args: Pointer;
    user_data: Pointer;
    mem_list: Pcl_mem;
    args_mem_loc: PPointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_2_0
   stdcall; external LibOpenCL;

  function clEnqueueSVMMap(command_queue: Pcl_command_queue;
    num_svm_pointers: Tcl_uint;
    args: Pointer;
    user_data: Pointer;
    mem_list: Pcl_mem;
    args_mem_loc: PPointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_2_0
   stdcall; external LibOpenCL;

  function clEnqueueSVMUnmap(command_queue: Pcl_command_queue;
    svm_ptr: Pointer;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_2_0
   stdcall; external LibOpenCL;

  function clEnqueueSVMmigrateMem(command_queue: Pcl_command_queue;
    num_svm_pointers: Tcl_uint;
    svm_pointers: PPointer;
    sizes: Psize_t;
    flags: Tcl_mem_flags;  //cl_mem_migration_flags;
    num_events_in_wait_list: Tcl_uint;
    event_wait_list: Pcl_event;
    event: Pcl_event): Tcl_int; //CL_API_SUFFIX__VERSION_2_1
   stdcall; external LibOpenCL;

  function clEnqueueWaitForEvents(command_queue: Pcl_command_queue;
    num_events: Tcl_uint;
    event_list: Pcl_event): Tcl_int;
   stdcall; external LibOpenCL;

  function clEnqueueBarrier(command_queue: Pcl_command_queue): Tcl_int;
   stdcall; external LibOpenCL;

  //* Extension function access
  //*
  //* Returns the extension function address for the given function name,
  //* or NULL if a valid function can not be found.  The client must
  //* check to make sure the address is not NULL, before using or
  //* calling the returned function address.
  //*

  function clGetExtensionFunctionAddressForPlatform(_platform: Pcl_platform_id;
    func_name: Pcl_char): Pointer;  //CL_API_SUFFIX__VERSION_1_2
   stdcall; external LibOpenCL;


   //* Deprecated OpenCL 1.1 APIs *//
   // clCreateImage2D();
   // clCreateImage3D();
   // clEnqueueMarker();
   // clEnqueueWaitForEvents();
   // clEnqueueBarrier();
   // clUnloadCompiler();
   // clGetExtensionFunctionAddress();
   
   //* Deprecated OpenCL 2.0 APIs *//
   
   // clCreateCommandQueue();
   // clCreateSampler();
   // clEnqueueTask();
//-------------------------------------------------------------------------

function InitOpenCL: Boolean;
procedure CloseOpenCL;
function InitFromLibraryOpenCL(const CLName: WideString): Boolean;
function IsInitializedOpenCL: Boolean;

//---------------------------------------------------------------------
implementation
//---------------------------------------------------------------------

const
  INVALID_MODULEHANDLE = 0;

// ************** Windows specific ********************
{$IFDEF MSWINDOWS}
var
  CLHandle: HINST;
{$ENDIF}

// ************** UNIX specific ********************
{$IFDEF UNIX}
var
  CLHandle: TLibHandle;
{$ENDIF}

//---------------------------------------------------

function GetProcAddressOpenCL(ProcName: PAnsiChar): Pointer;
begin
  result := GetProcAddress(Cardinal(CLHandle), ProcName);
end;

function InitOpenCL: Boolean;
begin
  if CLHandle = INVALID_MODULEHANDLE then
    Result := InitFromLibraryOpenCL(LibOpenCL)
  else
    Result := True;
end;

procedure CloseOpenCL;
begin
  if CLHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(Cardinal(CLHandle));
    CLHandle := INVALID_MODULEHANDLE;
  end;
end;

function InitFromLibraryOpenCL(const CLName: WideString): Boolean;
begin
  Result := False;
  CloseOpenCL;
  CLHandle := LoadLibraryW(PWideChar(CLName));
  if CLHandle = INVALID_MODULEHANDLE then
    Exit;


  Result := True;
end;

function IsInitializedOpenCL: Boolean;
begin
  Result := (CLHandle <> INVALID_MODULEHANDLE);
end;

end.

