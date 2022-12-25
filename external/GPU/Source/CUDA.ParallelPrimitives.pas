//
// The graphics rendering engine GLScene http://glscene.org
//
unit CUDA.ParallelPrimitives;

(* CUDA Parallel Primitives *)

// -------------------------------------------------------------
// cuDPP -- CUDA Data Parallel Primitives library
// -------------------------------------------------------------
// $Revision: 4567 $
// $Date: 2020-05-17
// -------------------------------------------------------------
// This source code is distributed under the terms of license.txt in
// the root directory of this source distribution.
// -------------------------------------------------------------

interface

const
  CUDPPDLL = 'cudpp32.dll';
  CUDPP_INVALID_HANDLE = $C0DABAD1;

type

  TCUDPPResult = (
    CUDPP_SUCCESS, // No error.
    CUDPP_ERROR_INVALID_HANDLE, // Specified handle (for example,
    //          to a plan) is invalid.
    CUDPP_ERROR_ILLEGAL_CONFIGURATION, // Specified configuration is
    //     illegal. For example, an
    //     invalid or illogical
    //     combination of options.
    CUDPP_ERROR_UNKNOWN // Unknown or untraceable error.
    );

  TCUDPPOption = (
    CUDPP_OPTION_FORWARD, // Algorithms operate forward:
    // from start to end of input
    // array
    CUDPP_OPTION_BACKWARD, // Algorithms operate backward:
    // from end to start of array
    CUDPP_OPTION_EXCLUSIVE, // Exclusive (for scans) - scan
    // includes all elements up to (but
    // not including) the current
    // element
    CUDPP_OPTION_INCLUSIVE, // Inclusive (for scans) - scan
    // includes all elements up to and
    // including the current element
    CUDPP_OPTION_CTA_LOCAL, // Algorithm performed only on
    // the CTAs (blocks) with no
    // communication between blocks.
    // @todo Currently ignored.
    CUDPP_OPTION_KEYS_ONLY, // No associated value to a key
    // (for global radix sort)
    CUDPP_OPTION_KEY_VALUE_PAIRS // Each key has an associated value
    );

  TCUDPPDatatype = (
    CUDPP_CHAR, // Character type (C char)
    CUDPP_UCHAR, // Unsigned character (byte) type (C unsigned char)
    CUDPP_INT, // Integer type (C int)
    CUDPP_UINT, // Unsigned integer type (C unsigned int)
    CUDPP_FLOAT // Float type (C float)
    );

  TCUDPPOperator = (
    CUDPP_ADD, // Addition of two operands
    CUDPP_MULTIPLY, // Multiplication of two operands
    CUDPP_MIN, // Minimum of two operands
    CUDPP_MAX // Maximum of two operands
    );

  TCUDPPAlgorithm = (
    CUDPP_SCAN,
    CUDPP_SEGMENTED_SCAN,
    CUDPP_COMPACT,
    CUDPP_REDUCE,
    CUDPP_SORT_RADIX,
    CUDPP_SPMVMULT, // Sparse matrix-dense vector multiplication
    CUDPP_RAND_MD5, // Pseudo Random Number Generator using MD5 hash algorithm
    CUDPP_ALGORITHM_INVALID // Placeholder at end of enum
    );

  TCUDPPConfiguration = record
    algorithm: TCUDPPAlgorithm; // The algorithm to be used
    op: TCUDPPOperator; // The numerical operator to be applied
    datatype: TCUDPPDatatype; // The datatype of the input arrays
    options: TCUDPPoption; // Options to configure the algorithm
  end;

  TCUDPPHandle = NativeUInt;

  // Plan allocation (for scan, sort, and compact)

function cudppPlan(var planHandle: TCUDPPHandle;
  config: TCUDPPConfiguration;
  n: NativeUInt;
  rows: NativeUInt;
  rowPitch: NativeUInt): TCUDPPResult;stdcall;external CUDPPDLL;

function cudppDestroyPlan(plan: TCUDPPHandle): TCUDPPResult;stdcall;external CUDPPDLL;

// Scan and sort algorithms

function cudppScan(planHandle: TCUDPPHandle;
  var d_out;
  var d_in,
  numElements: NativeUInt): TCUDPPResult;stdcall;external CUDPPDLL;

function cudppMultiScan(planHandle: TCUDPPHandle;
  var d_out;
  var d_in;
  numElements: NativeUInt;
  numRows: NativeUInt): TCUDPPResult;stdcall;external CUDPPDLL;

function cudppSegmentedScan(planHandle: TCUDPPHandle;
  var d_out;
  var d_idata;
  const d_iflags: PCardinal;
  numElements: NativeUInt): TCUDPPResult;stdcall;external CUDPPDLL;

function cudppCompact(planHandle: TCUDPPHandle;
  var d_out;
  var d_numValidElements: NativeUInt;
  var d_in;
  const d_isValid: PCardinal;
  numElements: NativeUInt): TCUDPPResult;stdcall;external CUDPPDLL;

function cudppSort(planHandle: TCUDPPHandle;
  var d_keys;
  var d_values;
  keybits: Integer;
  numElements: NativeUInt): TCUDPPResult;stdcall;external CUDPPDLL;

// Sparse matrix allocation

function cudppSparseMatrix(var sparseMatrixHandle: TCUDPPHandle;
  config: TCUDPPConfiguration;
  n: NativeUInt;
  rows: NativeUInt;
  var A;
  const h_rowIndices: PCardinal;
  const h_indices: PCardinal): TCUDPPResult;stdcall;external CUDPPDLL;

function cudppDestroySparseMatrix(sparseMatrixHandle: TCUDPPHandle):
  TCUDPPResult;stdcall;external CUDPPDLL;

// Sparse matrix-vector algorithms

function cudppSparseMatrixVectorMultiply(sparseMatrixHandle: TCUDPPHandle;
  var d_y;
  var d_x): TCUDPPResult;stdcall;external CUDPPDLL;

// random number generation algorithms
function cudppRand(planHandle: TCUDPPHandle;
  var d_out;
  numElements: NativeUInt): TCUDPPResult;stdcall;external CUDPPDLL;

function cudppRandSeed(const planHandle: TCUDPPHandle;
  seed: Cardinal): TCUDPPResult;stdcall;external CUDPPDLL;

//-------------------------------------
implementation
//-------------------------------------

end.

