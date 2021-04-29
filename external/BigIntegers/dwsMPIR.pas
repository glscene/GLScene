{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{
    This unit exposes MPIR dll (http://www.mpir.org)

    Targets MPIR 2.7.2 or above.

    Based on gmp_lib by wqyfavor
}
unit dwsMPIR;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses Windows, SysUtils, dwsXPlatform;

const
   MaxVarSize = MaxInt div 4;

type
   mp_limb_t = Cardinal;
   mp_limb_signed_t = Integer;
   mp_bitcnt_t = Cardinal;

   mp_ptr = ^mp_limb_t;

   mp_size_t_p = ^mp_size_t;
   mp_size_t = Integer;
   mp_exp_t = Integer;

   // Prototype of arbitrary precision integer number
   pmpz_t = ^mpz_t;
   mpz_t = record
      mp_alloc: Integer;
      mp_size: Integer;
      mp_d: mp_ptr;
   end;

   mpz_array_ptr = ^mpz_array;
   mpz_array = array[0..MaxVarSize div SizeOf(mpz_t) - 1] of mpz_t;

   // Prototype of arbitrary precision rational number
   pmpq_t = ^mpq_t;
   mpq_t = record
      mp_num: mpz_t;
      mp_den: mpz_t;
   end;

   // Prototype of arbitrary precision float number
   pmpf_t = ^mpf_t;
   mpf_t = record
      mp_prec: Integer;
      mp_size: Integer;
      mp_exp: mp_exp_t;
      mp_d: mp_ptr;
   end;

   // Available random number generation algorithms.
   gmp_randalg_t = (GMPRandAlgLC {Linear congruential}, GMPRandAlgMT{Mersenne Twister});

   // Linear congruential data struct.
   gmp_randata_lc = record
      a: mpz_t; { Multiplier. }
      c: Cardinal; { Adder. }
      m: mpz_t; { Modulus (valid only if M2Exp = 0). }
      M2Exp: Cardinal; { If <> 0, modulus is 2 ^ M2Exp. }
   end;

   gmp_randstate_t = record
      Seed: mpz_t; { Current seed. }
      Alg: gmp_randalg_t; { Algorithm used. }
      AlgData: record { Algorithm specific data. }
         case gmp_randalg_t of
            GMPRandAlgLC: (lc: ^gmp_randata_lc) { Linear congruential. }
      end
   end;

var
   { Integer (i.e. Z) routines }
   mpz_init : procedure (var dest: mpz_t); cdecl;
   mpz_inits : procedure (p: pmpz_t {; ...}); cdecl varargs;
   mpz_init2 : procedure (var dest: mpz_t; N: mp_bitcnt_t); cdecl varargs;
   mpz_clear : procedure (var dest: mpz_t); cdecl;
   mpz_clears : procedure (p: pmpz_t {; ...}); cdecl varargs;
   mpz_realloc : function (var dest: mpz_t; Limbs: mp_size_t): Pointer; cdecl;
   mpz_realloc2 : procedure (var dest: mpz_t; Bits: mp_size_t); cdecl;
   mpz_array_init : procedure (dest: mpz_array_ptr; ArraySize, FixedNumBits: mp_size_t); cdecl;

   mpz_swap : procedure (var v1, v2: mpz_t); cdecl;
   mpz_set : procedure (var dest: mpz_t; const src: mpz_t); cdecl;
   mpz_set_ui : procedure (var dest: mpz_t; src: Cardinal); cdecl;
   mpz_set_si : procedure (var dest: mpz_t; src: Integer); cdecl;
   mpz_set_d : procedure (var dest: mpz_t; src: Double); cdecl;
   mpz_set_q : procedure (var dest: mpz_t; src: mpq_t); cdecl;
   mpz_set_f : procedure (var dest: mpz_t; src: mpf_t); cdecl;
   mpz_set_str : function (var dest: mpz_t; src: PAnsiChar; Base: Integer): Integer; cdecl;

   mpz_init_set : procedure (var dest: mpz_t; const src: mpz_t); cdecl;
   mpz_init_set_ui : procedure (var dest: mpz_t; src: Cardinal); cdecl;
   mpz_init_set_si : procedure (var dest: mpz_t; src: Integer); cdecl;
   mpz_init_set_d : procedure (var dest: mpz_t; src: Double); cdecl;
   mpz_init_set_str : function (var dest: mpz_t; src: PAnsiChar; Base: Integer): Integer; cdecl;

   mpz_import : procedure (var dest: mpz_t; Count: mp_size_t; Order: Integer; Size: mp_size_t; Endian: Integer; Nails: mp_size_t; op: Pointer); cdecl;
   mpz_export : function (Rop: Pointer; PCount: mp_size_t_p; Order: Integer; Size: mp_size_t; Endian: Integer; Nails: mp_size_t; const src: mpz_t): Pointer; cdecl;

   mpz_getlimbn : function (const src: mpz_t; n: mp_size_t): mp_limb_t; cdecl;
   mpz_size : function (const src: mpz_t): mp_size_t; cdecl;

   mpz_get_ui : function (const src: mpz_t): Cardinal; cdecl;
   mpz_get_si : function (const src: mpz_t): Integer; cdecl;
   mpz_get_d : function (const src: mpz_t): Double; cdecl;
   mpz_get_d_2exp : function (var Exp: Integer; src: mpz_t): Double; cdecl;
   mpz_fits_sint_p : function (const src: mpz_t): Integer; cdecl;
   mpz_fits_slong_p : function (const src: mpz_t): Integer; cdecl;
   mpz_fits_sshort_p : function (const src: mpz_t): Integer; cdecl;
   mpz_fits_uint_p : function (const src: mpz_t): Integer; cdecl;
   mpz_fits_ulong_p : function (const src: mpz_t): Integer; cdecl;
   mpz_fits_ushort_p : function (const src: mpz_t): Integer; cdecl;
   { Pass nil for dest to let the function allocate memory for it }
   mpz_get_str : function (dest: PAnsiChar; Base: Integer; const src: mpz_t): PAnsiChar; cdecl;

   mpz_add : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_add_ui : procedure (var dest: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;
   mpz_sub : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_sub_ui : procedure (var dest: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;
   mpz_ui_sub : procedure (var dest: mpz_t; src1: Cardinal; const src2: mpz_t); cdecl;
   mpz_mul : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_mul_si : procedure (var dest: mpz_t; const src1: mpz_t; src2: Integer); cdecl;
   mpz_mul_ui : procedure (var dest: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;
   mpz_mul_2exp : procedure (var dest: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;
   mpz_addmul : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_addmul_ui : procedure (var dest: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;
   mpz_submul : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_submul_ui : procedure (var dest: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;
   mpz_neg : procedure (var dest: mpz_t; const src: mpz_t); cdecl;
   mpz_abs : procedure (var dest: mpz_t; const src: mpz_t); cdecl;

   mpz_cdiv_q : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_cdiv_r : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_cdiv_qr : procedure (var destQ, destR: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_cdiv_q_ui : function (var dest: mpz_t; const src1: mpz_t; src2: Cardinal): Cardinal; cdecl;
   mpz_cdiv_r_ui : function (var dest: mpz_t; const src1: mpz_t; src2: Cardinal): Cardinal; cdecl;
   mpz_cdiv_qr_ui : function (var destQ, destR: mpz_t; const src1: mpz_t; src2: Cardinal): Cardinal; cdecl;
   mpz_cdiv_ui : function (const src1: mpz_t; src2: Cardinal): Cardinal; cdecl;
   mpz_cdiv_q_2exp : procedure (var dest: mpz_t; const src1: mpz_t; src2: mp_bitcnt_t); cdecl;
   mpz_cdiv_r_2exp : procedure (var dest: mpz_t; const src1: mpz_t; src2: mp_bitcnt_t); cdecl;

   mpz_fdiv_q : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_fdiv_r : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_fdiv_qr : procedure (var destQ, destR: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_fdiv_q_ui : function (var dest: mpz_t; const src1: mpz_t; src2: Cardinal): Cardinal; cdecl;
   mpz_fdiv_r_ui : function (var dest: mpz_t; const src1: mpz_t; src2: Cardinal): Cardinal; cdecl;
   mpz_fdiv_qr_ui : function (var destQ, destR: mpz_t; const src1: mpz_t; src2: Cardinal): Cardinal; cdecl;
   mpz_fdiv_ui : function (const src1: mpz_t; src2: Cardinal): Cardinal; cdecl;
   mpz_fdiv_q_2exp : procedure (var dest: mpz_t; const src1: mpz_t; src2: mp_bitcnt_t); cdecl;
   mpz_fdiv_r_2exp : procedure (var dest: mpz_t; const src1: mpz_t; src2: mp_bitcnt_t); cdecl;

   mpz_tdiv_q : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_tdiv_r : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_tdiv_qr : procedure (var destQ, destR: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_tdiv_q_ui : procedure (var dest: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;
   mpz_tdiv_r_ui : procedure (var dest: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;
   mpz_tdiv_qr_ui : procedure (var destQ, destR: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;
   mpz_tdiv_ui : function (const src1: mpz_t; src2: Cardinal): Cardinal; cdecl;
   mpz_tdiv_q_2exp : procedure (var dest: mpz_t; const src1: mpz_t; src2: mp_bitcnt_t); cdecl;
   mpz_tdiv_r_2exp : procedure (var dest: mpz_t; const src1: mpz_t; src2: mp_bitcnt_t); cdecl;

   mpz_mod : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_mod_ui : procedure (var dest: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;
   mpz_divexact : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_divexact_ui : procedure (var dest: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;

   mpz_mod_2exp : procedure (var dest: mpz_t; const src1: mpz_t; src2: mp_bitcnt_t); cdecl;
   mpz_div_2exp : procedure (var dest: mpz_t; const src1: mpz_t; src2: mp_bitcnt_t); cdecl;

   mpz_divisible_p : function (var n, d: mpz_t): Integer; cdecl;
   mpz_divisible_ui_p : function (var n: mpz_t; d: Cardinal): Integer; cdecl;
   mpz_divisible_2exp_p : function (var n: mpz_t; d: mp_bitcnt_t): Integer; cdecl;
   mpz_congruent_p : function (var n, c, d: mpz_t): Integer; cdecl;
   mpz_congruent_ui_p : function (var n: mpz_t; c, d: Cardinal): Integer; cdecl;
   mpz_congruent_2exp_p : function (var n, c: mpz_t; b: mp_bitcnt_t): Integer; cdecl;

   mpz_powm : procedure (var dest: mpz_t; var Base, Exponent, Modulus: mpz_t); cdecl;
   mpz_powm_ui : procedure (var dest: mpz_t; var Base: mpz_t; Exponent: Cardinal; var Modulus: mpz_t); cdecl;
   mpz_pow_ui : procedure (var dest: mpz_t; var Base: mpz_t; Exponent: Cardinal); cdecl;
   mpz_ui_pow_ui : procedure (var dest: mpz_t; Base, Exponent: Cardinal); cdecl;

   mpz_root : function (var dest: mpz_t; const src: mpz_t; n: Cardinal): Integer; cdecl;
   mpz_nthroot : procedure (var dest: mpz_t; const src: mpz_t; n: Cardinal); cdecl;
   mpz_rootrem : procedure (var Root: mpz_t; var Rem: mpz_t; const src: mpz_t; n: Cardinal); cdecl;
   mpz_sqrt : procedure (var dest: mpz_t; const src: mpz_t); cdecl;
   mpz_sqrtrem : procedure (var dest, destR: mpz_t; const src: mpz_t); cdecl;
   mpz_perfect_square_p : function (const src: mpz_t): Integer; cdecl;
   mpz_perfect_power_p : function (const src: mpz_t): Integer; cdecl;

   mpz_sizeinbase : function (const src: mpz_t; Base: Integer): Integer; cdecl;

   mpz_probable_prime_p : function (const src: mpz_t; var state: gmp_randstate_t; Prob: Integer; DivTested: Cardinal): Integer; cdecl;
   mpz_likely_prime_p : function (const src: mpz_t; var state: gmp_randstate_t; DivTested: Cardinal): Integer; cdecl;
   mpz_next_prime_candidate : procedure (var dest: mpz_t; const src: mpz_t; var state: gmp_randstate_t); cdecl;

   // Number theoretic functions
   mpz_gcd : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_gcd_ui : function (var dest: mpz_t; const src1: mpz_t; src2: Cardinal): Cardinal; cdecl;
   mpz_gcdext : procedure (var dest, destA, destB: mpz_t; const srcA, srcB: mpz_t); cdecl;
   mpz_lcm : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_lcm_ui : function (var dest: mpz_t; const src1: mpz_t; src2: Cardinal): Cardinal; cdecl;
   mpz_invert : function (var dest: mpz_t; const src, Modulus: mpz_t): Integer; cdecl;
   mpz_jacobi : function (const src1, src2: mpz_t): Integer; cdecl;
   mpz_legendre : function (const src1, src2: mpz_t): Integer; cdecl;
   mpz_kronecker : function (const src1, src2: mpz_t): Integer; cdecl;
   mpz_kronecker_si : function (const src1: mpz_t; src2: Integer): Integer; cdecl;
   mpz_kronecker_ui : function (const src1: mpz_t; src2: Cardinal): Integer; cdecl;
   mpz_si_kronecker : function (src1: Integer; const src2: mpz_t): Integer; cdecl;
   mpz_ui_kronecker : function (src1: Cardinal; const src2: mpz_t): Integer; cdecl;
   mpz_remove : function (var dest: mpz_t; const src1, src2: mpz_t): Cardinal; cdecl;
   mpz_fac_ui : procedure (var dest: mpz_t; src: Cardinal); cdecl;
   mpz_primorial_ui : procedure (var dest: mpz_t; n: Cardinal); cdecl;
   mpz_fib_ui : procedure (var dest: mpz_t; src: Cardinal); cdecl;
   mpz_fib2_ui : procedure (var dest: mpz_t; var destSub: mpz_t; src: Cardinal); cdecl;
   mpz_bin_ui : procedure (var dest: mpz_t; const src1: mpz_t; src2: Cardinal); cdecl;
   mpz_bin_uiui : procedure (var dest: mpz_t; src1, src2: Cardinal); cdecl;
   mpz_lucnum_ui : procedure (var dest: mpz_t; src: Cardinal); cdecl;
   mpz_lucnum2_ui : procedure (var dest: mpz_t; var destSub: mpz_t; src: Cardinal); cdecl;

   mpz_cmp : function (const src1, src2: mpz_t): Integer; cdecl;
   mpz_cmp_d : function (const src1: mpz_t; src2: Double): Integer; cdecl;
   mpz_cmp_ui : function (const src1: mpz_t; src2: Cardinal): Integer; cdecl;
   mpz_cmp_si : function (const src1: mpz_t; src2: Integer): Integer; cdecl;
   mpz_cmpabs : function (const src1, src2: mpz_t): Integer; cdecl;
   mpz_cmpabs_d : function (const src1: mpz_t; src2: Double): Integer; cdecl;
   mpz_cmpabs_ui : function (const src1: mpz_t; src2: Cardinal): Integer; cdecl;

   mpz_and : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_ior : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_xor : procedure (var dest: mpz_t; const src1, src2: mpz_t); cdecl;
   mpz_com : procedure (var dest: mpz_t; const src: mpz_t); cdecl;
   mpz_popcount : function (const src: mpz_t): mp_bitcnt_t; cdecl;
   mpz_hamdist : function (const src1, src2: mpz_t): mp_bitcnt_t; cdecl;
   mpz_scan0 : function (const src: mpz_t; StartingBit: mp_bitcnt_t): mp_bitcnt_t; cdecl;
   mpz_scan1 : function (const src: mpz_t; StartingBit: mp_bitcnt_t): mp_bitcnt_t; cdecl;
   mpz_setbit : procedure (var dest: mpz_t; BitIndex: mp_bitcnt_t); cdecl;
   mpz_clrbit : procedure (var dest: mpz_t; BitIndex: mp_bitcnt_t); cdecl;
   mpz_combit : procedure (var dest: mpz_t; BitIndex: mp_bitcnt_t); cdecl;
   mpz_tstbit : function (var dest: mpz_t; BitIndex: mp_bitcnt_t): Integer; cdecl;

   mpz_urandomb : procedure (var ROP: mpz_t; var state: gmp_randstate_t; n: Cardinal); cdecl;
   mpz_urandomm : procedure (var ROP: mpz_t; var state: gmp_randstate_t; var n: mpz_t); cdecl;
   mpz_rrandomb : procedure (var ROP: mpz_t; var state: gmp_randstate_t; n: Cardinal); cdecl;

procedure mpz_set_uint64(var dest: mpz_t; const src: UInt64);
procedure mpz_set_int64(var dest: mpz_t; const src: Int64);

function mpz_odd_p(const src: mpz_t): Boolean; inline;
function mpz_even_p(const src: mpz_t): Boolean; inline;
function mpz_sgn(const src: mpz_t): Integer;

var
   { Rational (i.e. Q) routines }
   mpq_canonicalize : procedure (var dest: mpq_t); cdecl;

   mpq_init : procedure (var dest: mpq_t); cdecl;
   mpq_inits : procedure (p: pmpq_t {; ...}); cdecl varargs;
   mpq_clear : procedure (var dest: mpq_t); cdecl;
   mpq_clears : procedure (p: pmpq_t {; ...}); cdecl varargs;
   mpq_set : procedure (var dest: mpq_t; const src: mpq_t); cdecl;
   mpq_set_z : procedure (var dest: mpq_t; const src: mpz_t); cdecl;
   mpq_set_ui : procedure (var dest: mpq_t; Nom, Den: Cardinal); cdecl;
   mpq_set_si : procedure (var dest: mpq_t; Nom: Integer; Den: Cardinal); cdecl;
   mpq_set_str : function (var dest: mpq_t; src: PAnsiChar; Base: Integer): Integer; cdecl;
   mpq_set_d : procedure (var dest: mpq_t; src: Double); cdecl;
   mpq_set_f : procedure (var dest: mpq_t; const src: mpf_t); cdecl;
   mpq_swap : procedure (var v1, v2: mpq_t); cdecl;

   mpq_add : procedure (var dest: mpq_t; const src1, src2: mpq_t); cdecl;
   mpq_sub : procedure (var dest: mpq_t; const src1, src2: mpq_t); cdecl;
   mpq_mul : procedure (var dest: mpq_t; const src1, src2: mpq_t); cdecl;
   mpq_div : procedure (var dest: mpq_t; const src1, src2: mpq_t); cdecl;
   mpq_neg : procedure (var dest: mpq_t; const src: mpq_t); cdecl;
   mpq_abs : procedure (var dest: mpq_t; const src: mpq_t); cdecl;
   mpq_inv : procedure (var dest: mpq_t; const src: mpq_t); cdecl;
   mpq_mul_2exp : procedure (var dest: mpq_t; const src1: mpq_t; src2: mp_bitcnt_t); cdecl;
   mpq_div_2exp : procedure (var dest: mpq_t; const src1: mpq_t; src2: mp_bitcnt_t); cdecl;

   mpq_cmp : function (const src1, src2: mpq_t): Integer; cdecl;
   mpq_cmp_ui : function (const src1: mpq_t; Nom2, Den2: Cardinal): Integer; cdecl;
   mpq_cmp_si : function (const src1: mpq_t; Nom2: Integer; Den2: Cardinal): Integer; cdecl;
   mpq_equal : function (const src1, src2: mpq_t): Integer; cdecl;

   mpq_get_d : function (const src: mpq_t): Double; cdecl;
   mpq_set_num : procedure (var dest: mpq_t; const src: mpz_t); cdecl;
   mpq_set_den : procedure (var dest: mpq_t; const src: mpz_t); cdecl;
   mpq_get_num : procedure (var dest: mpz_t; const src: mpq_t); cdecl;
   mpq_get_den : procedure (var dest: mpz_t; const src: mpq_t); cdecl;

   mpq_get_str : function (dest: PAnsiChar; Base: Integer; const src: mpq_t): PAnsiChar; cdecl;

function mpq_sgn(const src: mpq_t): Integer; // [MACRO]
function mpq_numref(const src: mpq_t): pmpz_t; inline; // [MACRO]
function mpq_denref(const src: mpq_t): pmpz_t; inline; // [MACRO]

var
   { Floating point (i.e. R) routines }
   mpf_set_default_prec : procedure (Precision: mp_bitcnt_t); cdecl;
   mpf_get_default_prec: function : mp_bitcnt_t; cdecl;
   mpf_init : procedure (var dest: mpf_t); cdecl;
   mpf_init2 : procedure (var dest: mpf_t; Precision: mp_bitcnt_t); cdecl;
   mpf_inits : procedure (p: pmpf_t {; ...}); cdecl varargs;
   mpf_clear : procedure (var dest: mpf_t); cdecl;
   mpf_clears : procedure (p: pmpf_t {; ...}); cdecl varargs;
   mpf_set_prec : procedure (var dest: mpf_t; Precision: mp_bitcnt_t); cdecl;
   mpf_get_prec : function (const src: mpf_t): mp_bitcnt_t; cdecl;
   mpf_set_prec_raw : procedure (var dest: mpf_t; Precision: mp_bitcnt_t); cdecl;

   mpf_set : procedure (var dest: mpf_t; const src: mpf_t); cdecl;
   mpf_set_ui : procedure (var dest: mpf_t; src: Cardinal); cdecl;
   mpf_set_si : procedure (var dest: mpf_t; src: Integer); cdecl;
   mpf_set_d : procedure (var dest: mpf_t; src: Double); cdecl;
   mpf_set_z : procedure (var dest: mpf_t; const src: mpz_t); cdecl;
   mpf_set_q : procedure (var dest: mpf_t; const src: mpq_t); cdecl;
   mpf_set_str : function (var dest: mpf_t; src: PAnsiChar; Base: Integer): Integer; cdecl;
   mpf_swap : procedure (var v1, v2: mpf_t); cdecl;

   mpf_init_set : procedure (var dest: mpf_t; const src: mpf_t); cdecl;
   mpf_init_set_ui : procedure (var dest: mpf_t; src: Cardinal); cdecl;
   mpf_init_set_si : procedure (var dest: mpf_t; src: Integer); cdecl;
   mpf_init_set_d : procedure (var dest: mpf_t; src: Double); cdecl;
   mpf_init_set_str : function (var dest: mpf_t; src: PAnsiChar; Base: Integer): Integer; cdecl;

   mpf_get_d : function (const src: mpf_t): Double; cdecl;
   mpf_get_si : function (const src: mpf_t): Integer; cdecl;
   mpf_get_ui : function (const src: mpf_t): Cardinal; cdecl;
   mpf_get_d_2exp : function (var Exp: Integer; const src: mpf_t): Double; cdecl;
   mpf_fits_sint_p : function (const src: mpf_t): Integer; cdecl;
   mpf_fits_slong_p : function (const src: mpf_t): Integer; cdecl;
   mpf_fits_sshort_p : function (const src: mpf_t): Integer; cdecl;
   mpf_fits_uint_p : function (const src: mpf_t): Integer; cdecl;
   mpf_fits_ulong_p : function (const src: mpf_t): Integer; cdecl;
   mpf_fits_ushort_p : function (const src: mpf_t): Integer; cdecl;

   mpf_cmp : function (const src1, src2: mpf_t): Integer; cdecl;
   mpf_cmp_si : function (const src1: mpf_t; src2: Integer): Integer; cdecl;
   mpf_cmp_ui : function (const src1: mpf_t; src2: Cardinal): Integer; cdecl;
   mpf_cmp_d : function (const src1: mpf_t; src2: Double): Integer; cdecl;
   mpf_eq : function (const src1, src2: mpf_t; NumberOfBits: mp_bitcnt_t): Integer; cdecl;
   mpf_reldiff : procedure (var dest: mpf_t; const src1, src2: mpf_t); cdecl;

   mpf_get_str : function (dest: PAnsiChar; var Exponent: mp_exp_t; Base: Integer;
      NumberOfDigits: mp_size_t; const src: mpf_t): PAnsiChar; cdecl;

   mpf_add : procedure (var dest: mpf_t; const src1, src2: mpf_t); cdecl;
   mpf_add_ui : procedure (var dest: mpf_t; const src1: mpf_t; src2: Cardinal); cdecl;
   mpf_sub : procedure (var dest: mpf_t; const src1, src2: mpf_t); cdecl;
   mpf_ui_sub : procedure (var dest: mpf_t; src1: Cardinal; const src2: mpf_t); cdecl;
   mpf_sub_ui : procedure (var dest: mpf_t; const src1: mpf_t; src2: Cardinal); cdecl;
   mpf_mul : procedure (var dest: mpf_t; const src1, src2: mpf_t); cdecl;
   mpf_mul_ui : procedure (var dest: mpf_t; const src1: mpf_t; src2: Cardinal); cdecl;
   mpf_div : procedure (var dest: mpf_t; const src1, src2: mpf_t); cdecl;
   mpf_ui_div : procedure (var dest: mpf_t; src1: Cardinal; const src2: mpf_t); cdecl;
   mpf_div_ui : procedure (var dest: mpf_t; const src1: mpf_t; src2: Cardinal); cdecl;
   mpf_sqrt : procedure (var dest: mpf_t; const src: mpf_t); cdecl;
   mpf_sqrt_ui : procedure (var dest: mpf_t; src: Cardinal); cdecl;
   mpf_pow_ui : procedure (var dest: mpf_t; const src1: mpf_t; src2: Cardinal); cdecl;
   mpf_neg : procedure (var dest: mpf_t; const src: mpf_t); cdecl;
   mpf_abs : procedure (var dest: mpf_t; const src: mpf_t); cdecl;
   mpf_mul_2exp : procedure (var dest: mpf_t; const src1: mpf_t; src2: mp_bitcnt_t); cdecl;
   mpf_div_2exp : procedure (var dest: mpf_t; const src1: mpf_t; src2: mp_bitcnt_t); cdecl;

   mpf_ceil : procedure (var dest: mpf_t; const src: mpf_t); cdecl;
   mpf_floor : procedure (var dest: mpf_t; const src: mpf_t); cdecl;
   mpf_trunc : procedure (var dest: mpf_t; const src: mpf_t); cdecl;
   mpf_integer_p : function (const src: mpf_t): Integer; cdecl;

   mpf_urandomb : procedure (var ROP: mpf_t; var state: gmp_randstate_t; nBits: mp_bitcnt_t); cdecl;
   mpf_rrandomb : procedure (var ROP: mpf_t; var state: gmp_randstate_t; maxSize: mp_size_t; exp: mp_exp_t); cdecl;

function mpf_sgn(const src: mpf_t): Integer;

var
   gmp_randinit_default : procedure (var state: gmp_randstate_t); cdecl;
   gmp_randinit_mt : procedure (var state: gmp_randstate_t); cdecl;
   gmp_randinit_lc_2exp : procedure (var state: gmp_randstate_t; var a: mpz_t; c: Cardinal; M2Exp: mp_bitcnt_t); cdecl;
   gmp_randinit_lc_2exp_size : procedure (var state: gmp_randstate_t; size: mp_bitcnt_t); cdecl;
   gmp_randinit_set : procedure (var dest: gmp_randstate_t; const src: gmp_randstate_t); cdecl;
   gmp_randclear : procedure (var state: gmp_randstate_t); cdecl;
   gmp_randseed : procedure (var state: gmp_randstate_t; Seed: mpz_t); cdecl;
   gmp_randseed_ui : procedure (var state: gmp_randstate_t; Seed: Cardinal); cdecl;
   gmp_urandomb_ui : function (var state: gmp_randstate_t; n: Cardinal): Cardinal; cdecl;
   gmp_urandomm_ui : function (var state: gmp_randstate_t; n: Cardinal): Cardinal; cdecl;

{ Formatted I/O functions }
// for "..." arguments, pointer to mpx_t and PAnsiChar should be used.
// e.g:
//    var i: mpz_t;
//        buf: AnsiString;
//    begin
//       mpz_init_set_ui(i, 12345);
//       SetLength(buf, 100); // allocate memory
//       gmp_printf(PAnsiChar(buf), '%s is an mpz %Zd', PAnsiChar('hear'), @i);
//       mpz_clear(i);
//    end;

// for rational numbers,
   gmp_printf : procedure (Buf: PAnsiChar; Fmt: PAnsiChar{; ...}); cdecl varargs;
   gmp_scanf : procedure (Buf: PAnsiChar; Fmt: PAnsiChar{; ...}); cdecl varargs;

{ Extensions to the GMP library, implemented in this unit }

procedure mpf_exp(var dest: mpf_t; const src: mpf_t);

(*

Disabled for now, as these are not thread-safe

procedure mpf_ln(var dest: mpf_t; const src: mpf_t);
procedure mpf_pow(var dest: mpf_t; const src1, src2: mpf_t);
procedure mpf_sin(var dest: mpf_t; const src: mpf_t);
procedure mpf_cos(var dest: mpf_t; const src: mpf_t);
procedure mpf_arctan(var dest: mpf_t; const src: mpf_t);
procedure mpf_pi(var dest: mpf_t);

*)

var
   vOnNeedMPIRDynamicDLLName : function : String;

function Bind_MPIR_DLL(const dllName : String = '') : Boolean;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vDLLHandle : THandle;
   vBindMRSW : TMultiReadSingleWrite;

function PerformBind_MPIR_DLL(dllName : String) : Boolean;
var
   handle : THandle;

   function GetProcMPZ(const name : AnsiString) : Pointer;
   begin
      Result := GetProcAddress(handle, PAnsiChar('__gmpz_'+name));
   end;

   function GetProcMPQ(const name : AnsiString) : Pointer;
   begin
      Result := GetProcAddress(handle, PAnsiChar('__gmpq_'+name));
   end;

   function GetProcMPF(const name : AnsiString) : Pointer;
   begin
      Result := GetProcAddress(handle, PAnsiChar('__gmpf_'+name));
   end;

   function GetProcGMP(const name : AnsiString) : Pointer;
   begin
      Result := GetProcAddress(handle, PAnsiChar('__gmp_'+name));
   end;

begin
   vBindMRSW.BeginWrite;
   try
      if dllName = '' then begin
         if Assigned(vOnNeedMPIRDynamicDLLName) then
            dllName := vOnNeedMPIRDynamicDLLName();
         if dllName = '' then
            dllName := 'mpir.dll';
      end;

      handle := LoadLibrary(PChar(dllName));
      if handle = 0 then begin
          raise EOSError.CreateFmt('Failed to load "%s", Error %d: %s',
                                   [dllName, GetLastError, SysErrorMessage(GetLastError)]);
      end;

      mpz_init := GetProcMPZ('init');
      mpz_inits := GetProcMPZ('inits');
      mpz_init2 := GetProcMPZ('init2');
      mpz_clear := GetProcMPZ('clear');
      mpz_clears := GetProcMPZ('clears');
      mpz_realloc := GetProcMPZ('realloc');
      mpz_realloc2 := GetProcMPZ('realloc2');
      mpz_array_init := GetProcMPZ('array_init');
      mpz_swap := GetProcMPZ('swap');
      mpz_set := GetProcMPZ('set');
      mpz_set_ui := GetProcMPZ('set_ui');
      mpz_set_si := GetProcMPZ('set_si');
      mpz_set_d := GetProcMPZ('set_d');
      mpz_set_q := GetProcMPZ('set_q');
      mpz_set_f := GetProcMPZ('set_f');
      mpz_set_str := GetProcMPZ('set_str');
      mpz_init_set := GetProcMPZ('init_set');
      mpz_init_set_ui := GetProcMPZ('init_set_ui');
      mpz_init_set_si := GetProcMPZ('init_set_si');
      mpz_init_set_d := GetProcMPZ('init_set_d');
      mpz_init_set_str := GetProcMPZ('init_set_str');
      mpz_import := GetProcMPZ('import');
      mpz_export := GetProcMPZ('export');
      mpz_getlimbn := GetProcMPZ('getlimbn');
      mpz_size := GetProcMPZ('size');
      mpz_get_ui := GetProcMPZ('get_ui');
      mpz_get_si := GetProcMPZ('get_si');
      mpz_get_d := GetProcMPZ('get_d');
      mpz_get_d_2exp := GetProcMPZ('get_d_2exp');
      mpz_fits_sint_p := GetProcMPZ('fits_sint_p');
      mpz_fits_slong_p := GetProcMPZ('fits_slong_p');
      mpz_fits_sshort_p := GetProcMPZ('fits_sshort_p');
      mpz_fits_uint_p := GetProcMPZ('fits_uint_p');
      mpz_fits_ulong_p := GetProcMPZ('fits_ulong_p');
      mpz_fits_ushort_p := GetProcMPZ('fits_ushort_p');
      mpz_get_str := GetProcMPZ('get_str');
      mpz_add := GetProcMPZ('add');
      mpz_add_ui := GetProcMPZ('add_ui');
      mpz_sub := GetProcMPZ('sub');
      mpz_sub_ui := GetProcMPZ('sub_ui');
      mpz_ui_sub := GetProcMPZ('ui_sub');
      mpz_mul := GetProcMPZ('mul');
      mpz_mul_si := GetProcMPZ('mul_si');
      mpz_mul_ui := GetProcMPZ('mul_ui');
      mpz_mul_2exp := GetProcMPZ('mul_2exp');
      mpz_addmul := GetProcMPZ('addmul');
      mpz_addmul_ui := GetProcMPZ('addmul_ui');
      mpz_submul := GetProcMPZ('submul');
      mpz_submul_ui := GetProcMPZ('submul_ui');
      mpz_neg := GetProcMPZ('neg');
      mpz_abs := GetProcMPZ('abs');
      mpz_cdiv_q := GetProcMPZ('cdiv_q');
      mpz_cdiv_r := GetProcMPZ('cdiv_r');
      mpz_cdiv_qr := GetProcMPZ('cdiv_qr');
      mpz_cdiv_q_ui := GetProcMPZ('cdiv_q_ui');
      mpz_cdiv_r_ui := GetProcMPZ('cdiv_r_ui');
      mpz_cdiv_qr_ui := GetProcMPZ('cdiv_qr_ui');
      mpz_cdiv_ui := GetProcMPZ('cdiv_ui');
      mpz_cdiv_q_2exp := GetProcMPZ('cdiv_q_2exp');
      mpz_cdiv_r_2exp := GetProcMPZ('cdiv_r_2exp');
      mpz_fdiv_q := GetProcMPZ('fdiv_q');
      mpz_fdiv_r := GetProcMPZ('fdiv_r');
      mpz_fdiv_qr := GetProcMPZ('fdiv_qr');
      mpz_fdiv_q_ui := GetProcMPZ('fdiv_q_ui');
      mpz_fdiv_r_ui := GetProcMPZ('fdiv_r_ui');
      mpz_fdiv_qr_ui := GetProcMPZ('fdiv_qr_ui');
      mpz_fdiv_ui := GetProcMPZ('fdiv_ui');
      mpz_fdiv_q_2exp := GetProcMPZ('fdiv_q_2exp');
      mpz_fdiv_r_2exp := GetProcMPZ('fdiv_r_2exp');
      mpz_tdiv_q := GetProcMPZ('tdiv_q');
      mpz_tdiv_r := GetProcMPZ('tdiv_r');
      mpz_tdiv_qr := GetProcMPZ('tdiv_qr');
      mpz_tdiv_q_ui := GetProcMPZ('tdiv_q_ui');
      mpz_tdiv_r_ui := GetProcMPZ('tdiv_r_ui');
      mpz_tdiv_qr_ui := GetProcMPZ('tdiv_qr_ui');
      mpz_tdiv_ui := GetProcMPZ('tdiv_ui');
      mpz_tdiv_q_2exp := GetProcMPZ('tdiv_q_2exp');
      mpz_tdiv_r_2exp := GetProcMPZ('tdiv_r_2exp');
      mpz_mod := GetProcMPZ('mod');
      mpz_mod_ui := GetProcMPZ('mod_ui');
      mpz_divexact := GetProcMPZ('divexact');
      mpz_divexact_ui := GetProcMPZ('divexact_ui');
      mpz_mod_2exp := GetProcMPZ('mod_2exp');
      mpz_div_2exp := GetProcMPZ('div_2exp');
      mpz_divisible_p := GetProcMPZ('divisible_p');
      mpz_divisible_ui_p := GetProcMPZ('divisible_ui_p');
      mpz_divisible_2exp_p := GetProcMPZ('divisible_2exp_p');
      mpz_congruent_p := GetProcMPZ('congruent_p');
      mpz_congruent_ui_p := GetProcMPZ('congruent_ui_p');
      mpz_congruent_2exp_p := GetProcMPZ('congruent_2exp_p');
      mpz_powm := GetProcMPZ('powm');
      mpz_powm_ui := GetProcMPZ('powm_ui');
      mpz_pow_ui := GetProcMPZ('pow_ui');
      mpz_ui_pow_ui := GetProcMPZ('ui_pow_ui');
      mpz_root := GetProcMPZ('root');
      mpz_nthroot := GetProcMPZ('nthroot');
      mpz_rootrem := GetProcMPZ('rootrem');
      mpz_sqrt := GetProcMPZ('sqrt');
      mpz_sqrtrem := GetProcMPZ('sqrtrem');
      mpz_perfect_square_p := GetProcMPZ('perfect_square_p');
      mpz_perfect_power_p := GetProcMPZ('perfect_power_p');
      mpz_sizeinbase := GetProcMPZ('sizeinbase');
      mpz_probable_prime_p := GetProcMPZ('probable_prime_p');
      mpz_likely_prime_p := GetProcMPZ('likely_prime_p');
      mpz_next_prime_candidate := GetProcMPZ('next_prime_candidate');
      mpz_gcd := GetProcMPZ('gcd');
      mpz_gcd_ui := GetProcMPZ('gcd_ui');
      mpz_gcdext := GetProcMPZ('gcdext');
      mpz_lcm := GetProcMPZ('lcm');
      mpz_lcm_ui := GetProcMPZ('lcm_ui');
      mpz_invert := GetProcMPZ('invert');
      mpz_jacobi := GetProcMPZ('jacobi');
      mpz_legendre := GetProcMPZ('legendre');
      mpz_kronecker := GetProcMPZ('kronecker');
      mpz_kronecker_si := GetProcMPZ('kronecker_si');
      mpz_kronecker_ui := GetProcMPZ('kronecker_ui');
      mpz_si_kronecker := GetProcMPZ('si_kronecker');
      mpz_ui_kronecker := GetProcMPZ('ui_kronecker');
      mpz_remove := GetProcMPZ('remove');
      mpz_fac_ui := GetProcMPZ('fac_ui');
      mpz_primorial_ui := GetProcMPZ('primorial_ui');
      mpz_fib_ui := GetProcMPZ('fib_ui');
      mpz_fib2_ui := GetProcMPZ('fib2_ui');
      mpz_bin_ui := GetProcMPZ('bin_ui');
      mpz_bin_uiui := GetProcMPZ('bin_uiui');
      mpz_lucnum_ui := GetProcMPZ('lucnum_ui');
      mpz_lucnum2_ui := GetProcMPZ('lucnum2_ui');
      mpz_cmp := GetProcMPZ('cmp');
      mpz_cmp_d := GetProcMPZ('cmp_d');
      mpz_cmp_ui := GetProcMPZ('cmp_ui');
      mpz_cmp_si := GetProcMPZ('cmp_si');
      mpz_cmpabs := GetProcMPZ('cmpabs');
      mpz_cmpabs_d := GetProcMPZ('cmpabs_d');
      mpz_cmpabs_ui := GetProcMPZ('cmpabs_ui');
      mpz_and := GetProcMPZ('and');
      mpz_ior := GetProcMPZ('ior');
      mpz_xor := GetProcMPZ('xor');
      mpz_com := GetProcMPZ('com');
      mpz_popcount := GetProcMPZ('popcount');
      mpz_hamdist := GetProcMPZ('hamdist');
      mpz_scan0 := GetProcMPZ('scan0');
      mpz_scan1 := GetProcMPZ('scan1');
      mpz_setbit := GetProcMPZ('setbit');
      mpz_clrbit := GetProcMPZ('clrbit');
      mpz_combit := GetProcMPZ('combit');
      mpz_tstbit := GetProcMPZ('tstbit');
      mpz_urandomb := GetProcMPZ('urandomb');
      mpz_urandomm := GetProcMPZ('urandomm');
      mpz_rrandomb := GetProcMPZ('rrandomb');

      mpq_canonicalize := GetProcMPQ('canonicalize');
      mpq_init := GetProcMPQ('init');
      mpq_inits := GetProcMPQ('inits');
      mpq_clear := GetProcMPQ('clear');
      mpq_clears := GetProcMPQ('clears');
      mpq_set := GetProcMPQ('set');
      mpq_set_z := GetProcMPQ('set_z');
      mpq_set_ui := GetProcMPQ('set_ui');
      mpq_set_si := GetProcMPQ('set_si');
      mpq_set_str := GetProcMPQ('set_str');
      mpq_set_d := GetProcMPQ('set_d');
      mpq_set_f := GetProcMPQ('set_f');
      mpq_swap := GetProcMPQ('swap');
      mpq_add := GetProcMPQ('add');
      mpq_sub := GetProcMPQ('sub');
      mpq_mul := GetProcMPQ('mul');
      mpq_div := GetProcMPQ('div');
      mpq_neg := GetProcMPQ('neg');
      mpq_abs := GetProcMPQ('abs');
      mpq_inv := GetProcMPQ('inv');
      mpq_mul_2exp := GetProcMPQ('mul_2exp');
      mpq_div_2exp := GetProcMPQ('div_2exp');
      mpq_cmp := GetProcMPQ('cmp');
      mpq_cmp_ui := GetProcMPQ('cmp_ui');
      mpq_cmp_si := GetProcMPQ('cmp_si');
      mpq_equal := GetProcMPQ('equal');
      mpq_get_d := GetProcMPQ('get_d');
      mpq_set_num := GetProcMPQ('set_num');
      mpq_set_den := GetProcMPQ('set_den');
      mpq_get_num := GetProcMPQ('get_num');
      mpq_get_den := GetProcMPQ('get_den');
      mpq_get_str := GetProcMPQ('get_str');

      mpf_set_default_prec := GetProcMPF('set_default_prec');
      mpf_get_default_prec := GetProcMPF('get_default_prec');
      mpf_init := GetProcMPF('init');
      mpf_init2 := GetProcMPF('init2');
      mpf_inits := GetProcMPF('inits');
      mpf_clear := GetProcMPF('clear');
      mpf_clears := GetProcMPF('clears');
      mpf_set_prec := GetProcMPF('set_prec');
      mpf_get_prec := GetProcMPF('get_prec');
      mpf_set_prec_raw := GetProcMPF('set_prec_raw');
      mpf_set := GetProcMPF('set');
      mpf_set_ui := GetProcMPF('set_ui');
      mpf_set_si := GetProcMPF('set_si');
      mpf_set_d := GetProcMPF('set_d');
      mpf_set_z := GetProcMPF('set_z');
      mpf_set_q := GetProcMPF('set_q');
      mpf_set_str := GetProcMPF('set_str');
      mpf_swap := GetProcMPF('swap');
      mpf_init_set := GetProcMPF('init_set');
      mpf_init_set_ui := GetProcMPF('init_set_ui');
      mpf_init_set_si := GetProcMPF('init_set_si');
      mpf_init_set_d := GetProcMPF('init_set_d');
      mpf_init_set_str := GetProcMPF('init_set_str');
      mpf_get_d := GetProcMPF('get_d');
      mpf_get_si := GetProcMPF('get_si');
      mpf_get_ui := GetProcMPF('get_ui');
      mpf_get_d_2exp := GetProcMPF('get_d_2exp');
      mpf_fits_sint_p := GetProcMPF('fits_sint_p');
      mpf_fits_slong_p := GetProcMPF('fits_slong_p');
      mpf_fits_sshort_p := GetProcMPF('fits_sshort_p');
      mpf_fits_uint_p := GetProcMPF('fits_uint_p');
      mpf_fits_ulong_p := GetProcMPF('fits_ulong_p');
      mpf_fits_ushort_p := GetProcMPF('fits_ushort_p');
      mpf_cmp := GetProcMPF('cmp');
      mpf_cmp_si := GetProcMPF('cmp_si');
      mpf_cmp_ui := GetProcMPF('cmp_ui');
      mpf_cmp_d := GetProcMPF('cmp_d');
      mpf_eq := GetProcMPF('eq');
      mpf_reldiff := GetProcMPF('reldiff');
      mpf_get_str := GetProcMPF('get_str');
      mpf_add := GetProcMPF('add');
      mpf_add_ui := GetProcMPF('add_ui');
      mpf_sub := GetProcMPF('sub');
      mpf_ui_sub := GetProcMPF('ui_sub');
      mpf_sub_ui := GetProcMPF('sub_ui');
      mpf_mul := GetProcMPF('mul');
      mpf_mul_ui := GetProcMPF('mul_ui');
      mpf_div := GetProcMPF('div');
      mpf_ui_div := GetProcMPF('ui_div');
      mpf_div_ui := GetProcMPF('div_ui');
      mpf_sqrt := GetProcMPF('sqrt');
      mpf_sqrt_ui := GetProcMPF('sqrt_ui');
      mpf_pow_ui := GetProcMPF('pow_ui');
      mpf_neg := GetProcMPF('neg');
      mpf_abs := GetProcMPF('abs');
      mpf_mul_2exp := GetProcMPF('mul_2exp');
      mpf_div_2exp := GetProcMPF('div_2exp');
      mpf_ceil := GetProcMPF('ceil');
      mpf_floor := GetProcMPF('floor');
      mpf_trunc := GetProcMPF('trunc');
      mpf_integer_p := GetProcMPF('integer_p');
      mpf_urandomb := GetProcMPF('urandomb');
      mpf_rrandomb := GetProcMPF('rrandomb');

      gmp_randinit_default := GetProcGMP('randinit_default');
      gmp_randinit_mt := GetProcGMP('randinit_mt');
      gmp_randinit_lc_2exp := GetProcGMP('randinit_lc_2exp');
      gmp_randinit_lc_2exp_size := GetProcGMP('randinit_lc_2exp_size');
      gmp_randinit_set := GetProcGMP('randinit_set');
      gmp_randclear := GetProcGMP('randclear');
      gmp_randseed := GetProcGMP('randseed');
      gmp_randseed_ui := GetProcGMP('randseed_ui');
      gmp_urandomb_ui := GetProcGMP('urandomb_ui');
      gmp_urandomm_ui := GetProcGMP('urandomm_ui');
      gmp_printf := GetProcGMP('printf');
      gmp_scanf := GetProcGMP('scanf');

      vDLLHandle := handle;

      Result := True;
   finally
      vBindMRSW.EndWrite;
   end;
end;

function Bind_MPIR_DLL(const dllName : String = '') : Boolean;
begin
   if vDLLHandle <> 0 then Exit(True);
   Result := PerformBind_MPIR_DLL(dllName);
end;

function mpz_odd_p(const src: mpz_t): Boolean;
begin
   Result := (src.mp_size <> 0) and Odd(src.mp_d^);
end;

function mpz_even_p(const src: mpz_t): Boolean;
begin
   Result := (src.mp_size = 0) or not Odd(src.mp_d^);
end;

function mpq_numref(const src: mpq_t): pmpz_t;
begin
   Result := @src.mp_num;
end;

function mpq_denref(const src: mpq_t): pmpz_t;
begin
   Result := @src.mp_den;
end;

procedure mpz_set_uint64(var dest: mpz_t; const src: UInt64); // by delphi code
type
   _UINT64 = record
      m_lo: UInt32;
      m_hi: UInt32;
   end;
begin
   if _UINT64(src).m_hi = 0 then begin
      mpz_set_ui(dest, _UINT64(src).m_lo);
   end else begin
      mpz_set_ui(dest, _UINT64(src).m_hi);
      mpz_mul_2exp(dest, dest, 32);
      mpz_add_ui(dest, dest, _UINT64(src).m_lo);
   end;
end;

procedure mpz_set_int64(var dest: mpz_t; const src: Int64); // by delphi code
var
   u64: UInt64;
begin
   if src < 0 then begin
      u64 := Abs(src);
      mpz_set_uint64(dest, u64);
      dest.mp_size := -dest.mp_size;
   end else begin
      mpz_set_uint64(dest, src);
   end;
end;

function mpz_sgn(const src: mpz_t): Integer;
begin
   if src.mp_size < 0 then
      Result := -1
   else if src.mp_size > 0 then
      Result := 1
   else Result := 0;
end;

function mpq_sgn(const src: mpq_t): Integer;
begin
   if src.mp_num.mp_size < 0 then
      Result := -1
   else if src.mp_num.mp_size > 0 then
      Result := 1
   else Result := 0;
end;

function mpf_sgn(const src: mpf_t): Integer;
begin
   if src.mp_size < 0 then
      Result := -1
   else if src.mp_size > 0 then
      Result := 1
   else Result := 0;
end;

function GetExp(var x: mpf_t): Integer;
begin
   mpf_get_d_2exp(Result, x);
end;

const
   PREC_PRO = 25; // To keep the precision of intermediate calculation.

procedure mpf_exp(var dest: mpf_t; const src: mpf_t);
var
   y, s, c0: mpf_t;
   precision, n: Cardinal;
   exp, i: mp_exp_t;
   negative: Boolean;
begin
   precision := mpf_get_prec(dest) + PREC_PRO;
   mpf_init2(y, precision);
   mpf_set(y, src);
   mpf_set_ui(dest, 1);
   negative := mpf_sgn(y) < 0;
   if negative then
      mpf_neg(y, y);
   exp := GetExp(y);
   if exp > 0 then
      mpf_div_2exp(y, y, exp);
   mpf_init2(c0, precision);
   mpf_init2(s, precision);
   mpf_set_ui(s, 1);
   n := 1;
   repeat
      mpf_mul(s, s, y);
      mpf_div_ui(s, s, n);
      mpf_set(c0, dest);
      mpf_add(dest, dest, s);
      Inc(n)
   until mpf_eq(c0, dest, precision) <> 0;
   for i := 1 to exp do
      mpf_mul(dest, dest, dest);
   if negative then
      mpf_ui_div(dest, 1, dest);
   mpf_clear(s);
   mpf_clear(c0);
   mpf_clear(y);
end;

var
   LnHalf: mpf_t;
   LnHalfInited: Boolean = False;

procedure mpf_ln(var dest: mpf_t; const src: mpf_t);
var
   y, s, p, c0, half: mpf_t;
   n, precision: Cardinal;
   exp: mp_exp_t;
begin
   if mpf_sgn(src) <= 0 then
      raise EMathError.Create('Invalid argument for Ln');
   precision := mpf_get_prec(dest) + PREC_PRO;
   mpf_init2(y, precision);
   mpf_set(y, src);
   mpf_set_ui(dest, 0);
   exp := GetExp(y);
   if exp <> 0 then begin
      if not LnHalfInited or (mpf_get_prec(LnHalf) < precision) then begin
         if LnHalfInited then
            mpf_clear(LnHalf);
         LnHalfInited := True;
         mpf_init2(LnHalf, precision);
         mpf_init2(half, precision);
         mpf_set_d(half, 0.5);
         mpf_ln(LnHalf, half);
         mpf_clear(half)
      end;
      mpf_set(dest, LnHalf);
      mpf_mul_ui(dest, dest, Abs(exp));
      if exp > 0 then begin
         mpf_neg(dest, dest);
         mpf_div_2exp(y, y, exp)
      end else begin
         mpf_mul_2exp(y, y, -Exp)
      end;
   end;
   mpf_ui_sub(y, 1, y);
   mpf_init2(c0, precision);
   mpf_init2(s, precision);
   mpf_init2(p, precision);
   mpf_set_si(p, -1);
   n := 1;
   repeat
      mpf_mul(p, p, y);
      mpf_div_ui(s, p, n);
      mpf_set(c0, dest);
      mpf_add(dest, dest, s);
      Inc(n)
   until mpf_eq(c0, dest, precision) <> 0;
   mpf_clear(p);
   mpf_clear(s);
   mpf_clear(c0);
   mpf_clear(y);
end;

procedure mpf_pow(var dest: mpf_t; const src1, src2: mpf_t);
var
   temp: mpf_t;
begin
   mpf_init2(temp, mpf_get_prec(src1) + PREC_PRO);
   mpf_ln(temp, src1);
   mpf_mul(temp, temp, src2);
   mpf_exp(dest, temp);
   mpf_clear(temp);
end;
(*
var
   SqRtTwo: mpf_t;
   SqRtTwoInited: Boolean = False;

procedure mpf_arctan(var dest: mpf_t; const src: mpf_t);
var
   precision, n: Cardinal;
   xx, mx2, a, b: mpf_t;
begin
   precision := mpf_get_prec(dest) + PREC_PRO;
   mpf_init2(xx, precision);
   mpf_init2(mx2, precision);
   mpf_init2(a, precision);
   mpf_init2(b, precision);
   mpf_abs(xx, src);
   if not SqRtTwoInited or (mpf_get_prec(SqRtTwo) < precision) then begin
      if SqRtTwoInited then
         mpf_clear(SqRtTwo);
      SqRtTwoInited := True;
      mpf_init2(SqRtTwo, precision);
      mpf_sqrt_ui(SqRtTwo, 2)
   end;
   mpf_add_ui(a, SqRtTwo, 1);
   if mpf_cmp(xx, a) > 0 then begin
      mpf_pi(dest);
      mpf_div_2exp(dest, dest, 1);
      mpf_ui_div(xx, 1, xx);
      mpf_neg(xx, xx)
   end else begin
      mpf_sub_ui(b, SqRtTwo, 1);
      if mpf_cmp(xx, b) > 0 then begin
         mpf_pi(dest);
         mpf_div_2exp(dest, dest, 2);
         mpf_sub_ui(a, xx, 1);
         mpf_add_ui(b, xx, 1);
         mpf_div(xx, a, b)
      end else begin
         mpf_set_ui(dest, 0)
      end;
   end;
   mpf_mul(mx2, xx, xx);
   mpf_neg(mx2, mx2);
   mpf_add(dest, dest, xx);
   n := 1;
   repeat
      mpf_mul(xx, xx, mx2);
      mpf_div_ui(a, xx, 2 * n + 1);
      mpf_set(b, dest);
      mpf_add(dest, dest, a);
      Inc(n)
   until mpf_eq(b, dest, precision) <> 0;
   if mpf_sgn(src) < 0 then
      mpf_neg(dest, dest);
   mpf_clear(xx);
   mpf_clear(mx2);
   mpf_clear(a);
   mpf_clear(b);
end;

var
   _Pi: mpf_t;
   PiInited: Boolean = False;

procedure mpf_pi(var dest: mpf_t);
{ 4 arctan 1/5 - arctan 1/239 = pi/4 }
var
   b: mpf_t;
   Precision: Cardinal;
begin
   Precision := mpf_get_prec(dest) + PREC_PRO;
   if not PiInited or (mpf_get_prec(_Pi) < Precision) then begin
      if PiInited then
         mpf_clear(_Pi);
      PiInited := True;
      mpf_init2(_Pi, Precision);
      mpf_set_ui(_Pi, 1);
      mpf_div_ui(_Pi, _Pi, 5);
      mpf_arctan(_Pi, _Pi);
      mpf_mul_ui(_Pi, _Pi, 4);
      mpf_init2(b, Precision);
      mpf_set_ui(b, 1);
      mpf_div_ui(b, b, 239);
      mpf_arctan(b, b);
      mpf_sub(_Pi, _Pi, b);
      mpf_mul_ui(_Pi, _Pi, 4);
      mpf_clear(b)
   end;
   mpf_set(dest, _Pi);
end;

procedure mpf_sin(var dest: mpf_t; const src: mpf_t);
var
   precision, quadrant, n: Cardinal;
   sign: Integer;
   a, b, z, xx, c0: mpf_t;
begin
   precision := mpf_get_prec(dest) + PREC_PRO;
   mpf_init2(a, precision);
   mpf_init2(b, precision);
   mpf_init2(z, precision);
   mpf_init2(xx, precision);
   mpf_init2(c0, precision);
   sign := mpf_sgn(src);
   mpf_abs(xx, src);
   mpf_pi(z);
   mpf_div_2exp(z, z, 1);
   mpf_div(a, xx, z);
   mpf_floor(xx, a);
   if mpf_cmp_ui(xx, 4) >= 0 then begin
      mpf_div_2exp(b, xx, 2);
      mpf_floor(b, b);
      mpf_mul_2exp(b, b, 2);
      mpf_sub(b, xx, b)
   end else begin
      mpf_set(b, xx);
   end;
   quadrant := mpf_get_ui(b);
   mpf_sub(b, a, xx);
   mpf_mul(xx, z, b);
   if quadrant > 1 then
      sign := -Sign;
   if Odd(quadrant) then
      mpf_sub(xx, z, xx);
   mpf_mul(z, xx, xx);
   mpf_neg(z, z);
   n := 1;
   mpf_set_ui(b, 1);
   mpf_set_ui(dest, 1);
   repeat
      Inc(n);
      mpf_div_ui(b, b, n);
      Inc(n);
      mpf_div_ui(b, b, n);
      mpf_mul(b, b, z);
      mpf_set(c0, dest);
      mpf_add(dest, dest, b)
   until mpf_eq(c0, dest, precision) <> 0;
   mpf_mul(dest, dest, xx);
   if sign < 0 then
      mpf_neg(dest, dest);
   mpf_clear(a);
   mpf_clear(b);
   mpf_clear(z);
   mpf_clear(xx);
   mpf_clear(c0);
end;

procedure mpf_cos(var dest: mpf_t; const src: mpf_t);
var
   temp: mpf_t;
begin
   mpf_init2(temp, mpf_get_prec(dest) + PREC_PRO);
   mpf_pi(temp);
   mpf_div_2exp(temp, temp, 1);
   mpf_sub(temp, temp, src);
   mpf_sin(dest, temp);
   mpf_clear(temp);
end;
*)
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vBindMRSW := TMultiReadSingleWrite.Create;

finalization

(*
   if LnHalfInited then
      mpf_clear(LnHalf);

   if SqRtTwoInited then
      mpf_clear(SqRtTwo);

   if PiInited then
      mpf_clear(_Pi);
*)

   FreeAndNil(vBindMRSW);

end.
