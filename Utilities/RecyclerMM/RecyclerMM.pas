// RecyclerMM
{: Egg<p>

	Recycling Memory Manager (aka RMM).<p>
   Provides high-speed allocation/release of highly-aligned memory
   via a segregated storage algorithm (for small and medium blocks)
   and a virtual heap (large blocks).<br>
   Supports Shared Memory (like ShareMem, but no DLL required).<p>

   Copyright 2005 - Creative IT / Eric Grange<br>
   Default licensing is GPL, use under MPL can be granted (on request, for free)
   for users/companies "supporting" Open Source (purely subjective decision by us)<p>

   Implementation Notes:<ul>
      <li>Shared Memory support is implemented through the creation of a Local
         Atom and a (never visible) window, which allow main EXE/DLLs modules
         to be aware of each other's RecyclerMM support and thus, reuse a single
         manager instance (which may be one from the main exe, or the one of
         the statically linked DLLs, depending on initialization order).
      <li>Small blocks chunks and batches are allocated at the top of the address
         space, large blocks at the bottom.
      <li>Use of the Delphi 7 SP1 is *NOT* recommended, not because it won't work,
         but because bugs introduced in the SP1 register allocator will generate
         sub-optimal code. If you really need SP1, apply the patch first then
         manually revert DCC32.EXE and DCC70.DLL to their original state
   </ul><p>

	<b>History : </b><font size=-1><ul>
      <li>19/04/05 - EG - Work on version 2.0 started
	   <li>28/10/03 - EG - Creation of Version 1.0
	</ul></font>
}
unit RecyclerMM;

interface

{$OPTIMIZATION ON}
{$STACKFRAMES OFF}
{$WRITEABLECONST OFF}
{$BOOLEVAL OFF}

{$ifdef VER150}   // of course it's "unsafe", so no warnings plz
   {$WARN UNSAFE_CODE OFF}
   {$WARN UNSAFE_TYPE OFF}
{$endif}

// No debug info, so the debugger won't step through memory management code
{.$D-}

uses Windows;

// if set the RecyclerMM will automatically bind itself as default memory manager
{$define AUTO_BIND}

// If set, RecyclerMM will automatically locate and share memory with other
// RecyclerMMs in DLL modules (same functionality as Borland's ShareMem unit).
// Sharing will only happen with compatible RMMs.
{.$define SHARE_MEM}

// If set the (possible) BPLs won't be patched, only the jump table will
{.$define NO_BPL_PATCHING}

// If set SSE code for Move16/Clear16 will be allowed
// Mileage on the efficiency of SSE over the FPU-based transfer may vary,
// you may want to test it and figure out which is best in your case. Typically,
// the FPU approach will be good for lots of small or scattered blocks on AMD
// CPUs, while SSE shines on large blocks with a P4
{$define ALLOW_SSE}

// If set and exception will be explicitly raised if your code attempts
// to release a block that isn't allocated. By default, RMM only detects
// that issue reliably for large blocks and signals the issue to the Borland RTL,
// which may then raise an exception. But in some circumstances, the RTL will
// just ignore the issue. When the option is active, RMM will accurately detect
// this issue all the time, and trigger an exception itself.
// Doing so incurs a performance penalty on block release, and should preferably
// only be used for testing or if memory integrity is of primary importance
{$define RAISE_EXCEPTION_ON_INVALID_RELEASE}

// Delayed release affects the management of the pool of free memory retained
// by the manager. In delayed mode, 1/4th of the freed blocks are returned
// to the OS every 250 ms, with by default up to 64 MB of memory whose release
// is delayed. In non-delayed mode, the pool is fully retained all the time
// (but the default pool size is much smaller at 8 MB)
// This can improve performance as blocks will linger a bit before being
// returned to the OS, but can temporarily increase memory consumption
// (should be harmless most of the time, as the memory is still usable
// by your application, just not usable for other applications within
// a small delay).
// This option is automatically turned off in DLLs
{$define ALLOW_DELAYED_RELEASE}

// If set usage of memory mapped files for large blocks will be allowed,
// this can have a significant performance impact on frequently reallocated
// large blocks, as it bypasses most of the copy on reallocation.
// However, as it stresses the file system, it may exhibit performance side
// effects if the application allocates a very large number of large blocks.
// Note that memorymapping will only be used for reallocated blocks, there is
// thus no penalty for statically allocated large blocks.
{$define ALLOW_MEMORYMAPPED_LARGE_BLOCKS}

// If set RMMUsageSnapShot functions will be available.
// These functions allow generating a diagnostic and memory map report
{.$define ALLOW_USAGE_SNAPSHOT}

// Selection of the locking mechanim
// If set, Windows CriticalSections will be used, otherwise it will be SpinLocks
{.$define USE_CRITICAL_SECTIONS}

// compile error when incompatible options have been selected
{$ifdef SHARE_MEM}
{$ifdef ALLOW_DELAYED_RELEASE}
   Error : you cannot combine ALLOW_DELAYED_RELEASE and SHARE_MEM (yet)
{$endif}
{$endif}

const
   // Ratio for ReallocDownSizing (4 = downsizing will happen if only 1/4 used)
   cSMBReallocDownSizing   = 4;
   cLGBReallocDownSizing   = 4;

   // Ratio for upsizing (1 = allocate only what's needed, 2 = allocate twice the
   //                     needed space, etc. Must be >= 1.0 or things will go banana )
   cReallocUpSizing        = 1.1;
   cReallocUpSizing256     = Word(Round(cReallocUpSizing*256)); // what's actualy used internally
   cReallocUpSizingLimit   = Cardinal(1 shl 31) div cReallocUpSizing256;
   cReallocUpSizingLGBLimit= Round((1 shl 30)/cReallocUpSizing);
   cReallocUpSizingSMBPad  = 48;
   cReallocMinSize         = 64;

   // Size and Index limits for SMBs
   cSMBMaxSizeIndex        = 49;
   cSMBSizes               : packed array [0..cSMBMaxSizeIndex] of Word = (
         // 52 values from an exponential curve manually adjusted to "look nice" :)
         16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224,
         240, 256, 288, 320, 384, 432, 496, 576, 656, 752, 864, 976, 1120,
         1280, 1472, 1712, 2032, 2256, 2512, 2848, 3264, 3840, 4352, 4672, 5024,
         5456, 5952, 6544, 7264, 8176, 9344, 10912, 13088, 16368, 21824, 32752 );

   // Maximum Size (bytes) of blocks managed by SMBs (max 64kB)
   cSMBMaxSize             = 32752;

   // Size of chunks to retrieve from the OS
   cOSChunkSize = 1024*1024;      // 640 kB should be enough for everyboy, no?
   cOSChunkRandomOffset = 4096;  // max size of random offset
   cOSChunkItemSize = 65536;
   cOSChunkBlockCount = (cOSChunkSize-cOSChunkRandomOffset) div cOSChunkItemSize;

   // Amount of memory who's delayed release of the next seconds is tolerated
   cOSDelayedAllowedMemoryLatency = 8*1024*1024;   // 8 MB
   cOSDelayedAllowedChunksLatency = cOSDelayedAllowedMemoryLatency div cOSChunkSize;

   {$ifdef ALLOW_MEMORYMAPPED_LARGE_BLOCKS}
   // minimal size of LGB before memory mapping mode is allowed to kick in
   cMemoryMappedLargeBlocksMinSize = 1024*1024;
   {$endif}

type

   // TRMMStatus
   //
   TRMMStatus = (rmmsUnallocated, rmmsAllocated, rmmsReserved,
                 rmmsSysAllocated, rmmsSysReserved);

   // TRMMMemoryMap
   //
   {: Describes a 64 kB range of the RMM memory use.<p>
      This structure isn't used by RMM itself, it's used to report the status
      of the memory allocation in RMMUsageSnapShot. }
   TRMMMemoryMap = packed record
      StartAddr : Pointer;          // Start of address range
      Length : Cardinal;            // Length of address range (bytes)
      AllocatedUserSize : Cardinal; // Bytes in range allocated by user
      Status : TRMMStatus;          // Status of address range
   end;
   PRMMMemoryMap = ^TRMMMemoryMap;

   // TRMMSMBStat
   //
   TRMMSMBStat = packed record
      BlockSize : Cardinal;
      AllocatedBlocks : Cardinal;
      AllocatedUserSize : Cardinal;
   end;
   PRMMSMBStat = ^TRMMSMBStat;

const
   // As a constant, we make our mem map big enough to support 3GB addressing
   // If you really need the extra 64 kB and know that your code will never be
   // run in /3GB mode then you can reduce this to 32767.
   // The actual limit of the memmap is in vMemMapUpper.
   cMemMapUpperMax = 49151;
   // Now the two we chose between to use as the actual limit within the array
   cMemMapUpper2GB = 32767;
   cMemMapUpper3GB = 49151;

type
   // TRMMUsageBench
   //
   TRMMUsageBench = packed record
      TotalTime : Int64;      // in CPU ticks!
      NbCalls : Cardinal;
   end;

{$ifdef ALLOW_USAGE_SNAPSHOT}
   // TRMMUsageSnapShot
   //
   {: RMM usage diagnostic snapshot, returned by RMMUsageSnapShot. }
   TRMMUsageSnapShot = packed record
      // RMM Stats
      TotalVirtualAllocated : Cardinal;
      AllocatedBlocks : Cardinal;
      AllocatedUserSize : Cardinal;
      // Virtual Memory Stats
      TotalVMSpace : Cardinal;
      SystemAllocatedVM : Cardinal;
      SystemReservedVM : Cardinal;
      LargestFreeVM : Cardinal;
      // Map
      NbMapItems : Cardinal;
      Map : packed array [0..cMemMapUpperMax] of TRMMMemoryMap;
      SMBStats : packed array [0..cSMBMaxSizeIndex] of TRMMSMBStat;
      // Usage
      BenchRGetMem : TRMMUsageBench;
      BenchRReallocMem : TRMMUsageBench;
      BenchRFreeMem : TRMMUsageBench;
   end;
   PRMMUsageSnapShot = ^TRMMUsageSnapShot;
{$endif}

{: Fast 16 bytes-based move.<p>
   Copies blocks of 16 bytes only, ie. Count is rounded up to the nearest
   multiple of 16. Overlapping source/destination are not handled. }
var Move16 : procedure (const Source; var Dest; Count: Integer); register;
{: Fills an area whose size is a multiple of 16-bytes with zeros.<p>
   Count is rounded up to the nearest multiple of 16 }
var MemClear16 : procedure (const Buffer; Count: Integer); register;

// Direct access functions - only for single .EXE with no RMM DLLs

function RGetMem(Size : Integer) : Pointer;
function RAllocMem(Size : Cardinal) : Pointer;
function RFreeMem(P : Pointer) : Integer;
function RReallocMem(P : Pointer; Size : Cardinal) : Pointer;
function RAllocated(const P : Pointer) : Boolean;

{: True if P points to the beginning of an allocated block.<p> }
var Allocated : function (const P : Pointer) : Boolean;

{: Generates a memory map of RMM memory usage.<p>
   While the map is generated, all RMM activity is freezed. }
{$ifdef ALLOW_USAGE_SNAPSHOT}
function RMMUsageSnapShot : TRMMUsageSnapShot; overload;
procedure RMMUsageSnapShot(var result : TRMMUsageSnapShot); overload;
{$endif}

procedure BindRMM;
procedure UnBindRMM;
function  RMMActive : Boolean;

procedure InitializeRMM;
procedure FinalizeRMM;

function RunningIn3GBMode : Boolean;

var
   // Number of entries in memmap array
   vMemMapUpper : Cardinal;
   // Virtual memory limit (used for SECURE_MEMMAP)
   vVirtualLimit : Cardinal;

const
   // Unused, this is just to have it in clear in the DCU
   cRecyclerMMCopyright = 'RecyclerMM - ©2005 Creative IT';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cMAX_PATH = 512;
   cBAADFOOD = $BAADF00D;

type
   PPointer = ^Pointer;
   TPointerArrayMap = packed array [0..cMemMapUpperMax] of Pointer;

   TWordArray = packed array [0..MaxInt shr 2] of Word;
   PWordArray = ^TWordArray;
   TCardinalArray = packed array [0..MaxInt shr 3] of Cardinal;
   PCardinalArray = ^TCardinalArray;

   PSMBManager = ^TSMBManager;
   POSChunk = ^TOSChunk;

   {$ifdef USE_CRITICAL_SECTIONS}
   TCSLock = TRTLCriticalSection;
   {$else}
   TCSLock = LongBool;
   {$endif}
   PCSLock = ^TCSLock;

   TMemoryRange = packed record
      Start : Pointer;
      Length : Cardinal;
   end;

   // TSMBLinkedList
   //
   TSMBLinkedList = packed record
      First, Last : PSMBManager;
   end;

   // TSMBInfo
   //
   {: SmallBlock management info for a given size.<p> }
   TSMBInfo = packed record
      CSLock : TCSLock;
      FreeSMBs : TSMBLinkedList;
      FullSMBs : TSMBLinkedList;
      Size : Cardinal;
      BlocksPerSMB : Cardinal;
      DownSizingSize : Cardinal;
   end;
   PSMBInfo = ^TSMBInfo;

   // TSMBManager
   //
   {: Manages a Small Blocks chunk.<p>
      Small blocks manage many user blocks of constant (BlockSize) size,
      which are allocated/freed in a stack-like fashion. }
   TSMBManager = packed record
      SMBInfo : PSMBInfo;           // pointer to the SMBInfo (size related)
      NbFreeBlocks : Cardinal;
      FirstFreedBlock : Pointer;
      Next, Prev : PSMBManager;     // pointer to the next/prev managers
      MaxNbFreeBlocks : Cardinal;
      BlockSize : Cardinal;         // Size of blocks in SMB
      BlockStart : Pointer;         // base address for SMB blocks
      DownSizingSize : Cardinal;
      NextNonAllocatedBlkID : Pointer;
      Padding : packed array [1..6] of Cardinal;
   end;

   // TLGBManager
   //
   {: Manages a Large Block.<p>
      LGBs each manage a single user-allocated block. They are allowed to
      reserve address space (to improve the chances of in-place growth). }
   PLGBManager = ^TLGBManager;
   TLGBManager = record
      BlockSize : Cardinal;      // Total allocated size for the block
      DataStart : Pointer;       // Start of user data
      DataSize : Cardinal;       // Size requested by the user
      MaxDataSize : Cardinal;    // Maximum size without reallocation
      Next, Prev : PLGBManager;
      hFile, hMapping : Cardinal;// handles for memory mapping
   end;

   // TOSChunk
   //
   {: A range of heap-managed SMB or small LGB space }
   TOSChunk = packed record
      Prev, Next : POSChunk;
      FreeBlocks : Integer;
      Full : LongBool;
      FirstBlock : Cardinal;
      Manager : packed array [0..cOSChunkBlockCount-1] of TSMBManager;
   end;

   // TSharedMemoryManager
   //
   {: Extends TMemoryManager to accomodate RMM functions.<p>
      This structure is what RMMs cross-refer when sharing memory. }
   TSharedMemoryManager = record
      MemoryManager : TMemoryManager;
      Allocated : function(const P : Pointer) : Boolean;
      {$ifdef ALLOW_USAGE_SNAPSHOT}
      RMMUsageSnapShot : function : TRMMUsageSnapShot;
      {$endif}
   end;
   PSharedMemoryManager = ^TSharedMemoryManager;

var
   // Only the lower 2 or 3 GB are accessible to an application under Win32,
   // that's a maximum of 32768 or 49152 blocks which are all mapped by a 128/192 kB array
   vRunningIn3GBMode : Boolean;

   {$ifdef ALLOW_DELAYED_RELEASE}
   // ID of the cleanup thread
   vCleanupThreadID : Cardinal;
   vCleanupThreadHnd : Cardinal;
   vCleanupThreadEvent : Cardinal;
   {$endif}

   vMemoryMap : TPointerArrayMap;

   // Binding variables
   vOldMemoryManager : TMemoryManager;
   vRMMBound : Integer;
   {$ifdef ALLOW_SSE}
   vSSESupported : Integer;
   {$endif}

   // Shared memory variables
   vSharedMemoryManager : TSharedMemoryManager;
   {$ifdef SHARE_MEM}
   vSharedMemory_Data : HWND;
   vSharedMemory_DataName : ShortString = '########-RecyclerMM-100'#0;
   vSharedMemory_InUse : Boolean;
   {$endif}

   // Chunks pool
   vOSChunksLock : TCSLock;
   vOSChunksFirst : POSChunk;
   vOSChunksFirstFull : POSChunk;
   {$ifdef ALLOW_DELAYED_RELEASE}
   vOSChunkNbEntirelyFree : Integer;
   {$endif}

   // SMB information array by size class (index)
   vSMBs : array [0..cSMBMaxSizeIndex] of TSMBInfo;
   vSMBSizeToPSMBInfo : packed array [0..(cSMBMaxSize-1) shr 4] of Byte;

   // Large blocks are just chained
   vLGBManagers : PLGBManager;
   vLGBLock : TCSLock;

   // Temporary path for memorymapped temp files
   vTemporaryFilesPath : array [0..cMAX_PATH] of Char;

// RunningIn3GBMode
//
function RunningIn3GBMode : Boolean;
begin
   Result:=vRunningIn3GBMode;
end;

// SwitchToThread logic
//
var vSwitchToThread : procedure; stdcall;
procedure Win9xSwitchToThread; stdcall;
begin
   Sleep(0);
end;
procedure InitializeSwitchToThread;
var
   hLib : Cardinal;
begin
   hLib:=LoadLibrary('Kernel32.dll');
   vSwitchToThread:=GetProcAddress(hLib, 'SwitchToThread');
   FreeLibrary(hLib);
   if not Assigned(vSwitchToThread) then
      vSwitchToThread:=@Win9xSwitchToThread;
end;

// RaiseInvalidPtrError
//
procedure RaiseInvalidPtrError;
begin
   RunError(204); // Invalid pointer operation
end;

// InitializeCSLock
//
procedure InitializeCSLock(var csLock : TCSLock);
begin
   {$ifdef USE_CRITICAL_SECTIONS}
   InitializeCriticalSection(csLock);
   {$else}
   csLock:=False;
   {$endif}
end;

// DeleteCSLock
//
procedure DeleteCSLock(var csLock : TCSLock);
begin
   {$ifdef USE_CRITICAL_SECTIONS}
   DeleteCriticalSection(csLock);
   {$endif}
end;

// LockCmpxchg
//
function LockCmpXchg(compareVal, newVal : Byte; anAddress : PByte) : Byte;
// AL = compareVal, dl = newVal, ecx = anAddress
asm
   lock cmpxchg [ecx], dl
end;

// CSLockEnter
//
procedure CSLockEnter(var csLock : TCSLock);
{$ifdef USE_CRITICAL_SECTIONS}
begin
   if IsMultiThread then
      EnterCriticalSection(csLock); //}
{$else}
begin
   if IsMultiThread then begin
      while LockCmpxchg(0, 1, @csLock)<>0 do begin
         vSwitchToThread;
         if LockCmpxchg(0, 1, @csLock)=0 then
            Break;
         Windows.Sleep(10);
      end;
   end; // }
{asm
      cmp   byte ptr [IsMultiThread], 0
      jz    @@LockDone

      mov   ecx, eax
      xor   eax, eax
      mov   dl, 1
      lock  cmpxchg [ecx], dl
      jz    @@LockDone

      push  ebx
      mov   ebx, ecx
      call  [vSwitchToThread]

@@LockLoop:
      xor   eax, eax
      mov   dl, 1
      lock  cmpxchg [ebx], dl
      jz    @@LockEntered

      push  10
      call  Windows.Sleep
      jmp   @@LockLoop

@@LockEntered:
      pop   ebx

@@LockDone: //}
{$endif}
end;

// CSLockTryEnter
//
function CSLockTryEnter(var csLock : TCSLock) : Boolean;
begin
   {$ifdef USE_CRITICAL_SECTIONS}
   Result:=(not IsMultiThread) or (TryEnterCriticalSection(csLock));
   {$else}
   Result:=(not IsMultiThread) or (LockCmpxchg(0, 1, @csLock)=0);
   {$endif}
end;

// CSLockLeave
//
procedure CSLockLeave(var csLock : TCSLock);
begin
   {$ifdef USE_CRITICAL_SECTIONS}
   if IsMultiThread then
      LeaveCriticalSection(csLock);
   {$else}
   csLock:=False;
   {$endif}
end;

// MMRandom
//
var vRandomLast : Cardinal;
function MMRandom : Integer;
begin
   vRandomLast:=(3877*vRandomLast+29573) mod 139968;
   Result:=vRandomLast xor (vRandomLast shr 8);
end;

// UpdateMemoryMap
//
procedure UpdateMemoryMap(baseAddr : Pointer; size : Cardinal; manager : Pointer);
var
   i : Cardinal;
begin
   for i:=(Cardinal(baseAddr) shr 16) to ((Cardinal(baseAddr)+size-1) shr 16) do
      vMemoryMap[i]:=manager;
end;

// CreateTemporaryFileAndMapping
//    returns False if failed
function CreateTemporaryFile(var hFile : Cardinal) : Boolean;
var
   tempFileName : array [0..cMAX_PATH] of Char;
begin
   GetTempFileName(@vTemporaryFilesPath[0], 'RMM', 0, @tempFileName[0]);
   hFile:=Windows.CreateFile(@tempFileName[0], GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
   Result:=(hFile<>INVALID_HANDLE_VALUE);
end;

// SetTemporaryFileSizeAndRemap
//    returns pointer to mapped space, nil if failed
function SetTemporaryFileSizeAndMap(const hFile, newSize : Cardinal;
                                    var hMapping : Cardinal) : Pointer;
begin
   Result:=nil;
   SetFilePointer(hFile, newSize, nil, FILE_BEGIN);
   if SetEndOfFile(hFile) then begin
      hMapping:=CreateFileMapping(hFile, nil, PAGE_READWRITE, 0, 0, nil);
      if (hMapping<>0) and (hMapping<>ERROR_ALREADY_EXISTS) then begin
         Result:=MapViewOfFile(hMapping, FILE_MAP_WRITE, 0, 0, newSize);
         if Result=nil then begin
            CloseHandle(hMapping);
            hMapping:=0;
         end;
      end;
   end;
end;

// CutOutChunk
//
procedure CutOutChunk(chunk : POSChunk);
begin
   if chunk.Full then begin
      // cut from full
      if chunk.Prev=nil then
         vOSChunksFirstFull:=chunk.Next
      else chunk.Prev.Next:=chunk.Next;
      if chunk.Next<>nil then
         chunk.Next.Prev:=chunk.Prev;
   end else begin
      // cut from non-full
      if chunk.Prev=nil then
         vOSChunksFirst:=chunk.Next
      else chunk.Prev.Next:=chunk.Next;
      if chunk.Next<>nil then
         chunk.Next.Prev:=chunk.Prev;
   end;
end;

// DestroyChunk
//
procedure DestroyChunk(chunk : POSChunk);
begin
   if chunk.Full then RunError(103);
   if chunk.FreeBlocks<>cOSChunkBlockCount then RunError(104);
   chunk:=Pointer(Cardinal(chunk) and $FFFF0000);
   UpdateMemoryMap(chunk, cOSChunkSize, nil);
   VirtualFree(chunk, 0, MEM_RELEASE);
end;

// RMMAllocChunkItem
//
function RMMAllocChunkItem : PSMBManager;
var
   chunk : POSChunk;
   chunkRandomOffset : Cardinal;
   i : Integer;
   alignedAddress : Cardinal;
begin
   CSLockEnter(vOSChunksLock);

   chunk:=vOSChunksFirst;
   if chunk=nil then begin
      // all chunks full, allocate a new one
      chunk:=VirtualAlloc(nil, cOSChunkSize, MEM_COMMIT, PAGE_READWRITE);
      if chunk=nil then begin
         Result:=nil;
         Exit;
      end;
      // randomize chunk location by up to 4 kB
      chunkRandomOffset:=(((cOSChunkRandomOffset div 16)-1) and MMRandom)*16;
      while chunkRandomOffset+SizeOf(TOSChunk)>4096 do
         Dec(chunkRandomOffset, 16);
      // update MM and initialize chunk structure
      UpdateMemoryMap(chunk, cOSChunkSize, Pointer(Cardinal(chunk)+chunkRandomOffset));
      Inc(Cardinal(chunk), chunkRandomOffset);
      chunk.FreeBlocks:=cOSChunkBlockCount;
      alignedAddress:=(Cardinal(chunk)+SizeOf(TOSChunk)+15) and $FFFFFFF0;
      chunk.FirstBlock:=alignedAddress;
      for i:=0 to cOSChunkBlockCount-1 do
         chunk.Manager[i].BlockStart:=nil;
      // place in linked list
      chunk.Prev:=nil;
      chunk.Next:=vOSChunksFirst;
      if vOSChunksFirst<>nil then
         vOSChunksFirst.Prev:=chunk;
      vOSChunksFirst:=chunk;
   end;

   i:=0;
   while chunk.Manager[i].BlockStart<>nil do Inc(i);
   Result:=@chunk.Manager[i];
   Result.BlockStart:=Pointer(chunk.FirstBlock+Cardinal(i)*cOSChunkItemSize);
   Dec(chunk.FreeBlocks);

   // if we filled this one up, move it to the full chunks
   if chunk.FreeBlocks=0 then begin
      // cut from non-full
      CutOutChunk(chunk);
      // paste to full
      chunk.Full:=True;
      chunk.Next:=vOSChunksFirstFull;
      if vOSChunksFirstFull<>nil then
         vOSChunksFirstFull.Prev:=chunk;
      vOSChunksFirstFull:=chunk;
      chunk.Prev:=nil;
   end;

   CSLockLeave(vOSChunksLock);
end;

// RMMVirtualFreeChunkItem
//
procedure RMMVirtualFreeChunkItem(p : Pointer);
var
   i : Integer;
   chunk : POSChunk;
begin
   CSLockEnter(vOSChunksLock);

   // identify the chunk for the pointer
   chunk:=POSChunk(vMemoryMap[Cardinal(p) shr 16]);
   if (Cardinal(chunk) and 1)<>0 then
      RaiseInvalidPtrError;
   chunk:=POSChunk(Cardinal(Chunk) and $FFFFFFF0);

   // release
   i:=(Cardinal(p)-chunk.FirstBlock) div cOSChunkItemSize;
   chunk.Manager[i].BlockStart:=nil;
   Inc(chunk.FreeBlocks);

   if chunk.Full then begin
      // we're no longer full, cut from full
      CutOutChunk(chunk);
      // paste to non-full
      chunk.Full:=False;
      chunk.Next:=vOSChunksFirst;
      if vOSChunksFirst<>nil then
         vOSChunksFirst.Prev:=chunk;
      vOSChunksFirst:=chunk;
      chunk.Prev:=nil;
   end;

   // if completely freed and not the only chunk, cleanup
   if (chunk.FreeBlocks=cOSChunkBlockCount) then begin
      {$ifndef ALLOW_DELAYED_RELEASE}
      if (chunk.Prev<>nil) or (chunk.Next<>nil) then begin
         CutOutChunk(chunk);
         DestroyChunk(chunk);
      end;
      {$else}
      Inc(vOSChunkNbEntirelyFree);
      if vOSChunkNbEntirelyFree>cOSDelayedAllowedChunksLatency then begin
         if vCleanupThreadID<>0 then
            SetEvent(vCleanupThreadEvent);
      end;                   
      {$endif}
   end;

   CSLockLeave(vOSChunksLock);
end;

// RMMVirtualAlloc
//
function RMMVirtualAlloc(const blkSize : Cardinal) : Pointer;
begin
   Result:=VirtualAlloc(nil, blkSize, MEM_COMMIT+MEM_TOP_DOWN, PAGE_READWRITE);
end;

// RMMVirtualFree
//
procedure RMMVirtualFree(p : Pointer; const blkSize : Cardinal);
begin
   VirtualFree(p, 0, MEM_RELEASE);
end;

// ComputeLGBBlockSize
//
function ComputeLGBBlockSize(dataSize : Cardinal) : Cardinal;
var
   baseOffset : Cardinal;
begin
   baseOffset:=(SizeOf(TLGBManager)+15) and $FFFFFFF0;
   Result:=((dataSize+baseOffset+$FFFF) and $FFFF0000);
end;

// ComputeLGBDataStart
//
function ComputeLGBDataStart(p : Pointer; blockSize, dataSize : Cardinal) : Pointer;
var
   baseOffset, margin, randomOffset, test : Cardinal;
begin
   baseOffset:=(SizeOf(TLGBManager)+15) and $FFFFFFF0;

   margin:=(blockSize-baseOffset-dataSize);
   if margin>$2000 then margin:=$2000; // 8 kB max
   test:=0;
   repeat
      randomOffset:=test;
      test:=(test shl 1)+16;
   until test>margin;
   randomOffset:=randomOffset and MMRandom;

   Result:=Pointer(Cardinal(P)+baseOffset+randomOffset);
end;

// AllocateLGB
//
function AllocateLGB(Size : Cardinal) : Pointer;
var
   blkSize : Cardinal;
   lgbManager : PLGBManager;
begin
   blkSize:=ComputeLGBBlockSize(Size);

   // Spawn manager, allocate block
   lgbManager:=RMMVirtualAlloc(blkSize);
   if lgbManager=nil then
      Result:=nil
   else begin
      lgbManager.hFile:=0;
      lgbManager.DataSize:=Size;
      lgbManager.BlockSize:=blkSize;
      lgbManager.DataStart:=ComputeLGBDataStart(lgbManager, blkSize, Size);
      lgbManager.MaxDataSize:=blkSize-(Cardinal(lgbManager.DataStart)-Cardinal(lgbManager));

      // Add in the LGB linked list
      CSLockEnter(vLGBLock);

      if vLGBManagers<>nil then
         vLGBManagers.Prev:=lgbManager;
      lgbManager.Next:=vLGBManagers;
      lgbManager.Prev:=nil;
      vLGBManagers:=lgbManager;

      CSLockLeave(vLGBLock);

      UpdateMemoryMap(lgbManager, lgbManager.BlockSize, Pointer(Cardinal(lgbManager)+1));

      Result:=lgbManager.DataStart;
   end;
end;

// ReleaseLGB
//
procedure ReleaseLGB(aManager : PLGBManager);
var
   manager : TLGBManager; // local copy
begin
   UpdateMemoryMap(aManager, aManager.BlockSize, nil);
   manager:=aManager^;

   // Remove from LGB linked list
   CSLockEnter(vLGBLock);

   if manager.Prev=nil then
      vLGBManagers:=manager.Next
   else manager.Prev.Next:=manager.Next;
   if manager.Next<>nil then
      manager.Next.Prev:=manager.Prev;

   CSLockLeave(vLGBLock);
   
   // Free block
   {$ifdef ALLOW_MEMORYMAPPED_LARGE_BLOCKS}
   if aManager.hFile<>0 then begin
      UnmapViewOfFile(aManager);
      CloseHandle(manager.hMapping);
      CloseHandle(manager.hFile);
   end else RMMVirtualFree(aManager, manager.BlockSize);
   {$else}
   RMMVirtualFree(aManager, manager.BlockSize);
   {$endif}
end;

// ReallocateLGB
//
function ReallocateLGB(oldManager : PLGBManager; newSize : Cardinal) : PLGBManager;
var
   blkSize, oldDataOffset, copySize : Cardinal;
   newManager : PLGBManager;
   hFile, hMapping : Cardinal;
   needDataTransfer : Boolean;
begin
   if (newSize>oldManager.DataSize) and (newSize<cReallocUpSizingLGBLimit) then
      newSize:=Round(newSize*cReallocUpSizing);
   blkSize:=ComputeLGBBlockSize(newSize);

   hFile:=oldManager.hFile;
   hMapping:=oldManager.hMapping;
   newManager:=nil;
   needDataTransfer:=True;

   CSLockEnter(vLGBLock);

   {$ifdef ALLOW_MEMORYMAPPED_LARGE_BLOCKS}
   if hFile<>0 then begin
      oldDataOffset:=Cardinal(oldManager.DataStart)-Cardinal(oldManager);
      UpdateMemoryMap(oldManager, oldManager.BlockSize, nil);
      UnmapViewOfFile(oldManager);
      CloseHandle(hMapping);
      newManager:=SetTemporaryFileSizeAndMap(hFile, blkSize, hMapping);
      if newManager<>nil then begin
         newManager.DataStart:=Pointer(Cardinal(newManager)+oldDataOffset);
         needDataTransfer:=False;
      end;
   end;
   {$endif}
   if hFile=0 then begin
      {$ifdef ALLOW_MEMORYMAPPED_LARGE_BLOCKS}
      if newSize>cMemoryMappedLargeBlocksMinSize then begin
         // promote to memory-mapped
         if CreateTemporaryFile(hFile) then begin
            newManager:=SetTemporaryFileSizeAndMap(hFile, blkSize, hMapping);
            if newManager=nil then begin
               CloseHandle(hFile);
               hFile:=0;
            end;
         end else hFile:=0;
      end;
      {$endif}
      if newManager=nil then
         newManager:=RMMVirtualAlloc(blkSize);
      newManager.Next:=oldManager.Next;
      newManager.Prev:=oldManager.Prev;
   end;

   if newManager<>nil then begin
      if newManager.Prev<>nil then
         newManager.Prev.Next:=newManager
      else vLGBManagers:=newManager;
      if newManager.Next<>nil then
         newManager.Next.Prev:=newManager;
   end;

   CSLockLeave(vLGBLock);

   if Assigned(newManager) then begin

      if needDataTransfer then begin
         newManager.DataStart:=ComputeLGBDataStart(newManager, blkSize, newSize);
         copySize:=oldManager.DataSize;
         if copySize>newSize then
            copySize:=newSize;
         Move16(oldManager.DataStart^, newManager.DataStart^, copySize);
         UpdateMemoryMap(oldManager, oldManager.BlockSize, nil);
         RMMVirtualFree(oldManager, oldManager.BlockSize);
      end;

      newManager.hFile:=hFile;
      newManager.hMapping:=hMapping;
      newManager.BlockSize:=blkSize;
      newManager.DataSize:=newSize;
      newManager.MaxDataSize:=blkSize-(Cardinal(newManager.DataStart)-Cardinal(newManager));

      UpdateMemoryMap(newManager, blkSize, Pointer(Cardinal(newManager)+1));

      Result:=newManager;

   end else Result:=nil;
end;

// Move16SSE
//
procedure Move16SSE(const Source; var Dest; Count: Integer); register;
// eax : Source
// edx : Dest      
// ecx : Count
asm
   or       ecx, ecx
   jz       @@End

@@Copy:
   add      ecx, 15 // round up ecx (Count) to 16
   and      cl, $F0

   lea      eax, [eax+ecx]
   lea      edx, [edx+ecx]

   neg      ecx

   test     ecx, 16
   jz       @@Batch32

   db $0F,$6F,$24,$08         /// movq     mm4, [eax+ecx]
   db $0F,$6F,$6C,$08,$08     /// movq     mm5, [eax+ecx+8]
   db $0F,$7F,$24,$0A         /// movq     [edx+ecx], mm4
   db $0F,$7F,$6C,$0A,$08     /// movq     [edx+ecx+8], mm5
   
   add      ecx, 16
   jnz      @@Batch32
   emms
   ret

@@Batch32:
   cmp      ecx, -2*1024    // beyond 2 kb, use uncached transfer
   jl       @@HugeLoop 

@@Loop:
   db $0F,$6F,$04,$08         /// movq     mm0, [eax+ecx]
   db $0F,$6F,$4C,$08,$08     /// movq     mm1, [eax+ecx+8]
   db $0F,$6F,$54,$08,$10     /// movq     mm2, [eax+ecx+16]
   db $0F,$6F,$5C,$08,$18     /// movq     mm3, [eax+ecx+24]
   db $0F,$7F,$04,$0A         /// movq     [edx+ecx], mm0
   db $0F,$7F,$4C,$0A,$08     /// movq     [edx+ecx+8], mm1
   db $0F,$7F,$54,$0A,$10     /// movq     [edx+ecx+16], mm2
   db $0F,$7F,$5C,$0A,$18     /// movq     [edx+ecx+24], mm3
   
   add      ecx, 32
   jnz      @@Loop
   emms
   ret

@@HugeLoop:
   db $0F,$6F,$04,$08         /// movq     mm0, [eax+ecx]
   db $0F,$6F,$4C,$08,$08     /// movq     mm1, [eax+ecx+8]
   db $0F,$6F,$54,$08,$10     /// movq     mm2, [eax+ecx+16]
   db $0F,$6F,$5C,$08,$18     /// movq     mm3, [eax+ecx+24]
   db $0F,$E7,$04,$0A         /// movntq   [edx+ecx], mm0
   db $0F,$E7,$4C,$0A,$08     /// movntq   [edx+ecx+8], mm1
   db $0F,$E7,$54,$0A,$10     /// movntq   [edx+ecx+16], mm2
   db $0F,$E7,$5C,$0A,$18     /// movntq   [edx+ecx+24], mm3

   add      ecx, 32
   jnz      @@HugeLoop
   emms

@@End:
end;

// MemClear16SSE
//
procedure MemClear16SSE(var Buffer; Count: Integer); register;
asm
   or       edx, edx
   jz       @@End

@@Copy:
   // round to 16
   add      edx, 15
   and      dl, $F0

   lea      eax, [eax+edx]
   db $0F,$EF,$C0           /// pxor     mm0, mm0

   neg      edx

   test     edx, 16
   jz       @@Loop

   db $0F,$E7,$04,$10       /// movntq   [eax+edx], mm0
   db $0F,$E7,$44,$10,$08   /// movntq   [eax+edx+8], mm0
   add      edx, 16
   jz       @@End

@@Loop:
   db $0F,$E7,$04,$10       /// movntq   [eax+edx], mm0
   db $0F,$E7,$44,$10,$08   /// movntq   [eax+edx+8], mm0
   db $0F,$E7,$44,$10,$10   /// movntq   [eax+edx+16], mm0
   db $0F,$E7,$44,$10,$18   /// movntq   [eax+edx+24], mm0
   add      edx, 32
   jnz      @@Loop

@@End:
   db $0F,$77               /// emms
end;

// MemClear16RTL
//
procedure MemClear16RTL(var Buffer; Count: Integer);
begin
   FillChar(Buffer, Count, 0);
end;

// SMBLinkedListInsertFirst
//
procedure SMBLinkedListInsertFirst(var linkedList : TSMBLinkedList; item : PSMBManager);
begin
   if linkedList.First=nil then begin
      linkedList.First:=item;
      linkedList.Last:=item;
      item.Next:=nil;
   end else begin
      item.Next:=linkedList.First;
      linkedList.First.Prev:=item;
      linkedList.First:=item;
   end;
   item.Prev:=nil;
end;

// SMBLinkedListInsertLast
//
procedure SMBLinkedListInsertLast(var linkedList : TSMBLinkedList; item : PSMBManager);
begin
   if linkedList.First=nil then begin
      linkedList.First:=item;
      linkedList.Last:=item;
      item.Prev:=nil;
   end else begin
      item.Prev:=linkedList.Last;
      linkedList.Last.Next:=item;
      linkedList.Last:=item;
   end;
   item.Next:=nil;
end;

// SMBLinkedListCut
//
procedure SMBLinkedListCut(var linkedList : TSMBLinkedList; item : PSMBManager);
begin
   if item.Prev=nil then
      linkedList.First:=item.Next
   else item.Prev.Next:=item.Next;
   if item.Next=nil then
      linkedList.Last:=item.Prev
   else item.Next.Prev:=item.Prev;
end;

// AllocateSMB
//
function AllocateSMB(smbInfo : PSMBInfo) : PSMBManager;
var
   n : Cardinal;
begin
   // Determine ChunkSize
   n:=smbInfo.BlocksPerSMB;

   Result:=RMMAllocChunkItem;
   if Result=nil then Exit;

   Result.MaxNbFreeBlocks:=n;
   Result.NbFreeBlocks:=n;
   Result.SMBInfo:=smbInfo;
   Result.BlockSize:=smbInfo.Size;
   Result.DownSizingSize:=smbInfo.DownSizingSize;

   // prepare block offsets stack
   Result.NextNonAllocatedBlkID:=Result.BlockStart;
   Result.FirstFreedBlock:=nil;

   SMBLinkedListInsertFirst(smbInfo.FreeSMBs, Result);
end;

// ReleaseSMB
//
procedure ReleaseSMB(manager : PSMBManager);
begin
   if manager.NbFreeBlocks>0 then
      SMBLinkedListCut(manager.SMBInfo.FreeSMBs, manager)
   else SMBLinkedListCut(manager.SMBInfo.FullSMBs, manager);

   RMMVirtualFreeChunkItem(manager.BlockStart);
end;

// RGetMem
//
function RGetMem(Size: Integer): Pointer;

   procedure MoveManagerToFull(manager : PSMBManager);
   begin
      SMBLinkedListCut(manager.SMBInfo.FreeSMBs, manager);
      SMBLinkedListInsertFirst(manager.SMBInfo.FullSMBs, manager);
   end;

   function AllocFromNewManager(smbInfo : PSMBInfo) : Pointer;
   var
      manager : PSMBManager;
   begin
      manager:=AllocateSMB(smbInfo);
      if manager=nil then begin
         Result:=nil;
      end else begin
         Dec(manager.NbFreeBlocks);
         Result:=manager.NextNonAllocatedBlkID;
         Inc(Cardinal(manager.NextNonAllocatedBlkID), manager.BlockSize);
      end;
      CSLockLeave(smbInfo.CSLock);
   end;

var
   manager : PSMBManager;
   smbInfo : PSMBInfo;
begin
   if Size>cSMBMaxSize then begin
      // Large blocks
      Result:=AllocateLGB(Size);
   end else begin
      // Small Blocks logic
      smbInfo:=@vSMBs[vSMBSizeToPSMBInfo[(Size-1) shr 4]];

      CSLockEnter(smbInfo.CSLock);

      manager:=smbInfo.FreeSMBs.First;
      if manager=nil then begin
         // allocate block from new manager
         Result:=AllocFromNewManager(smbInfo);
      end else begin
         // allocate from existing manager
         if manager.NbFreeBlocks=1 then
            MoveManagerToFull(manager);
         Dec(manager.NbFreeBlocks);
         Result:=manager.FirstFreedBlock;
         if Result<>nil then
            manager.FirstFreedBlock:=PPointer(Result)^
         else begin
            Result:=manager.NextNonAllocatedBlkID;
            Inc(Cardinal(manager.NextNonAllocatedBlkID), manager.BlockSize);
         end;
         CSLockLeave(manager.SMBInfo.CSLock);
      end;

      {$ifdef RAISE_EXCEPTION_ON_INVALID_RELEASE}
      PDouble(Result)^:=0;
      {$endif}
   end;
end;

// FreeSMBBlockAndLeaveLock
//
procedure FreeSMBBlockAndLeaveLock(manager : PSMBManager; p : Pointer);

   procedure MoveToNonFullAndLeaveLock(manager : PSMBManager);
   var
      smbInfo : PSMBInfo;
   begin
      smbInfo:=manager.SMBInfo;
      SMBLinkedListCut(smbInfo.FullSMBs, manager);
      SMBLinkedListInsertFirst(smbInfo.FreeSMBs, manager);
      CSLockLeave(smbInfo.CSLock);
   end;

var
   n : Cardinal;
   smbInfo : PSMBInfo;
begin
   PPointer(P)^:=manager.FirstFreedBlock;
   manager.FirstFreedBlock:=P;

   {$ifdef RAISE_EXCEPTION_ON_INVALID_RELEASE}
   PCardinalArray(P)[1]:=cBAADFOOD;
   {$endif}

   n:=manager.NbFreeBlocks;
   Inc(manager.NbFreeBlocks);
   if n=0 then begin
      MoveToNonFullAndLeaveLock(manager);
   end else if (n+1=manager.MaxNbFreeBlocks) then begin
      smbInfo:=manager.SMBInfo;
      ReleaseSMB(manager);
      CSLockLeave(smbInfo.CSLock);
   end else CSLockLeave(manager.SMBInfo.CSLock);
end;

// SMBManagerFromChunkAndPointer
//
function SMBManagerFromChunkAndPointer(chunk : POSChunk; P : Cardinal) : PSMBManager;
var
   i : Cardinal;
begin
   // identify chunk item in the chunk, this will be our manager
   i:=(P-chunk.FirstBlock) div cOSChunkItemSize;
   Result:=@chunk.Manager[i];
   {$ifdef RAISE_EXCEPTION_ON_INVALID_RELEASE}
   if Result.BlockStart=nil then
      Result:=nil;
   {$endif}
end;

// RFreeMem
//
function RFreeMem(P : Pointer) : Integer;
var
   manager : PSMBManager;
   lgm : PLGBManager;
begin
   lgm:=vMemoryMap[Cardinal(P) shr 16];

   if (Cardinal(lgm) and 1)=0 then begin
      // Small block release logic, this is a chunk
      manager:=SMBManagerFromChunkAndPointer(POSChunk(lgm), Cardinal(P));
      if manager<>nil then begin
         {$ifdef RAISE_EXCEPTION_ON_INVALID_RELEASE}
         if PCardinalArray(P)[1]=cBAADFOOD then begin
            Result:=-1;
            Exit;
         end;
         {$endif}

         CSLockEnter(manager.SMBInfo.CSLock);
         FreeSMBBlockAndLeaveLock(manager, P);
      end else begin
         // not found = invalid free
         Result:=-1;
         Exit;
      end;
   end else begin
      lgm:=PLGBManager(Cardinal(lgm) and $FFFFFFF0);
      if (lgm<>nil) and (P=lgm.DataStart) then begin
         // Large block
         ReleaseLGB(lgm);
      end else begin
         Result:=-1;
         Exit;
      end;
   end;

   Result:=0;
end;

// ReallocTransferSMB
//
function ReallocTransferSMB(P: Pointer; Size : Cardinal; manager : PSMBManager) : Pointer;
var
   copySize : Cardinal;
begin
   if Size<cReallocUpSizingLimit then
      Result:=RGetMem(cReallocUpSizingSMBPad+((Size*cReallocUpSizing256) shr 8))
   else Result:=RGetMem(Size);
   if Result<>nil then begin
      copySize:=manager.BlockSize;
      if copySize>Size then copySize:=Size;
      Move16(P^, Result^, copySize);
      CSLockEnter(manager.SMBInfo.CSLock);
      FreeSMBBlockAndLeaveLock(manager, P);
   end;
end;

// ReallocTransferLGB
//
function ReallocTransferLGB(p : Pointer; size : Cardinal; lgm : PLGBManager) : Pointer;
begin
   if Size>cSMBMaxSize then begin
      // LGB to LGB
      lgm:=ReallocateLGB(lgm, size);
      if lgm<>nil then
         Result:=lgm.DataStart
      else result:=nil;
   end else begin
      // transition from LGB to SMB
      Result:=RGetMem(size);
      if Result<>nil then begin
         if size>lgm.DataSize then
            size:=lgm.DataSize;
         Move16(p^, Result^, size);
         ReleaseLGB(lgm);
      end;
   end;
end;

// RReallocMem
//
function RReallocMem(P : Pointer; Size : Cardinal) : Pointer;
var
   manager : PSMBManager;
   lgm : PLGBManager;
begin
   lgm:=vMemoryMap[Cardinal(P) shr 16];

   if (Cardinal(lgm) and 1)=0 then begin
      // Reallocating a SMB
      {$ifdef RAISE_EXCEPTION_ON_INVALID_RELEASE}
      if PCardinalArray(P)[1]=cBAADFOOD then begin
         Result:=nil;
         Exit;
      end;
      {$endif}

      manager:=SMBManagerFromChunkAndPointer(POSChunk(lgm), Cardinal(P));
      if manager<>nil then begin
         if (Size<=manager.BlockSize) and (Size>=manager.DownSizingSize) then
            Result:=P
         else Result:=ReallocTransferSMB(P, Size, manager);
      end else begin
         Result:=nil;
      end;
   end else begin
      lgm:=PLGBManager(Cardinal(lgm) and $FFFFFFF0);
      if (lgm<>nil) and (P=lgm.DataStart) then begin
         // Reallocating a LGB
         if (Size<=lgm.MaxDataSize) and (Size>=lgm.MaxDataSize div cLGBReallocDownSizing) then begin
            lgm.DataSize:=Size;
            Result:=P;
         end else Result:=ReallocTransferLGB(P, Size, lgm);
      end else begin
         Result:=nil;
      end;
   end;
end;

// RAllocMem
//
function RAllocMem(Size : Cardinal) : Pointer; register;
asm
   push  ebx
   cmp   eax, 0
   jg    @@Alloc
   xor   eax, eax
   jmp   @@End
@@Alloc:
   mov   ebx, eax
   call  RGetMem        // Result:=RGetMem(Size);
   cmp   ebx, 64*1024   // Blocks larger than 64kB are automatically initialized to zero
   jg    @@End
   mov   edx, ebx
   mov   ebx, eax
   call  [MemClear16]   // MemClear16(Result^, Size);
   mov   eax, ebx
@@End:
   pop   ebx
end;

// RAllocated
//
function RAllocated(const P : Pointer) : Boolean;
{var
   blkID : Cardinal;
   manager : PSMBManager;
   locP : Pointer;}
begin
{   locP:=P;
   if locP=nil then }
      Result:=False
{   else begin
      manager:=vMemoryMap[Cardinal(locP) shr 16];
      if Assigned(manager) then begin
         if manager.SMBInfo.CSLock<>nil then begin
            blkID:=Cardinal(P)-Cardinal(manager.BlockStart);
            Result:=(blkID mod manager.blockSize=0);
         end else Result:=(PLGBManager(manager).DataStart=locP);
      end else Result:=False;
   end;                         }
end;

// InitializeRMM
//
procedure InitializeRMM;
var
   i, j, k : Integer;
   smbInfo : PSMBInfo;
begin
   InitializeCSLock(vLGBLock);

   for i:=Low(vSMBs) to High(vSMBs) do begin
      smbInfo:=@vSMBs[i];
      InitializeCSLock(smbInfo.CSLock);
      smbInfo.Size:=cSMBSizes[i];
      smbInfo.BlocksPerSMB:=cOSChunkItemSize div smbInfo.Size;
      if smbInfo.Size>cReallocMinSize then
         smbInfo.DownSizingSize:=smbInfo.Size div cSMBReallocDownSizing
      else smbInfo.DownSizingSize:=0;
   end;

   j:=0;
   for i:=0 to cSMBMaxSizeIndex do begin
      k:=cSMBSizes[i];
      while j<k do begin
         vSMBSizeToPSMBInfo[j shr 4]:=i;
         Inc(j, 16);
      end;
   end;

   InitializeCSLock(vOSChunksLock);
end;

// FinalizeRMM
//
procedure FinalizeRMM;
var
   i : Integer;
   chunk : POSChunk;
begin
   // release LGBs
   DeleteCSLock(vLGBLock);
   while vLGBManagers<>nil do
      ReleaseLGB(vLGBManagers);
   // release SMBs
   for i:=Low(vSMBs) to High(vSMBs) do begin
      while vSMBs[i].FreeSMBs.First<>nil do
         ReleaseSMB(vSMBs[i].FreeSMBs.First);
      while vSMBs[i].FullSMBs.First<>nil do
         ReleaseSMB(vSMBs[i].FullSMBs.First);
      DeleteCSLock(vSMBs[i].CSLock);
   end;

   // release OS chunks
   if vOSChunksFirstFull<>nil then
      RunError(153); // "CRC Error in Data"
   if vOSChunksFirst<>nil then begin
      while vOSChunksFirst<>nil do begin
         chunk:=vOSChunksFirst;
         CutOutChunk(chunk);
         DestroyChunk(chunk);
      end;
   end;

   DeleteCSLock(vOSChunksLock);
end;

// LockRMM
//
procedure LockRMM;
var
   i : Integer;
begin
   CSLockEnter(vLGBLock);
   for i:=Low(vSMBs) to High(vSMBs) do
      CSLockEnter(vSMBs[i].CSLock);
end;

// UnLockRMM
//
procedure UnLockRMM;
var
   i : Integer;
begin
   for i:=High(vSMBs) downto Low(vSMBs) do
      CSLockLeave(vSMBs[i].CSLock);
   CSLockLeave(vLGBLock);
end;

//
// Cleanup thread
//
{$ifdef ALLOW_DELAYED_RELEASE}
function CleanupThreadProc(parameter : Pointer) : Integer; stdcall;
var
   chunk, chunkNext : POSChunk;
   cleanupChunkChain : POSChunk;
begin
   repeat
      cleanupChunkChain:=nil;

      // Clean up one empty chunk
      if vOSChunkNbEntirelyFree>1 then begin
         CSLockEnter(vOSChunksLock);
         chunk:=vOSChunksFirst;
         if chunk<>nil then chunk:=chunk.Next;
         while chunk<>nil do begin
            chunkNext:=chunk.Next;
            if (chunk.FreeBlocks=cOSChunkBlockCount) then begin
               CutOutChunk(chunk);
               chunk.Next:=cleanupChunkChain;
               cleanupChunkChain:=chunk;
               Dec(vOSChunkNbEntirelyFree);
               if vOSChunkNbEntirelyFree<cOSDelayedAllowedChunksLatency then Break;
            end;
            chunk:=chunkNext;
         end;
         CSLockLeave(vOSChunksLock);
      end;

      // Destroy them out of the lock
      while cleanupChunkChain<>nil do begin
         chunk:=cleanupChunkChain;
         cleanupChunkChain:=chunk.Next;
         DestroyChunk(chunk);
      end;

      WaitForSingleObject(vCleanupThreadEvent, 250);
   until vCleanupThreadID=0;
   Result:=0;
end;

procedure StartCleanupThread;
begin
   if (vCleanupThreadID=0) and (not IsLibrary) then begin
      vCleanupThreadEvent:=CreateEvent(nil, False, False, nil);
      vCleanupThreadHnd:=CreateThread(nil, 16*1024, @CleanupThreadProc, nil,
                                      0, vCleanupThreadID);
//      SetThreadPriority(vCleanupThreadHnd, THREAD_PRIORITY_ABOVE_NORMAL);
   end;
end;

procedure StopCleanupThread;
begin
   if vCleanupThreadID<>0 then begin
      vCleanupThreadID:=0;
      SetEvent(vCleanupThreadEvent);
      WaitForSingleObject(vCleanupThreadHnd, INFINITE);

      CloseHandle(vCleanupThreadHnd);
      CloseHandle(vCleanupThreadEvent);
   end;
end;
{$endif}

// BindRMM
//
procedure BindRMM;

   {$ifdef SHARE_MEM}
   procedure PrepareDataName;
   const
      cIntToHex : ShortString = '0123456789ABCDEF';
   var
      i : Integer;
      h : Cardinal;
   begin
      // name generation must NOT use any dynamic stuff (for obvious reasons)
      h:=GetCurrentProcessID;
      for i:=0 to 7 do
         vSharedMemory_DataName[i+1]:=cIntToHex[1+((h shr (i*4)) and $F)];
   end;
   {$endif}

var
   smm : PSharedMemoryManager;
   hwnd : Integer;
begin
   Inc(vRMMBound);
   if vRMMBound=1 then begin
      {$ifdef SHARE_MEM}
      PrepareDataName;
      hwnd:=FindWindow('STATIC', PChar(@vSharedMemory_DataName[1]));
      {$else}
      hwnd:=0;
      {$endif}
      smm:=@vSharedMemoryManager;
      if hwnd=0 then begin
         // defined SharedMemoryManager fields
         smm.MemoryManager.GetMem:=@RGetMem;
         smm.MemoryManager.FreeMem:=@RFreeMem;
         smm.MemoryManager.ReallocMem:=@RReallocMem;
         smm.Allocated:=@Allocated;
         {$ifdef ALLOW_USAGE_SNAPSHOT}
         smm.RMMUsageSnapShot:=@RMMUsageSnapShot;
         {$endif}
         // Setup structure data for shared memory
         {$ifdef SHARE_MEM}
         vSharedMemory_Data:=CreateWindow('STATIC',
                                          PChar(@vSharedMemory_DataName[1]),
                                          WS_POPUP,
                                          0, 0, 0, 0,
                                          0, 0, GetCurrentProcessID, nil);
         SetWindowLong(vSharedMemory_Data, GWL_USERDATA,
                       LongWord(@vSharedMemoryManager));
         vSharedMemory_InUse:=False;
         {$endif}
         InitializeRMM;
         {$ifdef ALLOW_DELAYED_RELEASE}
         StartCleanupThread;
         {$endif}
      end else begin
         {$ifdef SHARE_MEM}
         // we're in a DLL and a RMM has been setup by the application
         smm:=PSharedMemoryManager(GetWindowLong(hwnd, GWL_USERDATA));
         vSharedMemory_InUse:=True;
         {$endif}
      end;
      // replace standard MemoryManager
      GetMemoryManager(vOldMemoryManager);
      SetMemoryManager(smm.MemoryManager);
      // Redirect SysUtils's AllocMem
      Allocated:=@smm.Allocated;
      {$ifdef ALLOW_USAGE_SNAPSHOT}
      vRMMUsageSnapShotPatch:=RedirectPatch(@RMMUsageSnapShot, @smm.RMMUsageSnapShot);
      {$endif}
   end;
end;

// UnBindRMM
//
procedure UnBindRMM;
begin
   Dec(vRMMBound);
   if vRMMBound=0 then begin
      SetMemoryManager(vOldMemoryManager);
      {$ifdef SHARE_MEM}
         if not vSharedMemory_InUse then
            DestroyWindow(vSharedMemory_Data);
      {$endif}
      FinalizeRMM;
   end else if vRMMBound<0 then
      RunError(210); // Object not initialized
end;

// RMMActive
//
function RMMActive : Boolean;
begin
   Result:=(vRMMBound>0);
end;

{$ifdef ALLOW_USAGE_SNAPSHOT}
// RMMUsageSnapShot (func)
//
function RMMUsageSnapShot : TRMMUsageSnapShot;
begin
   RMMUsageSnapShot(Result);
end;

// RMMUsageSnapShot (proc)
//
procedure RMMUsageSnapShot(var result : TRMMUsageSnapShot); overload;

   // computes userSize and nbBlocks contribution for overlapping situations
   procedure AddOverlapStat(start, blockStart, blockEnd, blockSize : Cardinal;
                            var userSize, nbBlocks : Cardinal);
   var
      startEnd : Cardinal;
   begin
      if blockSize=0 then Exit;
      startEnd:=start+(1 shl 16);
      if (blockStart>=startEnd) or (blockEnd<=start) then Exit;
      if (blockStart>=start) and (blockStart<startEnd) then
         Inc(nbBlocks);
      blockEnd:=blockStart+blockSize;
      if blockEnd<=start then Exit;
      if blockEnd<=startEnd then
         if blockStart>=start then
            Inc(userSize, blockSize)
         else Inc(userSize, blockEnd-start)
      else if blockStart>=start then
         Inc(userSize, startEnd-blockStart)
      else Inc(userSize, 1 shl 16);
   end;

var
   i, j, k, kp : Cardinal;
   userSize, nbBlocks, blkSize, totalUserSize : Cardinal;
   psmb : PSMBManager;
   plgb : PLGBManager;
   mapEntry : PRMMMemoryMap;
   mbi : TMemoryBasicInformation;
begin
   Assert(vRMMBound>0);
   // we're not allowed to use any kind of dynamic allocation here
   LockRMM;
   try
      Result.BenchRGetMem:=vBenchRGetMem;
      Result.BenchRReallocMem:=vBenchRReallocMem;
      Result.BenchRFreeMem:=vBenchRFreeMem; 

      Result.NbMapItems:=vMemMapUpper+1;
      Result.TotalVirtualAllocated:=vTotalVirtualAllocated;
      nbBlocks:=0;
      totalUserSize:=0;
      // Build the memory map
      // first go through the memory map
      for i:=0 to vMemMapUpper do begin
         mapEntry:=@Result.Map[i];
         mapEntry.StartAddr:=Pointer(i shl 16);
         mapEntry.Length:=1 shl 16;
         psmb:=vMemoryMap[i];
         if psmb=nil then begin
            // 64kb area not allocated by RMM (but maybe reserved as chunkbatch)
            mapEntry.AllocatedUserSize:=0;
            mapEntry.Status:=rmmsUnallocated;
         end else if psmb.Signature=cSMBSignature then begin
            // 64kb area used by an SMB
            userSize:=0;
            for k:=0 to psmb.MaxNbFreeBlocks-1 do begin
               blkSize:=psmb.BlockSize;
               if blkSize>0 then begin
                  Inc(userSize, blkSize);
                  Inc(nbBlocks);
               end;
            end;
            Inc(totalUserSize, userSize);
            mapEntry.AllocatedUserSize:=userSize;
            if userSize>0 then
               mapEntry.Status:=rmmsAllocated
            else mapEntry.Status:=rmmsReserved;
         end else if psmb.Signature=cLGBSignature then begin
            // 64kb area used by an LGB
            plgb:=PLGBManager(psmb);
            k:=(i shl 16)-Cardinal(plgb.BlockStart);
            if k=0 then begin
               Inc(totalUserSize, plgb.DataSize);
               Inc(nbBlocks);
            end;
            if k<plgb.DataSize then
               mapEntry.AllocatedUserSize:=1 shl 16
            else if k+(1 shl 16)<plgb.DataSize then
               mapEntry.AllocatedUserSize:=plgb.DataSize-k
            else mapEntry.AllocatedUserSize:=0;
            mapEntry.Status:=rmmsAllocated;
         end;
      end;
      Result.AllocatedBlocks:=nbBlocks;
      Result.AllocatedUserSize:=totalUserSize;
      // Collect VM space stats
      Result.TotalVMSpace:=(vMemMapUpper+1) shl 16;
      Result.SystemAllocatedVM:=0;
      Result.SystemReservedVM:=0;
      k:=0; kp:=0;
      // Make a pass through the unallocated chunks and ask about their status
      for i:=0 to vMemMapUpper  do begin
         mapEntry:=@Result.Map[i];
         if mapEntry.Status=rmmsUnallocated then begin
            VirtualQuery(Pointer(i shl 16), mbi, SizeOf(mbi));
            if mbi.State=MEM_COMMIT	then begin
               mapEntry.Status:=rmmsSysAllocated;
               Inc(Result.SystemAllocatedVM, 1 shl 16);
            end else if mbi.State=MEM_RESERVE then begin
               mapEntry.Status:=rmmsSysReserved;
               Inc(Result.SystemReservedVM, 1 shl 16);
            end;
         end;
         if mapEntry.Status<>rmmsUnallocated then begin
            if i-k>kp then kp:=i-k;
            k:=i+1;
         end;
      end;
      if vMemMapUpper+1 - k > kp then kp:=vMemMapUpper - k;
      Result.LargestFreeVM:=kp shl 16;
      // Build SMBStats
      for i:=Low(vSMBs) to High(vSMBs) do begin
         nbBlocks:=0;
         userSize:=0;
         k:=0;
         psmb:=vSMBs[i].First;
         while Assigned(psmb) do begin
            Inc(nbBlocks, psmb.MaxNbFreeBlocks-psmb.NbFreeBlocks);
            for j:=0 to psmb.MaxNbFreeBlocks-1 do
               Inc(userSize, psmb.BlockSize);
            Inc(k, cSMBChunkSize);
            psmb:=psmb.Next;
         end;
         with Result.SMBStats[i] do begin
            BlockSize:=SMBIndexToSize(i);
            AllocatedBlocks:=nbBlocks;
            AllocatedUserSize:=userSize;
            TotalVirtualAllocated:=k;
         end;
      end;
   finally
      UnLockRMM;
   end;
end;
{$endif} // ALLOW_USAGE_SNAPSHOT

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
var
  vMemStatus : TMEMORYSTATUS;

initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   if IsMemoryManagerSet then
      RunError(208); // Overlay manager not installed

   // detect if in 3GB mode
   vMemStatus.dwLength:=32;
   GlobalMemoryStatus(vMemStatus);
   vRunningIn3GBMode:=(vMemStatus.dwTotalVirtual>$80000000);
   if vRunningIn3GBMode then
      vMemMapUpper:=cMemMapUpper3GB
   else vMemMapUpper:=cMemMapUpper2GB;
   vVirtualLimit:=vMemStatus.dwTotalVirtual;

   // SwitchToThread available on NT only, so dynamic linking required
   InitializeSwitchToThread;

   {$ifdef ALLOW_SSE}
   try
      // detect SSE capable CPU
      asm
         push   ebx
         pushfd
         pop    eax
         mov    edx, eax
         xor    edx, $200000
         push   eax
         popfd
         pushfd
         pop    eax
         cmp    eax, edx
         jz     @@Exit           // CPUID not supported
         mov    eax, 0
         db $0F,$A2              /// cpuid
         jz     @@Exit           // features not supported
         mov    eax, 1
         db $0F,$A2              /// cpuid
         test   edx, (1 shl 25)  // SSE support?
         setnz  al
         mov    byte ptr [vSSESupported], al
      @@Exit:
         pop     ebx
      end;
   except
      // trap for old/exotics CPUs
      vSSESupported:=0;
   end;
   if vSSESupported<>0 then begin
      MemClear16:=@MemClear16SSE;
      Move16:=@Move16SSE;
   end else begin
      MemClear16:=@MemClear16RTL;
      Move16:=@System.Move;
   end;
   {$else}
   MemClear16:=@MemClear16FPU;
   Move16:=@Move16FPU;
   {$endif}

   {$ifdef ALLOW_MEMORYMAPPED_LARGE_BLOCKS}
   GetTempPath(cMAX_PATH, @vTemporaryFilesPath[0]);
   {$endif}

   {$ifdef AUTO_BIND}
   BindRMM;
   {$endif}

finalization

   {$ifdef ALLOW_DELAYED_RELEASE}
   StopCleanupThread;
   {$endif}
   {$ifdef AUTO_BIND}
   UnBindRMM;
   {$endif}

end.
