; File: src/boot/bios_thunk.asm
; This assembly file provides a real-mode thunk for calling BIOS interrupts
; from protected mode.

section .text
bits 32 ; We start in 32-bit protected mode

extern _terminal_writestring ; For basic debugging in protected mode if needed

; Global GDT and GDT pointer definitions for the thunk
; These are temporary GDT entries used only during the thunking process
; They assume a flat 4GB memory model for both code and data segments
global _bios_thunk_gdt_ptr
global _bios_thunk_gdt

; A GDT with entries for a NULL descriptor, a 32-bit code segment,
; a 32-bit data segment, and a 16-bit code segment (for real mode calls)
; and a 16-bit data segment.
; This GDT covers the entire 4GB memory space.
_bios_thunk_gdt:
    ; Null Descriptor
    dd 0x00000000
    dd 0x00000000

    ; 32-bit Code Segment (for returning to Protected Mode C)
    ; Base = 0x0, Limit = 0xFFFFF, G = 1 (4KB granularity)
    ; P=1, DPL=0, S=1 (Code/Data), Type=1011 (Executable, readable)
    db 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x9B, 0xCF, 0x00 ; G=1, D=1 (32-bit), P=1, DPL=0, Type=Executable, Readable

    ; 32-bit Data Segment (for returning to Protected Mode C)
    ; Base = 0x0, Limit = 0xFFFFF, G = 1 (4KB granularity)
    ; P=1, DPL=0, S=1 (Code/Data), Type=0011 (Read/Write)
    db 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x93, 0xCF, 0x00 ; G=1, D=1 (32-bit), P=1, DPL=0, Type=Writable

    ; 16-bit Code Segment (for real mode BIOS call)
    ; Base = 0x0, Limit = 0xFFFF (64KB), G = 0 (byte granularity)
    ; P=1, DPL=0, S=1, Type=1011
    ; Access byte (0x9A): P=1, DPL=00, S=1, Type=1010b (Code, execute-only)
    ; Flags (0x00): G=0, D=0 (16-bit), AVL=0, Limit_high=0
    db 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x9A, 0x00, 0x00 ; P=1, DPL=0, S=1, Type=Code (Execute-only), 16-bit operand/address, G=0

    ; 16-bit Data Segment (for real mode BIOS call)
    ; Base = 0x0, Limit = 0xFFFF (64KB), G = 0 (byte granularity)
    ; P=1, DPL=0, S=1, Type=0011 (Read/Write)
    ; Access byte (0x92): P=1, DPL=00, S=1, Type=0010b (Data, read/write)
    ; Flags (0x00): G=0, D=0 (16-bit), AVL=0, Limit_high=0
    db 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x92, 0x00, 0x00 ; P=1, DPL=0, S=1, Type=Data (Read/Write), 16-bit operand/address, G=0

_bios_thunk_gdt_end:

; GDT pointer for the thunk's GDT
_bios_thunk_gdt_ptr:
    dw _bios_thunk_gdt_end - _bios_thunk_gdt - 1 ; Limit
    dd _bios_thunk_gdt                   ; Base address

; GDT segment selectors for the thunk's GDT
CODE32_SEG  equ _bios_thunk_gdt_end - _bios_thunk_gdt - 8 - 8 - 8
DATA32_SEG  equ _bios_thunk_gdt_end - _bios_thunk_gdt - 8 - 8
CODE16_SEG  equ _bios_thunk_gdt_end - _bios_thunk_gdt - 8
DATA16_SEG  equ _bios_thunk_gdt_end - _bios_thunk_gdt

; This is the entry point from C.
; void call_bios_int(uint8_t int_no, regs16_t* in_regs, regs16_t* out_regs);
; Arguments are pushed right-to-left onto the stack:
; [esp+8] = out_regs (pointer to regs16_t for output)
; [esp+4] = in_regs (pointer to regs16_t for input)
; [esp+0] = int_no (BIOS interrupt number)
global call_bios_int
call_bios_int:
    ; Save current protected mode context
    pusha           ; Save EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI
    push ds
    push es
    push fs
    push gs
    push ss

    ; Store current GDT pointer and CR0 state
    sgdt [esp - 6] ; Store GDT pointer (48 bits) 6 bytes below ESP
    mov eax, cr0
    push eax       ; Save CR0

    ; Disable interrupts (important for mode switch)
    cli

    ; Load temporary GDT for real-mode thunk
    lgdt [_bios_thunk_gdt_ptr]

    ; Clear Protected Mode Enable (PE) bit in CR0
    ; This puts the CPU into a "real mode" operation state, but within
    ; the 32-bit flat address space. This is "unreal mode" or "big real mode".
    ; A specific far jump is needed to actually flush the pipeline and
    ; reload segment registers in their real-mode compatible form.
    mov eax, cr0
    and eax, not 1 ; Clear PE bit
    mov cr0, eax

    ; Far jump to 16-bit code segment at offset `_thunk_entry`
    ; This flushes the prefetch queue and forces CPU into 16-bit mode.
    jmp DATA16_SEG:flush_pipeline

bits 16 ; From here, we're in 16-bit mode

flush_pipeline:
    ; Set up 16-bit segment registers
    mov ax, DATA16_SEG
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax ; Stack segment should also be set correctly

    ; Copy input registers from protected mode structure to real mode registers
    ; in_regs is at [esp+8+32] because of saved registers above (8*4 for pusha + 5*2 for segment regs)
    ; This assumes a flat memory model (offset is direct address)
    ; Stack layout:
    ; [esp+32+2] (after pusha, 5 segs, cr0, sgdt) = int_no
    ; [esp+32+6] = in_regs_ptr (protected mode address)
    ; [esp+32+10] = out_regs_ptr (protected mode address)

    ; Let's re-evaluate the stack relative to current SP (which is BP)
    ; Stack at _thunk_entry:
    ; [BP+52] is int_no (uint8_t) -- but pushed as 4 bytes by C
    ; [BP+56] is in_regs_ptr (uint32_t)
    ; [BP+60] is out_regs_ptr (uint32_t)
    ; The easiest way is to push the args and read them here

    ; Let's simplify the stack for thunk
    ; We push only what's needed for the thunk itself.
    ; The C stack frame is irrelevant for the real mode call.

    ; Let's try again with a simpler approach.
    ; The C function signature: `void call_bios_int(uint8_t int_no, regs16_t* in_regs, regs16_t* out_regs);`
    ; In 32-bit protected mode, arguments are on stack:
    ; [ESP+12] -> out_regs_ptr
    ; [ESP+8]  -> in_regs_ptr
    ; [ESP+4]  -> int_no

    ; We need to access these pointers in 16-bit mode.
    ; The stack before `pusha` was where C left it.
    ; `pusha` pushed 8 dwords = 32 bytes.
    ; `push ds` etc pushes 5 words = 10 bytes.
    ; `sgdt` pushes 6 bytes.
    ; `push eax` (CR0) pushes 4 bytes.
    ; Total 52 bytes.

    ; So, int_no is at [ESP + 52 + 4] = [ESP + 56] relative to current ESP *after* these pushes.
    ; in_regs_ptr at [ESP + 56 + 4] = [ESP + 60]
    ; out_regs_ptr at [ESP + 60 + 4] = [ESP + 64]

    ; Let's use EBP for stack frame
    mov ebp, esp ; Save current ESP (32-bit)

    ; Load input registers into real mode registers
    mov eax, [ebp + 60] ; in_regs_ptr
    mov bx, [eax + 0]   ; in_regs->eax (low 16-bit)
    mov ax, [eax + 2]   ; in_regs->eax (high 16-bit) ; EAX for BIOS call
    mov cx, [eax + 4]   ; in_regs->ecx
    mov dx, [eax + 8]   ; in_regs->edx
    mov si, [eax + 16]  ; in_regs->esi
    mov di, [eax + 20]  ; in_regs->edi
    mov es, [eax + 24]  ; in_regs->es
    mov ds, [eax + 26]  ; in_regs->ds
    mov fs, [eax + 28]  ; in_regs->fs
    mov gs, [eax + 30]  ; in_regs->gs
    mov ss, [eax + 32]  ; in_regs->ss
    ; EBP, ESP are for stack itself, not usually passed

    ; Execute the BIOS interrupt
    mov cl, byte [ebp + 56] ; int_no
    int cl                  ; Call the BIOS interrupt

    ; Store output registers back to protected mode structure
    mov eax, [ebp + 64] ; out_regs_ptr
    mov [eax + 0], bx   ; out_regs->eax (low 16-bit)
    mov [eax + 2], ax   ; out_regs->eax (high 16-bit) ; EAX from BIOS call
    mov [eax + 4], cx   ; out_regs->ecx
    mov [eax + 8], dx   ; out_regs->edx
    mov [eax + 16], si  ; out_regs->esi
    mov [eax + 20], di  ; out_regs->edi
    mov [eax + 24], es  ; out_regs->es
    mov [eax + 26], ds  ; out_regs->ds
    mov [eax + 28], fs  ; out_regs->fs
    mov [eax + 30], gs  ; out_regs->gs
    mov [eax + 32], ss  ; out_regs->ss
    mov [eax + 34], eflags ; Save EFLAGS (pushed by INT instruction, then retrieved)

    ; Transition back to 32-bit protected mode
    ; Reload CR0.PE bit
    ; This needs a far jump to reload segments correctly

    mov eax, cr0
    or eax, 1         ; Set PE bit
    mov cr0, eax

    ; Far jump back to 32-bit code segment
    jmp CODE32_SEG:back_to_protected_mode

bits 32 ; Back to 32-bit mode

back_to_protected_mode:
    ; Restore protected mode context
    pop eax         ; Restore original CR0 value
    mov cr0, eax
    lgdt [esp + 4] ; Restore original GDT pointer from stack (stored below saved CR0)

    pop ss
    pop gs
    pop fs
    pop es
    pop ds
    popa            ; Restore original general purpose registers

    ; Cleanup C stack frame
    add esp, 12     ; Remove int_no, in_regs_ptr, out_regs_ptr from stack

    sti             ; Re-enable interrupts
    ret
