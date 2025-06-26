// File: src/extensions/vesa_extension.c
#include <stdint.h>
#include <stddef.h>
#include "base_kernel.h"

// VBE Info Block structure (from VESA BIOS Extensions Core Functions Standard)
typedef struct {
    char vbe_signature[4];       // 'VESA' signature
    uint16_t vbe_version;         // VBE version (e.g., 0x0200 for 2.0)
    uint32_t oem_string_ptr;      // Pointer to OEM string
    uint8_t capabilities[4];      // Capabilities of the controller
    uint32_t video_mode_ptr;      // Pointer to video mode list
    uint16_t total_memory;        // Total memory in 64KB blocks
    // VBE 2.0+ fields
    uint16_t oem_software_rev;
    uint32_t oem_vendor_name_ptr;
    uint32_t oem_product_name_ptr;
    uint32_t oem_product_rev_ptr;
    uint8_t reserved[222];        // Reserved for VBE 2.0+
    uint8_t oem_data[256];        // OEM data string (often 2.0+)
} __attribute__((packed)) VbeInfoBlock;

// VBE Mode Info Block structure
typedef struct {
    uint16_t mode_attributes;     // Mode attributes
    uint8_t win_a_attributes;     // Window A attributes
    uint8_t win_b_attributes;     // Window B attributes
    uint16_t win_granularity;     // Window granularity in KBytes
    uint16_t win_size;            // Window size in KBytes
    uint16_t win_a_segment;       // Window A start segment
    uint16_t win_b_segment;       // Window B start segment
    uint32_t real_font_ptr;       // Pointer to real mode font
    uint16_t bytes_per_scan_line; // Bytes per scan line (pitch)
    uint16_t x_resolution;        // Horizontal resolution
    uint16_t y_resolution;        // Vertical resolution
    uint8_t char_set_w;           // Character cell width
    uint8_t char_set_h;           // Character cell height
    uint8_t num_planes;           // Number of memory planes
    uint8_t bits_per_pixel;       // Bits per pixel
    uint8_t num_banks;            // Number of banks
    uint8_t memory_model;         // Memory model type
    uint8_t bank_size;            // Bank size in KBytes
    uint8_t num_image_pages;      // Number of image pages
    uint8_t reserved0;            // Reserved
    // LFB fields
    uint8_t red_mask_size;
    uint8_t red_field_position;
    uint8_t green_mask_size;
    uint8_t green_field_position;
    uint8_t blue_mask_size;
    uint8_t blue_field_position;
    uint8_t reserved_mask_size;
    uint8_t reserved_field_position;
    uint8_t direct_color_mode_info; // Direct Color Mode Info
    uint32_t phys_base_ptr;       // Physical address of the linear frame buffer
    uint32_t reserved1;           // Reserved (VBE 2.0+)
    uint16_t reserved2;           // Reserved (VBE 2.0+)
    uint16_t linear_bytes_per_scan_line; // Bytes per scan line (linear)
    uint8_t banked_num_image_pages;
    uint8_t linear_num_image_pages;
    uint8_t linear_red_mask_size;
    uint8_t linear_red_field_position;
    uint8_t linear_green_mask_size;
    uint8_t linear_green_field_position;
    uint8_t linear_blue_mask_size;
    uint8_t linear_blue_field_position;
    uint8_t linear_reserved_mask_size;
    uint8_t linear_reserved_field_position;
    uint32_t max_pixel_clock;
    uint8_t reserved3[189];       // Reserved for VBE 3.0+
} __attribute__((packed)) VbeModeInfoBlock;

// --- Globals for this Extension ---
static int vesa_ext_id = -1;
static VbeInfoBlock vbe_info;
static VbeModeInfoBlock current_vbe_mode_info;
static uint16_t current_vesa_mode = 0;
static uint32_t frame_buffer_address = 0;
static uint16_t screen_width = 0;
static uint16_t screen_height = 0;
static uint8_t screen_bpp = 0;
static uint32_t pitch = 0;

// --- VESA Helper Functions ---

// Get VESA Controller Information (AX = 0x4F00)
static int vbe_get_controller_info(VbeInfoBlock* info) {
    regs16_t in_regs, out_regs;
    in_regs.eax = 0x4F00;
    in_regs.edi = (uint32_t)info; // ES:DI points to buffer (in C, flat model means just address)
    in_regs.es = 0x00; // ES must be 0 for real mode calls to point to 0:address

    call_bios_int(0x10, &in_regs, &out_regs);

    if (out_regs.eax == 0x4F) { // AH = 0x4F means VBE function supported
        return 0; // Success
    }
    return -1; // Failure
}

// Get VESA Mode Information (AX = 0x4F01)
static int vbe_get_mode_info(uint16_t mode, VbeModeInfoBlock* mode_info) {
    regs16_t in_regs, out_regs;
    in_regs.eax = 0x4F01;
    in_regs.ecx = mode;
    in_regs.edi = (uint32_t)mode_info; // ES:DI points to buffer
    in_regs.es = 0x00;

    call_bios_int(0x10, &in_regs, &out_regs);

    if (out_regs.eax == 0x4F) {
        return 0; // Success
    }
    return -1; // Failure
}

// Set VESA Mode (AX = 0x4F02)
static int vbe_set_mode(uint16_t mode) {
    regs16_t in_regs, out_regs;
    in_regs.eax = 0x4F02;
    in_regs.ebx = mode | 0x4000; // Set Linear Frame Buffer bit (0x4000)
                                 // Add 0x4000 to mode number to activate LFB
                                 // (If supported by VBE and mode)

    call_bios_int(0x10, &in_regs, &out_regs);

    if (out_regs.eax == 0x4F) {
        return 0; // Success
    }
    return -1; // Failure
}

// Find a suitable LFB mode (e.g., 1024x768x32bpp)
static uint16_t vbe_find_lfb_mode(uint16_t width, uint16_t height, uint8_t bpp) {
    uint16_t* mode_list_ptr = (uint16_t*)((vbe_info.video_mode_ptr & 0xFFFF) + ((vbe_info.video_mode_ptr >> 16) << 4)); // Convert segment:offset to linear
    if (mode_list_ptr == 0) return 0; // Should not happen if info block is valid

    for (int i = 0; mode_list_ptr[i] != 0xFFFF; i++) {
        uint16_t mode = mode_list_ptr[i];
        VbeModeInfoBlock temp_mode_info;
        if (vbe_get_mode_info(mode, &temp_mode_info) == 0) {
            // Check for graphics mode, LFB support, and desired resolution/bpp
            if ((temp_mode_info.mode_attributes & 0x90) == 0x90 && // Graphics mode & Linear Frame Buffer supported
                temp_mode_info.x_resolution == width &&
                temp_mode_info.y_resolution == height &&
                temp_mode_info.bits_per_pixel == bpp) {
                return mode;
            }
        }
    }
    return 0; // Mode not found
}


// --- VESA Command Handlers ---

void cmd_vesa_info(const char* args) {
    terminal_writestring("VESA: Controller Info:\n");
    char hex_buf[20];

    if (vbe_info.vbe_signature[0] == 'V' && vbe_info.vbe_signature[1] == 'E' &&
        vbe_info.vbe_signature[2] == 'S' && vbe_info.vbe_signature[3] == 'A') {
        terminal_writestring("  Signature: '");
        terminal_putchar(vbe_info.vbe_signature[0]);
        terminal_putchar(vbe_info.vbe_signature[1]);
        terminal_putchar(vbe_info.vbe_signature[2]);
        terminal_putchar(vbe_info.vbe_signature[3]);
        terminal_writestring("'\n");

        terminal_writestring("  VBE Version: 0x");
        uint16_to_hex_str(vbe_info.vbe_version, hex_buf); terminal_writestring(hex_buf);
        terminal_writestring("\n");

        terminal_writestring("  Total Memory: ");
        char num_buf[10];
        int i = 0; uint16_t temp_mem = vbe_info.total_memory * 64; // In KB
        if (temp_mem == 0) { num_buf[0] = '0'; i = 1; } else { uint16_t tmp = temp_mem; while (tmp > 0) { num_buf[i++] = (tmp % 10) + '0'; tmp /= 10; } } num_buf[i] = '\0'; for (int start = 0, end = i - 1; start < end; start++, end--) { char t = num_buf[start]; num_buf[start] = num_buf[end]; num_buf[end] = t; }
        terminal_writestring(num_buf); terminal_writestring("KB\n");

        terminal_writestring("  Capabilities: 0x");
        uint32_to_hex_str(*(uint32_t*)vbe_info.capabilities, hex_buf); terminal_writestring(hex_buf);
        terminal_writestring("\n");

        if (current_vesa_mode != 0) {
            terminal_writestring("VESA: Current Mode Info (0x");
            uint16_to_hex_str(current_vesa_mode, hex_buf); terminal_writestring(hex_buf);
            terminal_writestring("):\n");
            terminal_writestring("  Resolution: ");
            i = 0; uint16_t temp_res = screen_width; if (temp_res == 0) { num_buf[0] = '0'; i = 1; } else { uint16_t tmp = temp_res; while (tmp > 0) { num_buf[i++] = (tmp % 10) + '0'; tmp /= 10; } } num_buf[i] = '\0'; for (int start = 0, end = i - 1; start < end; start++, end--) { char t = num_buf[start]; num_buf[start] = num_buf[end]; num_buf[end] = t; }
            terminal_writestring(num_buf); terminal_writestring("x");
            i = 0; temp_res = screen_height; if (temp_res == 0) { num_buf[0] = '0'; i = 1; } else { uint16_t tmp = temp_res; while (tmp > 0) { num_buf[i++] = (tmp % 10) + '0'; tmp /= 10; } } num_buf[i] = '\0'; for (int start = 0, end = i - 1; start < end; start++, end--) { char t = num_buf[start]; num_buf[start] = num_buf[end]; num_buf[end] = t; }
            terminal_writestring(num_buf);
            terminal_writestring(", BPP: ");
            i = 0; uint8_t temp_bpp = screen_bpp; if (temp_bpp == 0) { num_buf[0] = '0'; i = 1; } else { uint8_t tmp = temp_bpp; while (tmp > 0) { num_buf[i++] = (tmp % 10) + '0'; tmp /= 10; } } num_buf[i] = '\0'; for (int start = 0, end = i - 1; start < end; start++, end--) { char t = num_buf[start]; num_buf[start] = num_buf[end]; num_buf[end] = t; }
            terminal_writestring(num_buf); terminal_writestring("\n");

            terminal_writestring("  Linear Frame Buffer: 0x");
            uint32_to_hex_str(frame_buffer_address, hex_buf); terminal_writestring(hex_buf);
            terminal_writestring("\n  Pitch (Bytes/ScanLine): ");
            i = 0; uint32_t temp_pitch = pitch; if (temp_pitch == 0) { num_buf[0] = '0'; i = 1; } else { uint32_t tmp = temp_pitch; while (tmp > 0) { num_buf[i++] = (tmp % 10) + '0'; tmp /= 10; } } num_buf[i] = '\0'; for (int start = 0, end = i - 1; start < end; start++, end--) { char t = num_buf[start]; num_str[start] = num_str[end]; num_str[end] = t; }
            terminal_writestring(num_buf); terminal_writestring("\n");

        } else {
            terminal_writestring("VESA: No VESA graphical mode currently active.\n");
        }

    } else {
        terminal_writestring("VESA: VBE Signature not found or VESA not supported.\n");
    }
}

void cmd_vesa_set_mode(const char* args) {
    if (strlen(args) < 3) { // Requires at least "0x" and one digit
        terminal_writestring("Usage: vesa_set_mode <mode_id_hex>\n");
        return;
    }

    uint16_t mode_id = 0;
    // Simple hex string to uint16_t conversion
    const char* p = args;
    if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
        p += 2;
    }
    while (*p) {
        uint8_t digit;
        if (*p >= '0' && *p <= '9') digit = *p - '0';
        else if (*p >= 'a' && *p <= 'f') digit = *p - 'a' + 10;
        else if (*p >= 'A' && *p <= 'F') digit = *p - 'A' + 10;
        else { terminal_writestring("Invalid hex character in mode ID.\n"); return; }
        mode_id = (mode_id << 4) | digit;
        p++;
    }

    if (mode_id == 0) {
        terminal_writestring("Invalid VESA mode ID provided.\n");
        return;
    }

    terminal_writestring("VESA: Attempting to set mode 0x");
    char hex_buf[5];
    uint16_to_hex_str(mode_id, hex_buf); terminal_writestring(hex_buf);
    terminal_writestring("...\n");

    if (vbe_set_mode(mode_id) == 0) {
        terminal_writestring("VESA: Mode set successfully.\n");
        // Update current mode info
        if (vbe_get_mode_info(mode_id, &current_vbe_mode_info) == 0) {
            current_vesa_mode = mode_id;
            screen_width = current_vbe_mode_info.x_resolution;
            screen_height = current_vbe_mode_info.y_resolution;
            screen_bpp = current_vbe_mode_info.bits_per_pixel;
            frame_buffer_address = current_vbe_mode_info.phys_base_ptr;
            pitch = current_vbe_mode_info.linear_bytes_per_scan_line;
            terminal_writestring("VESA: Updated current mode info.\n");
        } else {
            terminal_writestring("VESA: Failed to retrieve info for newly set mode.\n");
        }
    } else {
        terminal_writestring("VESA: Failed to set VESA mode. Mode might not be supported.\n");
    }
}


// --- VESA Extension Initialization ---
int vesa_extension_init(void) {
    terminal_writestring("VESA: Initializing VESA extension...\n");

    // 1. Get VBE Controller Information
    // Ensure the VBEInfoBlock is placed in a memory location accessible by BIOS (e.g., lower 1MB)
    // For simplicity, we're passing its direct address. Your kernel's memory map allows this.
    if (vbe_get_controller_info(&vbe_info) != 0) {
        terminal_writestring("VESA: Failed to get VBE controller info. VESA not supported or invalid memory access.\n");
        return 1; // Failure
    }

    // Check VESA signature
    if (!(vbe_info.vbe_signature[0] == 'V' && vbe_info.vbe_signature[1] == 'E' &&
          vbe_info.vbe_signature[2] == 'S' && vbe_info.vbe_signature[3] == 'A')) {
        terminal_writestring("VESA: Invalid VBE signature. VESA support not confirmed.\n");
        return 1; // Failure
    }

    terminal_writestring("VESA: VBE Controller found. Version: 0x");
    char hex_buf[10]; uint16_to_hex_str(vbe_info.vbe_version, hex_buf); terminal_writestring(hex_buf); terminal_writestring("\n");

    // Optional: Try to set a common LFB mode (e.g., 1024x768x32bpp) by default
    uint16_t default_mode = vbe_find_lfb_mode(1024, 768, 32);
    if (default_mode != 0) {
        terminal_writestring("VESA: Found default mode 1024x768x32bpp (0x");
        uint16_to_hex_str(default_mode, hex_buf); terminal_writestring(hex_buf); terminal_writestring(")\n");
        // We won't set it automatically here to avoid changing screen immediately.
        // User can use `vesa_set_mode 0x<id>`
    } else {
        terminal_writestring("VESA: Default 1024x768x32bpp LFB mode not found.\n");
    }


    terminal_writestring("VESA: Extension initialized. Use 'vesa_info' and 'vesa_set_mode'.\n");

    register_command("vesa_info", cmd_vesa_info, "Display VESA graphics info", vesa_ext_id);
    register_command("vesa_set_mode", cmd_vesa_set_mode, "Set VESA graphical mode (e.g., 0x105)", vesa_ext_id);

    return 0; // Success
}

// --- VESA Extension Cleanup ---
void vesa_extension_cleanup(void) {
    terminal_writestring("VESA: Cleaning up...\n");
    // Optionally reset to text mode here
    // regs16_t in_regs = {.eax = 0x0003}, out_regs; // VGA 80x25 text mode
    // call_bios_int(0x10, &in_regs, &out_regs);
    terminal_writestring("VESA: Cleanup complete.\n");
}

// --- Automatic Registration Function ---
__attribute__((section(".ext_register_fns")))
void __vesa_auto_register(void) {
    vesa_ext_id = register_extension("VESA_GFX", "1.0",
                                     vesa_extension_init,
                                     vesa_extension_cleanup);
    if (vesa_ext_id >= 0) {
        load_extension(vesa_ext_id);
    } else {
        terminal_writestring("Failed to register VESA Extension (auto)!\n");
    }
}
```asm
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

    ; Get input/output register pointers from stack
    mov bp, sp
    ; The stack frame changes from 32-bit to 16-bit here.
    ; Need to account for the pushes that occurred in 32-bit mode
    ; pusha (8*4 = 32 bytes)
    ; 5 segment registers (5*2 = 10 bytes)
    ; sgdt (6 bytes)
    ; cr0 (4 bytes)
    ; Total 52 bytes pushed before the C arguments
    ; The args are 4 bytes each
    ; int_no at (original_ESP + 52)
    ; in_regs_ptr at (original_ESP + 56)
    ; out_regs_ptr at (original_ESP + 60)

    ; However, `pusha` in 32-bit mode pushes 32-bit registers.
    ; `push ds` etc pushes 16-bit segment registers.
    ; The `sgdt` and `push eax` (for CR0) are 32-bit pushes.
    ; This needs careful stack management.

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
    ; `push ds` etc pushed 5 words = 10 bytes.
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
