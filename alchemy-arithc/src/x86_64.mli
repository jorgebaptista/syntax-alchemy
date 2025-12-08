
(** {0 Library for writing programs X86-64 }

    This is only a relatively small fragment
    of the X86-64 assembler.

    @author Jean-Christophe Filliâtre (CNRS)
    @author Kim Nguyen (Université Paris Sud)
*)

(** {1 Code } *)

type 'a asm
  (** abstract type of assembly code.
      The parameter ['a] is used as a phantom type. *)	
type text = [ `text ] asm
  (** assembly code located in the text section *)

type data = [ `data ] asm
  (** assembly code located in the data section *)

type label = string
  (** address labels are strings *)
  
val nop : [> ] asm
  (** the empty instruction. Can appear in either text or data *)

val ( ++ ) : ([< `text|`data ] asm as 'a)-> 'a -> 'a
   (** concatenates two code chunks (either text with text, or data with data) *)

val inline: string -> [> ] asm
  (** [inline s] copies the string [s] as is into the assembly file *)

type program = {
  text : text;
  data : data;
}
  (** a program consists of a text section and a data section *)

val print_program : Format.formatter -> program -> unit
  (** [print_program fmt p] imprime le code du programme [p] dans le
      formatter [fmt] *)

val print_in_file: file:string -> program -> unit

(** {1 Registers } *)

type size = [`B | `W | `L | `Q]

type 'size register
  (** abstract type for registers *)

val rax: [`Q] register
val rbx: [`Q] register
val rcx: [`Q] register
val rdx: [`Q] register
val rsi: [`Q] register
val rdi: [`Q] register
val rbp: [`Q] register
val rsp: [`Q] register
val r8 : [`Q] register
val r9 : [`Q] register
val r10: [`Q] register
val r11: [`Q] register
val r12: [`Q] register
val r13: [`Q] register
val r14: [`Q] register
val r15: [`Q] register
  (**  64 bits registers *)

val eax: [`L] register
val ebx: [`L] register
val ecx: [`L] register
val edx: [`L] register
val esi: [`L] register
val edi: [`L] register
val ebp: [`L] register
val esp: [`L] register
val r8d: [`L] register
val r9d: [`L] register
val r10d: [`L] register
val r11d: [`L] register
val r12d: [`L] register
val r13d: [`L] register
val r14d: [`L] register
val r15d: [`L] register
  (** 32 bits registers*)

val ax: [`W] register
val bx: [`W] register
val cx: [`W] register
val dx: [`W] register
val si: [`W] register
val di: [`W] register
val bp: [`W] register
val sp: [`W] register
val r8w: [`W] register
val r9w: [`W] register
val r10w: [`W] register
val r11w: [`W] register
val r12w: [`W] register
val r13w: [`W] register
val r14w: [`W] register
val r15w: [`W] register
  (** 16 bits registers *)

val al: [`B] register
val bl: [`B] register
val cl: [`B] register
val dl: [`B] register
val ah: [`B] register
val bh: [`B] register
val ch: [`B] register
val dh: [`B] register
val sil: [`B] register
val dil: [`B] register
val bpl: [`B] register
val spl: [`B] register
val r8b: [`B] register
val r9b: [`B] register
val r10b: [`B] register
val r11b: [`B] register
val r12b: [`B] register
val r13b: [`B] register
val r14b: [`B] register
val r15b: [`B] register
  (** 8 bits registers *)

(** {1 Operands } *)

type 'size operand
  (** abstract type for operands *)

val imm: int -> [>] operand
  (** imediat operande $i *)
val imm32: int32 -> [>] operand
  (** immediat operand $i *)
val imm64: int64 -> [>] operand
  (** immediate operand $i *)
val reg: 'size register -> 'size operand
val (!%): 'size register -> 'size operand
  (** register *)
val ind: ?ofs:int -> ?index:'size1 register -> ?scale:int ->
  'size2 register -> [>] operand
  (** indirect operande ofs(register, index, scale) *)
val lab: label -> [>] operand
  (** label L  *)
val ilab: label -> [`Q] operand
  (** immediat label $L *)

(** {1 Instructions } *)

(** {2 Transfert } *)

val movb: [`B] operand -> [`B] operand -> text
val movw: [`W] operand -> [`W] operand -> text
val movl: [`L] operand -> [`L] operand -> text
val movq: [`Q] operand -> [`Q] operand -> text
  (** Caution: not all the possible combinations are allowed *)
val movsbw: [`B] operand -> [`W] register -> text
val movsbl: [`B] operand -> [`L] register -> text
val movsbq: [`B] operand -> [`Q] register -> text
val movswl: [`W] operand -> [`L] register -> text
val movswq: [`W] operand -> [`Q] register -> text
val movslq: [`L] operand -> [`Q] register -> text
  (** 8->64 bits, with sign extension *)
val movzbw: [`B] operand -> [`W] register -> text
val movzbl: [`B] operand -> [`L] register -> text
val movzbq: [`B] operand -> [`Q] register -> text
val movzwl: [`W] operand -> [`L] register -> text
val movzwq: [`W] operand -> [`Q] register -> text
  (** 8->64 bits, with zero extension *)

val movabsq: [`Q] operand -> [`Q] register -> text
  (** copy one 64 bits immediate value to a register *)

(** {2 Arithmetics } *)

val leab: [`B] operand -> [`B] register -> text
val leaw: [`W] operand -> [`W] register -> text
val leal: [`L] operand -> [`L] register -> text
val leaq: [`Q] operand -> [`Q] register -> text

val incb: [`B] operand -> text
val incw: [`W] operand -> text
val incl: [`L] operand -> text
val incq: [`Q] operand -> text

val decb: [`B] operand -> text
val decw: [`W] operand -> text
val decl: [`L] operand -> text
val decq: [`Q] operand -> text

val negb: [`B] operand -> text
val negw: [`W] operand -> text
val negl: [`L] operand -> text
val negq: [`Q] operand -> text

val addb: [`B] operand -> [`B] operand -> text
val addw: [`W] operand -> [`W] operand -> text
val addl: [`L] operand -> [`L] operand -> text
val addq: [`Q] operand -> [`Q] operand -> text

val subb: [`B] operand -> [`B] operand -> text
val subw: [`W] operand -> [`W] operand -> text
val subl: [`L] operand -> [`L] operand -> text
val subq: [`Q] operand -> [`Q] operand -> text

val imulw: [`W] operand -> [`W] operand -> text
val imull: [`L] operand -> [`L] operand -> text
val imulq: [`Q] operand -> [`Q] operand -> text

val idivq: [`Q] operand -> text
val cqto: text

(** {2 Logical operations } *)

val notb: [`B] operand -> text
val notw: [`W] operand -> text
val notl: [`L] operand -> text
val notq: [`Q] operand -> text

val andb: [`B] operand -> [`B] operand -> text
val andw: [`W] operand -> [`W] operand -> text
val andl: [`L] operand -> [`L] operand -> text
val andq: [`Q] operand -> [`Q] operand -> text

val orb : [`B] operand -> [`B] operand -> text
val orw : [`W] operand -> [`W] operand -> text
val orl : [`L] operand -> [`L] operand -> text
val orq : [`Q] operand -> [`Q] operand -> text

val xorb: [`B] operand -> [`B] operand -> text
val xorw: [`W] operand -> [`W] operand -> text
val xorl: [`L] operand -> [`L] operand -> text
val xorq: [`Q] operand -> [`Q] operand -> text
  (** Bitwise Operators. bitwise "and", "or", "not" *)

(** {2 Shifts } *)

val shlb: [`B] operand -> [`B] operand -> text
val shlw: [`W] operand -> [`W] operand -> text
val shll: [`L] operand -> [`L] operand -> text
val shlq: [`Q] operand -> [`Q] operand -> text
  (** note: shl is the same as sal *)

val shrb: [`B] operand -> [`B] operand -> text
val shrw: [`W] operand -> [`W] operand -> text
val shrl: [`L] operand -> [`L] operand -> text
val shrq: [`Q] operand -> [`Q] operand -> text

val sarb: [`B] operand -> [`B] operand -> text
val sarw: [`W] operand -> [`W] operand -> text
val sarl: [`L] operand -> [`L] operand -> text
val sarq: [`Q] operand -> [`Q] operand -> text

(** {2 Jumps } *)

val call: label -> text
val call_star: [`Q] operand -> text
val leave: text
val ret: text
  (** function call and return *)

val jmp : label -> text
  (** unconditional jump *)

val jmp_star: [`Q] operand -> text
  (** jump to a computed address *)

val je : label -> text  (* =  0 *)
val jz : label -> text  (* =  0 *)
val jne: label -> text  (* <> 0 *)
val jnz: label -> text  (* <> 0 *)
val js : label -> text  (* <  0 *)
val jns: label -> text  (* >= 0 *)
val jg : label -> text  (* >  signed *)
val jge: label -> text  (* >= signed *)
val jl : label -> text  (* <  signed *)
val jle: label -> text  (* <= signed *)
val ja : label -> text  (* >  unsigned *)
val jae: label -> text  (* >= unsigned *)
val jb : label -> text  (* <  unsigned *)
val jbe: label -> text  (* <= unsigned *)
  (** conditional jumps *)

(** {2 Conditions } *)

val cmpb: [`B] operand -> [`B] operand -> text
val cmpw: [`W] operand -> [`W] operand -> text
val cmpl: [`L] operand -> [`L] operand -> text
val cmpq: [`Q] operand -> [`Q] operand -> text

val testb: [`B] operand -> [`B] operand -> text
val testw: [`W] operand -> [`W] operand -> text
val testl: [`L] operand -> [`L] operand -> text
val testq: [`Q] operand -> [`Q] operand -> text

val sete : [`B] operand -> text  (* =  0 *)
val setne: [`B] operand -> text  (* <> 0 *)
val sets : [`B] operand -> text  (* <  0 *)
val setns: [`B] operand -> text  (* >= 0 *)
val setg : [`B] operand -> text  (* >  signed *)
val setge: [`B] operand -> text  (* >= signed *)
val setl : [`B] operand -> text  (* <  signed *)
val setle: [`B] operand -> text  (* <= signed *)
val seta : [`B] operand -> text  (* >  unsigned *)
val setae: [`B] operand -> text  (* >= unsigned *)
val setb : [`B] operand -> text  (* <  unsigned *)
val setbe: [`B] operand -> text  (* <= unsigned *)
 (** sets the operand byte to 1 if the test is true, otherwise to 0. *)

(** {2 Stack manipulation} *)

val pushq : [`Q] operand -> text
(** [pushq r] places the contents of [r] at the top of the stack.
      Reminder: %rsp points to the address of the last occupied slot. *)


val popq : [`Q] register -> text
  (** [popq r] places the word at the top of the stack into [r] and pops. *)

(** {2 Miscellaneous} *)

val label : label -> [> ] asm
  (** a label. May appear in the text or data section. *)

val globl : label -> [> ] asm
  (** .globl declaration (typically for main) *)

val comment : string -> [> ] asm
  (** inserts a comment in the generated code. May appear in
      the text or data section. *)


(** {2 Données } *)

val string : string -> data
  (** a string constant, ended by 0 *)
val dbyte : int list -> data
val dword : int list -> data
val dint : int list -> data
val dquad : int list -> data
  (** places a list of values using 1/2/4/8 bytes in the data section *)
val address: label list -> data
  (** places a list of addresses in the data section (with .quad) *)
val space: int -> data
(** [space n] allocates [n] bytes (initialized to 0) in the data segment. *)

