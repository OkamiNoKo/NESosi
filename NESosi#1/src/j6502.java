import java.util.Map;
import java.util.function.Function;

public class j6502 {
    public byte a = 0x00;		// Accumulator Register
    public byte x = 0x00;		// X Register
    public byte y = 0x00;		// Y Register
    public byte stkp = 0x00;		// Stack Pointer (points to location on bus)
    public char pc = 0x0000;	// Program Counter
    public byte status = 0x00;		// Status Register

    j6502()
    {
        // Assembles the translation table. It's big, it's ugly, but it yields a convenient way
        // to emulate the 6502. I'm certain there are some "code-golf" strategies to reduce this
        // but I've deliberately kept it verbose for study and alteration

        // It is 16x16 entries. This gives 256 instructions. It is arranged to that the bottom
        // 4 bits of the instruction choose the column, and the top 4 bits choose the row.

        // For convenience to get function pointers to members of this class, I'm using this
        // or else it will be much much larger :D

        // The table is one big initialiser list of initialiser lists...
        this.lookup = new INSTRUCTION[]{new INSTRUCTION( "BRK", opCode.BRK, mode.IMM, (byte)(byte)7),new INSTRUCTION( "ORA", opCode.ORA, mode.IZX, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)3 ),new INSTRUCTION( "ORA", opCode.ORA, mode.ZP0, (byte)3 ),new INSTRUCTION( "ASL", opCode.ASL, mode.ZP0, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)5 ),new INSTRUCTION( "PHP", opCode.PHP, mode.IMP, (byte)3 ),new INSTRUCTION( "ORA", opCode.ORA, mode.IMM, (byte)2 ),new INSTRUCTION( "ASL", opCode.ASL, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "ORA", opCode.ORA, mode.ABS, (byte)4 ),new INSTRUCTION( "ASL", opCode.ASL, mode.ABS, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),
                new INSTRUCTION( "BPL", opCode.BPL, mode.REL, (byte)2 ), new INSTRUCTION( "ORA", opCode.ORA, mode.IZY, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "ORA", opCode.ORA, mode.ZPX, (byte)4 ),new INSTRUCTION( "ASL", opCode.ASL, mode.ZPX, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),new INSTRUCTION( "CLC", opCode.CLC, mode.IMP, (byte)2 ),new INSTRUCTION( "ORA", opCode.ORA, mode.ABY, (byte)4 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "ORA", opCode.ORA, mode.ABX, (byte)4 ),new INSTRUCTION( "ASL", opCode.ASL, mode.ABX, (byte)7 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),
                new INSTRUCTION( "JSR", opCode.JSR, mode.ABS, (byte)6 ), new INSTRUCTION( "AND", opCode.AND, mode.IZX, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "BIT", opCode.BIT, mode.ZP0, (byte)3 ),new INSTRUCTION( "AND", opCode.AND, mode.ZP0, (byte)3 ),new INSTRUCTION( "ROL", opCode.ROL, mode.ZP0, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)5 ),new INSTRUCTION( "PLP", opCode.PLP, mode.IMP, (byte)4 ),new INSTRUCTION( "AND", opCode.AND, mode.IMM, (byte)2 ),new INSTRUCTION( "ROL", opCode.ROL, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "BIT", opCode.BIT, mode.ABS, (byte)4 ),new INSTRUCTION( "AND", opCode.AND, mode.ABS, (byte)4 ),new INSTRUCTION( "ROL", opCode.ROL, mode.ABS, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),
                new INSTRUCTION( "BMI", opCode.BMI, mode.REL, (byte)2 ), new INSTRUCTION( "AND", opCode.AND, mode.IZY, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "AND",opCode. AND, mode.ZPX, (byte)4 ),new INSTRUCTION( "ROL", opCode.ROL, mode.ZPX, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),new INSTRUCTION( "SEC", opCode.SEC, mode.IMP, (byte)2 ),new INSTRUCTION( "AND", opCode.AND, mode.ABY, (byte)4 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "AND", opCode.AND, mode.ABX, (byte)4 ),new INSTRUCTION( "ROL", opCode.ROL, mode.ABX, (byte)7 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),
                new INSTRUCTION( "RTI", opCode.RTI, mode.IMP, (byte)6 ), new INSTRUCTION( "EOR", opCode.EOR, mode.IZX, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)3 ),new INSTRUCTION( "EOR", opCode.EOR, mode.ZP0, (byte)3 ),new INSTRUCTION( "LSR", opCode.LSR, mode.ZP0, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)5 ),new INSTRUCTION( "PHA", opCode.PHA, mode.IMP, (byte)3 ),new INSTRUCTION( "EOR", opCode.EOR, mode.IMM, (byte)2 ),new INSTRUCTION( "LSR", opCode.LSR, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "JMP", opCode.JMP, mode.ABS, (byte)3 ),new INSTRUCTION( "EOR", opCode.EOR, mode.ABS, (byte)4 ),new INSTRUCTION( "LSR", opCode.LSR, mode.ABS, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),
                new INSTRUCTION( "BVC", opCode.BVC, mode.REL, (byte)2 ), new INSTRUCTION( "EOR", opCode.EOR, mode.IZY, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "EOR", opCode.EOR, mode.ZPX, (byte)4 ),new INSTRUCTION( "LSR", opCode.LSR, mode.ZPX, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),new INSTRUCTION( "CLI", opCode.CLI, mode.IMP, (byte)2 ),new INSTRUCTION( "EOR", opCode.EOR, mode.ABY, (byte)4 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "EOR", opCode.EOR, mode.ABX, (byte)4 ),new INSTRUCTION( "LSR", opCode.LSR, mode.ABX, (byte)7 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),
                new INSTRUCTION( "RTS", opCode.RTS, mode.IMP, (byte)6 ), new INSTRUCTION( "ADC", opCode.ADC, mode.IZX, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)3 ),new INSTRUCTION( "ADC", opCode.ADC, mode.ZP0, (byte)3 ),new INSTRUCTION( "ROR", opCode.ROR, mode.ZP0, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)5 ),new INSTRUCTION( "PLA", opCode.PLA, mode.IMP, (byte)4 ),new INSTRUCTION( "ADC", opCode.ADC, mode.IMM, (byte)2 ),new INSTRUCTION( "ROR", opCode.ROR, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "JMP", opCode.JMP, mode.IND, (byte)5 ),new INSTRUCTION( "ADC", opCode.ADC, mode.ABS, (byte)4 ),new INSTRUCTION( "ROR", opCode.ROR, mode.ABS, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),
                new INSTRUCTION( "BVS", opCode.BVS, mode.REL, (byte)2 ), new INSTRUCTION( "ADC", opCode.ADC, mode.IZY, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "ADC", opCode.ADC, mode.ZPX, (byte)4 ),new INSTRUCTION( "ROR", opCode.ROR, mode.ZPX, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),new INSTRUCTION( "SEI", opCode.SEI, mode.IMP, (byte)2 ),new INSTRUCTION( "ADC", opCode.ADC, mode.ABY, (byte)4 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "ADC", opCode.ADC, mode.ABX, (byte)4 ),new INSTRUCTION( "ROR", opCode.ROR, mode.ABX, (byte)7 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),
                new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)2 ), new INSTRUCTION( "STA", opCode.STA, mode.IZX, (byte)6 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),new INSTRUCTION( "STY", opCode.STY, mode.ZP0, (byte)3 ),new INSTRUCTION( "STA", opCode.STA, mode.ZP0, (byte)3 ),new INSTRUCTION( "STX", opCode.STX, mode.ZP0, (byte)3 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)3 ),new INSTRUCTION( "DEY", opCode.DEY, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)2 ),new INSTRUCTION( "TXA", opCode.TXA, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "STY", opCode.STY, mode.ABS, (byte)4 ),new INSTRUCTION( "STA", opCode.STA, mode.ABS, (byte)4 ),new INSTRUCTION( "STX", opCode.STX, mode.ABS, (byte)4 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)4 ),
                new INSTRUCTION( "BCC", opCode.BCC, mode.REL, (byte)2 ), new INSTRUCTION( "STA", opCode.STA, mode.IZY, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),new INSTRUCTION( "STY", opCode.STY, mode.ZPX, (byte)4 ),new INSTRUCTION( "STA", opCode.STA, mode.ZPX, (byte)4 ),new INSTRUCTION( "STX", opCode.STX, mode.ZPY, (byte)4 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)4 ),new INSTRUCTION( "TYA", opCode.TYA, mode.IMP, (byte)2 ),new INSTRUCTION( "STA", opCode.STA, mode.ABY, (byte)5 ),new INSTRUCTION( "TXS", opCode.TXS, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)5 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)5 ),new INSTRUCTION( "STA", opCode.STA, mode.ABX, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)5 ),
                new INSTRUCTION( "LDY", opCode.LDY, mode.IMM, (byte)2 ), new INSTRUCTION( "LDA", opCode.LDA, mode.IZX, (byte)6 ),new INSTRUCTION( "LDX", opCode.LDX, mode.IMM, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),new INSTRUCTION( "LDY", opCode.LDY, mode.ZP0, (byte)3 ),new INSTRUCTION( "LDA", opCode.LDA, mode.ZP0, (byte)3 ),new INSTRUCTION( "LDX", opCode.LDX, mode.ZP0, (byte)3 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)3 ),new INSTRUCTION( "TAY", opCode.TAY, mode.IMP, (byte)2 ),new INSTRUCTION( "LDA", opCode.LDA, mode.IMM, (byte)2 ),new INSTRUCTION( "TAX", opCode.TAX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "LDY", opCode.LDY, mode.ABS, (byte)4 ),new INSTRUCTION( "LDA", opCode.LDA, mode.ABS, (byte)4 ),new INSTRUCTION( "LDX", opCode.LDX, mode.ABS, (byte)4 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)4 ),
                new INSTRUCTION( "BCS", opCode.BCS, mode.REL, (byte)2 ), new INSTRUCTION( "LDA", opCode.LDA, mode.IZY, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)5 ),new INSTRUCTION( "LDY", opCode.LDY, mode.ZPX, (byte)4 ),new INSTRUCTION( "LDA", opCode.LDA, mode.ZPX, (byte)4 ),new INSTRUCTION( "LDX", opCode.LDX, mode.ZPY, (byte)4 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)4 ),new INSTRUCTION( "CLV", opCode.CLV, mode.IMP, (byte)2 ),new INSTRUCTION( "LDA", opCode.LDA, mode.ABY, (byte)4 ),new INSTRUCTION( "TSX", opCode.TSX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)4 ),new INSTRUCTION( "LDY", opCode.LDY, mode.ABX, (byte)4 ),new INSTRUCTION( "LDA", opCode.LDA, mode.ABX, (byte)4 ),new INSTRUCTION( "LDX", opCode.LDX, mode.ABY, (byte)4 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)4 ),
                new INSTRUCTION( "CPY", opCode.CPY, mode.IMM, (byte)2 ), new INSTRUCTION( "CMP", opCode.CMP, mode.IZX, (byte)6 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "CPY", opCode.CPY, mode.ZP0, (byte)3 ),new INSTRUCTION( "CMP", opCode.CMP, mode.ZP0, (byte)3 ),new INSTRUCTION( "DEC", opCode.DEC, mode.ZP0, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)5 ),new INSTRUCTION( "INY", opCode.INY, mode.IMP, (byte)2 ),new INSTRUCTION( "CMP", opCode.CMP, mode.IMM, (byte)2 ),new INSTRUCTION( "DEX", opCode.DEX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "CPY", opCode.CPY, mode.ABS, (byte)4 ),new INSTRUCTION( "CMP", opCode.CMP, mode.ABS, (byte)4 ),new INSTRUCTION( "DEC", opCode.DEC, mode.ABS, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),
                new INSTRUCTION( "BNE", opCode.BNE, mode.REL, (byte)2 ), new INSTRUCTION( "CMP", opCode.CMP, mode.IZY, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "CMP", opCode.CMP, mode.ZPX, (byte)4 ),new INSTRUCTION( "DEC", opCode.DEC, mode.ZPX, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),new INSTRUCTION( "CLD", opCode.CLD, mode.IMP, (byte)2 ),new INSTRUCTION( "CMP", opCode.CMP, mode.ABY, (byte)4 ),new INSTRUCTION( "NOP", opCode.NOP, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "CMP", opCode.CMP, mode.ABX, (byte)4 ),new INSTRUCTION( "DEC", opCode.DEC, mode.ABX, (byte)7 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),
                new INSTRUCTION( "CPX", opCode.CPX, mode.IMM, (byte)2 ), new INSTRUCTION( "SBC", opCode.SBC, mode.IZX, (byte)6 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "CPX", opCode.CPX, mode.ZP0, (byte)3 ),new INSTRUCTION( "SBC", opCode.SBC, mode.ZP0, (byte)3 ),new INSTRUCTION( "INC", opCode.INC, mode.ZP0, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)5 ),new INSTRUCTION( "INX", opCode.INX, mode.IMP, (byte)2 ),new INSTRUCTION( "SBC", opCode.SBC, mode.IMM, (byte)2 ),new INSTRUCTION( "NOP", opCode.NOP, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.SBC, mode.IMP, (byte)2 ),new INSTRUCTION( "CPX", opCode.CPX, mode.ABS, (byte)4 ),new INSTRUCTION( "SBC", opCode.SBC, mode.ABS, (byte)4 ),new INSTRUCTION( "INC", opCode.INC, mode.ABS, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),
                new INSTRUCTION( "BEQ", opCode.BEQ, mode.REL, (byte)2 ), new INSTRUCTION( "SBC", opCode.SBC, mode.IZY, (byte)5 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)8 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "SBC", opCode.SBC, mode.ZPX, (byte)4 ),new INSTRUCTION( "INC", opCode.INC, mode.ZPX, (byte)6 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)6 ),new INSTRUCTION( "SED", opCode.SED, mode.IMP, (byte)2 ),new INSTRUCTION( "SBC", opCode.SBC, mode.ABY, (byte)4 ),new INSTRUCTION( "NOP", opCode.NOP, mode.IMP, (byte)2 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),new INSTRUCTION( "???", opCode.NOP, mode.IMP, (byte)4 ),new INSTRUCTION( "SBC", opCode.SBC, mode.ABX, (byte)4 ),new INSTRUCTION( "INC", opCode.INC, mode.ABX, (byte)7 ),new INSTRUCTION( "???", opCode.XXX, mode.IMP, (byte)7 ),
        };
    }

    // External event functions. In hardware these represent pins that are asserted
    // to produce a change in state.
    public void reset(){
        addr_abs = 0xFFFC;
        char lo = (char) read((char) (addr_abs + 0));
        char hi = (char) read((char) (addr_abs + 1));

        // Set it
        pc = (char)((hi << 8) | lo);

        // Reset internal registers
        a = 0;
        x = 0;
        y = 0;
        stkp = (byte)0xFD;
        status = (byte)((int)0x00 | FLAGS6502.U.getBit_val());

        // Clear internal helper variables
        addr_rel = 0x0000;
        addr_abs = 0x0000;
        fetched = 0x00;

        // Reset takes time
        cycles = 8;
    }	// Reset Interrupt - Forces CPU into known state

    // Interrupt requests are a complex operation and only happen if the
    //"disable interrupt" flag is 0. IRQs can happen at any time, but
    //you dont want them to be destructive to the operation of the running
    //program. Therefore the current instruction is allowed to finish
    //(which I facilitate by doing the whole thing when cycles == 0) and
    //then the current program counter is stored on the stack. Then the
    //current status register is stored on the stack. When the routine
    //that services the interrupt has finished, the status register
    //and program counter can be restored to how they where before it
    //occurred. This is impemented by the "RTI" instruction. Once the IRQ
    //has happened, in a similar way to a reset, a programmable address
    //is read form hard coded location 0xFFFE, which is subsequently
    //set to the program counter.

    public void irq(){
        write((char)(0x0100 + stkp), (byte)((pc >> 8) & 0x00FF));
        stkp--;
        write((char)(0x0100 + stkp), (byte)(pc & 0x00FF));
        stkp--;

        // Then Push the status register to the stack
        SetFlag(FLAGS6502.B, false);
        SetFlag(FLAGS6502.U, true);
        SetFlag(FLAGS6502.I, true);
        write((char)(0x0100 + stkp), status);
        stkp--;

        // Read new program counter location from fixed address
        addr_abs = 0xFFFE;
        char lo = (char) read((char)(addr_abs + 0));
        char hi = (char) read((char)(addr_abs + 1));
        pc = (char)((hi << 8) | lo);

        // IRQs take time
        cycles = 7;
    }		// Interrupt Request - Executes an instruction at a specific location
    // A Non-Maskable Interrupt cannot be ignored. It behaves in exactly the
    //same way as a regular IRQ, but reads the new program counter address
    //form location 0xFFFA.
    public void nmi(){
        write((char)(0x0100 + stkp), (byte)((pc >> 8) & 0x00FF));
        stkp--;
        write((char)(0x0100 + stkp), (byte)(pc & 0x00FF));
        stkp--;

        SetFlag(FLAGS6502.B, false);
        SetFlag(FLAGS6502.U, true);
        SetFlag(FLAGS6502.I, true);
        write((char)(0x0100 + stkp), status);
        stkp--;

        addr_abs = 0xFFFA;
        char lo = (char) read((char) (addr_abs + 0));
        char hi = (char) read((char) (addr_abs + 1));
        pc = (char)((hi << 8) | lo);

        cycles = 8;
    }		// Non-Maskable Interrupt Request - As above, but cannot be disabled
    public void clock(){
        // Each instruction requires a variable number of clock cycles to execute.
        // In my emulation, I only care about the final result and so I perform
        // the entire computation in one hit. In hardware, each clock cycle would
        // perform "microcode" style transformations of the CPUs state.
        //
        // To remain compliant with connected devices, it's important that the
        // emulation also takes "time" in order to execute instructions, so I
        // implement that delay by simply counting down the cycles required by
        // the instruction. When it reaches 0, the instruction is complete, and
        // the next one is ready to be executed.
        if (this.cycles == 0) {
            // Read next instruction byte. This 8-bit value is used to index
            // the translation table to get the relevant information about
            // how to implement the instruction
            opcode = read(pc);
        }
        // Increment global clock count - This is actually unused unless logging is enabled
        // but I've kept it in because its a handy watch variable for debugging
        clock_count++;

        // Decrement the number of cycles remaining for this instruction
        cycles--;
    }	// Perform one clock cycle's worth of update

    // Indicates the current instruction has completed by returning true. This is
    // a utility function to enable "step-by-step" execution, without manually
    // clocking every cycle
    public boolean complete(){

    }

    // Link this CPU to a communications bus
    public void ConnectBus(Bus n) { bus = n; }

    // Produces a map of strings, with keys equivalent to instruction start locations
    // in memory, for the specified address range
    private final Map<char, String> disassemble(char nStart, char nStop) {
        return null;
    }

    // The status register stores 8 flags. Ive enumerated these here for ease
    // of access. You can access the status register directly since its public.
    // The bits have different interpretations depending upon the context and
    // instruction being executed.
    public enum FLAGS6502 {
        C((byte)(1 << 0)),	// Carry Bit
        Z((byte)(1 << 1)),	// Zero
        I((byte)(1 << 2)),	// Disable Interrupts
        D((byte)(1 << 3)),	// Decimal Mode (unused in this IMP()lementation)
        B((byte)(1 << 4)),	// Break
        U((byte)(1 << 5)),	// Unused
        V((byte)(1 << 6)),	// Overflow
        N((byte)(1 << 7));	// Negative

        private byte bit_val;

        FLAGS6502(byte bit_val) {
            this.bit_val = bit_val;
        }

        public byte getBit_val() {
            return bit_val;
        }

    }

    ///////////////////////////////////////////////////////////////////////////////
    //FLAG FUNCTIONS

    // Returns the value of a specific bit of the status register
    private byte GetFlag(FLAGS6502 f){
        return (byte) (((status & f.getBit_val()) > 0) ? 1 : 0);
    }

    // Sets or clears a specific bit of the status register
    private void SetFlag(FLAGS6502 f, boolean v){
        if (v)
            status |= f.getBit_val();
        else
            status &= ~f.getBit_val();
    }

    // Assisstive variables to facilitate emulation
    private byte fetched = 0x00;   // Represents the working input value to the ALU
    private char temp = 0x0000; // A convenience variable used everywhere
    private char addr_abs = 0x0000; // All used memory addresses end up in here
    private char addr_rel = 0x00;   // Represents ABS()olute address following a branch
    private byte opcode = 0x00;   // Is the instruction byte
    private byte cycles = 0;	   // Counts how many cycles the instruction has remaining
    private int clock_count = 0;	   // A global accumulation of the number of clocks

    // Linkage to the communications bus
    private Bus bus = null;
    private byte read(char a){
        return bus.read(a, false);
    }
    private void write(char a, byte d){
        bus.write(a, d);
    }

    // The read location of data can come from two sources, a memory address, or
    // its IMM()ediately available as part of the instruction. This function decides
    // depending on address mode of instruction byte
    private byte fetch(){
        if (!(lookup[opcode].addrmode == mode.IMP))
            fetched = read(addr_abs);
        return fetched;
    }

    // This structure and the following vector are used to compile and store
    // the opcode translation table. The 6502 can effectively have 256
    // different instructions. Each of these are stored in a table in numerical
    // order so they can be looked up easily, with no decoding required.
    // Each table entry holds:
    //	Pneumonic : A textual representation of the instruction (used for disassembly)
    //	Opcode Function: A function pointer to the IMP()lementation of the opcode
    //	Opcode Address Mode : A function pointer to the IMP()lementation of the
    //						  addressing mechanism used by the instruction
    //	Cycle Count : An integer that represents the base number of clock cycles the
    //				  CPU requires to perform the instruction

    private class INSTRUCTION
    {
        String name;
        opCode operate = null;
        mode addrmode = null;
        byte cycles = 0;
        public INSTRUCTION(String name, opCode operate, mode addrmode, byte cycles){
            this.name = name;
            this.operate = operate;
            this.addrmode = addrmode;
            this.cycles = cycles;
        }
    };

    private INSTRUCTION[] lookup;

    // Addressing Modes =============================================
    // The 6502 has a variety of addressing modes to access data in
    // memory, some of which are direct and some are indirect (like
    // pointers in C++). Each opcode contains information about which
    // addressing mode should be employed to facilitate the
    // instruction, in regards to where it reads/writes the data it
    // uses. The address mode changes the number of bytes that
    // makes up the full instruction, so we IMP()lement addressing
    // before executing the instruction, to make sure the program
    // counter is at the correct location, the instruction is
    // primed with the addresses it needs, and the number of clock
    // cycles the instruction requires is calculated. These functions
    // may adjust the number of cycles required depending upon where
    // and how the memory is accessed, so they return the required
    // adjustment.

    private byte IMP(){
        fetched = a;
        return 0;
    }

    // Address Mode: Immediate
    // The instruction expects the next byte to be used as a value, so we'll prep
    // the read address to point to the next byte
    private byte IMM() {
        addr_abs = pc++;
        return 0;
    }

    // Address Mode: Zero Page
    // To save program bytes, zero page addressing allows you to absolutely address
    // a location in first 0xFF bytes of address range. Clearly this only requires
    // one byte instead of the usual two.
    private byte ZP0(){
        addr_abs = (char)read(pc);
        pc++;
        addr_abs &= 0x00FF;
        return 0;
    }

    // Address Mode: Zero Page with X Offset
    // Fundamentally the same as Zero Page addressing, but the contents of the X Register
    // is added to the supplied single byte address. This is useful for iterating through
    // ranges within the first page.
    private byte ZPX(){
        addr_abs = (char)(read(pc) + x);
        pc++;
        addr_abs &= 0x00FF;
        return 0;
    }

    // Address Mode: Zero Page with Y Offset
    //Same as above but uses Y Register for offset
    private byte ZPY(){
        addr_abs = (char)(read(pc) + y);
        pc++;
        addr_abs &= 0x00FF;
        return 0;
    }

    // Address Mode: Relative
    //This address mode is exclusive to branch instructions. The address
    //must reside within -128 to +127 of the branch instruction, i.e.
    //you cant directly branch to any address in the addressable range.
    private byte REL(){
        addr_rel = (char)read(pc);
        pc++;
        if (((addr_rel & 0x80) == 0) ? false : true)
            addr_rel |= 0xFF00;
        return 0;
    }

    // Address Mode: Absolute
    //A full 16-bit address is loaded and used
    private byte ABS(){
        char lo = (char)read(pc);
        pc++;
        char hi = (char)read(pc);
        pc++;

        addr_abs = (char)((hi << 8) | lo);

        return 0;
    }

    // Address Mode: Absolute with X Offset
    //Fundamentally the same as absolute addressing, but the contents of the X Register
    //is added to the supplied two byte address. If the resulting address changes
    //the page, an additional clock cycle is required
    private byte ABX(){
        char lo = (char)read(pc);
        pc++;
        char hi = (char)read(pc);
        pc++;

        addr_abs = (char)((hi << 8) | lo);
        addr_abs += x;

        if ((addr_abs & 0xFF00) != (hi << 8))
            return 1;
        else
            return 0;
    }
    // Address Mode: Absolute with Y Offset
    //Fundamentally the same as absolute addressing, but the contents of the Y Register
    //is added to the supplied two byte address. If the resulting address changes
    //the page, an additional clock cycle is required
    private byte ABY(){
        char lo = (char)read(pc);
        pc++;
        char hi = (char)read(pc);
        pc++;

        addr_abs = (char)((hi << 8) | lo);
        addr_abs += y;

        if ((addr_abs & 0xFF00) != (hi << 8))
            return 1;
        else
            return 0;
    }

    // Note: The next 3 address modes use indirection (aka Pointers!)

    // Address Mode: Indirect
    //The supplied 16-bit address is read to get the actual 16-bit address. This is
    //instruction is unusual in that it has a bug in the hardware! To emulate its
    //function accurately, we also need to emulate this bug. If the low byte of the
    //supplied address is 0xFF, then to read the high byte of the actual address
    //we need to cross a page boundary. This doesnt actually work on the chip as
    //designed, instead it wraps back around in the same page, yielding an
    //invalid actual address
    private byte IND(){
        char ptr_lo = (char) read(pc);
        pc++;
        char ptr_hi = (char) read(pc);
        pc++;

        char ptr = (char)((ptr_hi << 8) | ptr_lo);

        if (ptr_lo == 0x00FF) // Simulate page boundary hardware bug
        {
            addr_abs = (char) ((read((char)(ptr & 0xFF00)) << 8) | read((char)(ptr + 0)));
        }
        else // Behave normally
        {
            addr_abs = (char)((read((char)(ptr + 1)) << 8) | read((char)(ptr + 0)));
        }

        return 0;
    }

    // Address Mode: Indirect X
    //The supplied 8-bit address is offset by X Register to index
    //a location in page 0x00. The actual 16-bit address is read
    //from this location
    private byte IZX(){
        char t = (char)read(pc);
        pc++;

        char lo = (char) read((char) ((t + (char)x) & 0x00FF));
        char hi = (char) read((char)((t + (char)x + 1) & 0x00FF));

        addr_abs = (char)((hi << 8) | lo);

        return 0;
    }

    // Address Mode: Indirect Y
    //The supplied 8-bit address indexes a location in page 0x00. From
    //here the actual 16-bit address is read, and the contents of
    //Y Register is added to it to offset it. If the offset causes a
    //change in page then an additional clock cycle is required.
    private byte IZY(){
        char t = (char)read(pc);
        pc++;

        char lo = (char)read((char)(t & 0x00FF));
        char hi = (char)read((char)((t + 1) & 0x00FF));

        addr_abs = (char)((hi << 8) | lo);
        addr_abs += y;

        if ((addr_abs & 0xFF00) != (hi << 8))
            return 1;
        else
            return 0;
    }

    // (KEK NOTE) In case, that Java have not function variables in function
    // , and I remaked structures as classes i have to use that kind of piece of shit.
    // so some Operation Code will trigger related functions and mode
    private enum mode{
        IMP,
        IMM,
        ZP0,
        ZPX,
        ZPY,
        REL,
        ABS,
        ABX,
        ABY,
        IND,
        IZX,
        IZY;
    }

    // Opcodes ======================================================
    // There are 56 "legitimate" opcodes provided by the 6502 CPU. I
    // have not modelled "unofficial" opcodes. As each opcode is
    // defined by 1 byte, there are potentially 256 possible codes.
    // Codes are not used in a "switch case" style on a processor,
    // instead they are repsonisble for switching individual parts of
    // CPU circuits on and off. The opcodes listed here are official,
    // meaning that the functionality of the chip when provided with
    // these codes is as the developers intended it to be. Unofficial
    // codes will of course also influence the CPU circuitry in
    // interesting ways, and can be exploited to gain additional
    // functionality!
    //
    // These functions return 0 normally, but some are capable of
    // requiring more clock cycles when executed under certain
    // conditions combined with certain addressing modes. If that is
    // the case, they return 1.
    //
    // I have included detailed explanations of each function in
    // the class IMP()lementation file. Note they are listed in
    // alphabetical order here for ease of finding.

    private enum opCode{
        ADC,
        AND,
        ASL,
        BCC,
        BCS,
        BEQ,
        BIT,
        BMI,
        BNE,
        BPL,
        BRK,
        BVC,
        BVS,
        CLC,
        CLD,
        CLI,
        CLV,
        CMP,
        CPX,
        CPY,
        DEC,
        DEX,
        DEY,
        EOR,
        INC,
        INX,
        INY,
        JMP,
        JSR,
        LDA,
        LDX,
        LDY,
        LSR,
        NOP,
        ORA,
        PHA,
        PHP,
        PLA,
        PLP,
        ROL,
        ROR,
        RTI,
        RTS,
        SBC,
        SEC,
        SED,
        SEI,
        STA,
        STX,
        STY,
        TAX,
        TAY,
        TSX,
        TXA,
        TXS,
        TYA,
        XXX;
    }

    // We can see how the above equation calculates V, based on A, M and R. V was chosen
    //based on the following hypothesis:
    //      Positive Number + Positive Number = Negative Result -> Overflow
    //      Negative Number + Negative Number = Positive Result -> Overflow
    //      Positive Number + Negative Number = Either Result -> Cannot Overflow
    //      Positive Number + Positive Number = Positive Result -> OK! No Overflow
    //      Negative Number + Negative Number = Negative Result -> OK! NO Overflow
    private byte ADC(){
        // Grab the data that we are adding to the accumulator
        fetch();

        // Add is performed in 16-bit domain for emulation to capture any
        // carry bit, which will exist in bit 8 of the 16-bit word
        temp = (char) ((char)a + (int)fetched + (char) GetFlag(FLAGS6502.C));

        // The carry flag out exists in the high byte bit 0
        SetFlag(FLAGS6502.C, temp > 255);

        // The Zero flag is set if the result is 0
        SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0);

        // The signed Overflow flag is set based on all that up there! :D
        SetFlag(FLAGS6502.V, (((~((char)a ^ (char)fetched) & ((char)a ^ (char)temp)) & 0x0080) == 0) ? false : true);

        // The negative flag is set to the most significant bit of the result
        SetFlag(FLAGS6502.N, ((temp & 0x80) == 0) ? false : true);

        // Load the result into the accumulator (it's 8-bit dont forget!)
        a = (byte)(temp & 0x00FF);

        // This instruction has the potential to require an additional clock cycle
        return 1;
    }

    //OK! Complicated operations are done! the following are much simpler
    //and conventional. The typical order of events is:
    //1) Fetch the data you are working with
    //2) Perform calculation
    //3) Store the result in desired place
    //4) Set Flags of the status register
    //5) Return if instruction has potential to require additional
    //   clock cycle


    // Instruction: Bitwise Logic AND
    //Function:    A = A & M
    //Flags Out:   N, Z
    private byte AND(){
        fetch();
        a = (byte)(a & fetched);
        SetFlag(FLAGS6502.Z, ((a == 0x00) == false) ? false : true);
        SetFlag(FLAGS6502.N, ((a & 0x80) == 0) ? false : true);
        return 1;
    }

    // Instruction: Arithmetic Shift Left
    //Function:    A = C <- (A << 1) <- 0
    //Flags Out:   N, Z, C
    private	byte ASL(){
        fetch();
        temp = (char)(fetched << 1);
        SetFlag(FLAGS6502.C, (temp & 0xFF00) > 0);
        SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x00);
        SetFlag(FLAGS6502.N, ((temp & 0x80) == 0) ? false : true);
        if (lookup[opcode].addrmode == mode.IMP)
            a = (byte)(temp & 0x00FF);
        else
            write(addr_abs, (byte) (temp & 0x00FF));
        return 0;
    }

    // Instruction: Branch if Carry Clear
    //Function:    if(C == 0) pc = address
    private byte BCC(){
        if (GetFlag(FLAGS6502.C) == 0)
        {
            cycles++;
            addr_abs = (char)(pc + addr_rel);

            if((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;

            pc = addr_abs;
        }
        return 0;
    }

    // Instruction: Branch if Carry Set
    //Function:    if(C == 1) pc = address
    private byte BCS(){
        if (GetFlag(FLAGS6502.C) == 1)
        {
            cycles++;
            addr_abs = (char) (pc + addr_rel);

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;

            pc = addr_abs;
        }
        return 0;
    }

    // Instruction: Branch if Equal
    //Function:    if(Z == 1) pc = address
    private byte BEQ(){
        if (GetFlag(FLAGS6502.Z) == 1)
        {
            cycles++;
            addr_abs = (char) (pc + addr_rel);

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;

            pc = addr_abs;
        }
        return 0;
    }

    private byte BIT(){
        fetch();
        temp = (char) (a & fetched);
        SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x00);
        SetFlag(FLAGS6502.N, ((fetched & (1 << 7)) == 0) ? false : true);
        SetFlag(FLAGS6502.V, ((fetched & (1 << 6)) == 0) ? false : true);
        return 0;
    }

    // Instruction: Branch if Negative
    //Function:    if(N == 1) pc = address
    private byte BMI(){
        if (GetFlag(FLAGS6502.N) == 1)
        {
            cycles++;
            addr_abs = (char) (pc + addr_rel);

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;

            pc = addr_abs;
        }
        return 0;
    }

    // Instruction: Branch if Not Equal
    //Function:    if(Z == 0) pc = address
    private byte BNE(){
        if (GetFlag(FLAGS6502.Z) == 0)
        {
            cycles++;
            addr_abs = (char) (pc + addr_rel);

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;

            pc = addr_abs;
        }
        return 0;
    }

    // Instruction: Branch if Positive
    //Function:    if(N == 0) pc = address
    private byte BPL(){
        if (GetFlag(FLAGS6502.N) == 0)
        {
            cycles++;
            addr_abs = (char) (pc + addr_rel);

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;

            pc = addr_abs;
        }
        return 0;
    }

    // Instruction: Break
    //Function:    Program Sourced Interrupt
    private byte BRK(){
        pc++;

        SetFlag(FLAGS6502.I, 1);
        write((char) (0x0100 + stkp), (byte) ((pc >> 8) & 0x00FF));
        stkp--;
        write((char) (0x0100 + stkp), (byte) (pc & 0x00FF));
        stkp--;

        SetFlag(FLAGS6502.B, true);
        write((char) (0x0100 + stkp), status);
        stkp--;
        SetFlag(FLAGS6502.B, false);

        pc = (char) ((char)read((char) 0xFFFE) | ((char)read((char) 0xFFFF) << 8));
        return 0;
    }

    // Instruction: Branch if Overflow Clear
    //Function:    if(V == 0) pc = address
    private byte BVC(){
        if (GetFlag(FLAGS6502.V) == 0)
        {
            cycles++;
            addr_abs = (char) (pc + addr_rel);

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;

            pc = addr_abs;
        }
        return 0;
    }

    // Instruction: Branch if Overflow Set
    //Function:    if(V == 1) pc = address
    private byte BVS(){
        if (GetFlag(FLAGS6502.V) == 1)
        {
            cycles++;
            addr_abs = (char) (pc + addr_rel);

            if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                cycles++;

            pc = addr_abs;
        }
        return 0;
    }

    // Instruction: Clear Carry Flag
    // Function:    C = 0
    private byte CLC(){
        SetFlag(FLAGS6502.C, false);
        return 0;
    }

    // Instruction: Clear Decimal Flag
// Function:    D = 0
    private byte CLD(){
        SetFlag(FLAGS6502.D, false);
        return 0;
    }

    // Instruction: Disable Interrupts / Clear Interrupt Flag
// Function:    I = 0
    private byte CLI(){
        SetFlag(FLAGS6502.I, false);
        return 0;
    }

    // Instruction: Clear Overflow Flag
// Function:    V = 0
    private byte CLV(){
        SetFlag(FLAGS6502.V, false);
        return 0;
    }

    // Instruction: Compare Accumulator
// Function:    C <- A >= M      Z <- (A - M) == 0
// Flags Out:   N, C, Z
    private byte CMP(){
        fetch();
        temp = (char) ((char)a - (char)fetched);
        SetFlag(FLAGS6502.C, a >= fetched);
        SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(FLAGS6502.N, ((temp & 0x0080) == 0) ? false : true);
        return 1;
    }

    // Instruction: Compare X Register
// Function:    C <- X >= M      Z <- (X - M) == 0
// Flags Out:   N, C, Z
    private byte CPX(){
        fetch();
        temp = (char) ((char)x - (char)fetched);
        SetFlag(FLAGS6502.C, x >= fetched);
        SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(FLAGS6502.N, ((temp & 0x0080) == 0) ? false : true);
        return 0;
    }


    // Instruction: Compare Y Register
// Function:    C <- Y >= M      Z <- (Y - M) == 0
// Flags Out:   N, C, Z
    private byte CPY(){
        fetch();
        temp = (char) ((char)y - (char)fetched);
        SetFlag(FLAGS6502.C, y >= fetched);
        SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(FLAGS6502.N, ((temp & 0x0080) == 0) ? false : true);
        return 0;
    }

    // Instruction: Decrement Value at Memory Location
// Function:    M = M - 1
// Flags Out:   N, Z
    private byte DEC(){
        fetch();
        temp = (char) (fetched - 1);
        write(addr_abs, (byte) (temp & 0x00FF));
        SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(FLAGS6502.N, ((temp & 0x0080) == 0) ? false : true);
        return 0;
    }

    // Instruction: Decrement X Register
// Function:    X = X - 1
// Flags Out:   N, Z
    private byte DEX(){
        x--;
        SetFlag(FLAGS6502.Z, x == 0x00);
        SetFlag(FLAGS6502.N, ((x & 0x80) == 0) ? false : true);
        return 0;
    }

    // Instruction: Decrement Y Register
// Function:    Y = Y - 1
// Flags Out:   N, Z
    private byte DEY(){
        y--;
        SetFlag(FLAGS6502.Z, y == 0x00);
        SetFlag(FLAGS6502.N, ((y & 0x80) == 0) ? false : true);
        return 0;
    }

    // Instruction: Bitwise Logic XOR
// Function:    A = A xor M
// Flags Out:   N, Z
    private byte EOR(){
        fetch();
        a = (byte) (a ^ fetched);
        SetFlag(FLAGS6502.Z, a == 0x00);
        SetFlag(FLAGS6502.N, ((a & 0x80) == 0) ? false : true);
        return 1;
    }

    // Instruction: Increment Value at Memory Location
// Function:    M = M + 1
// Flags Out:   N, Z
    private byte INC(){
        fetch();
        temp = (char) (fetched + 1);
        write(addr_abs, (byte) (temp & 0x00FF));
        SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
        SetFlag(FLAGS6502.N, ((temp & 0x0080) == 0) ? false : true);
        return 0;
    }

    // Instruction: Increment X Register
// Function:    X = X + 1
// Flags Out:   N, Z
    private byte INX(){
        x++;
        SetFlag(FLAGS6502.Z, x == 0x00);
        SetFlag(FLAGS6502.N, ((x & 0x80) == 0) ? false : true);
        return 0;
    }

    // Instruction: Increment Y Register
// Function:    Y = Y + 1
// Flags Out:   N, Z
    private byte INY(){
        y++;
        SetFlag(FLAGS6502.Z, y == 0x00);
        SetFlag(FLAGS6502.N, ((y & 0x80) == 0) ? false : true);
        return 0;
    }

    // Instruction: Jump To Location
// Function:    pc = address
    private byte JMP(){
        pc = addr_abs;
        return 0;
    }

    // Instruction: Jump To Sub-Routine
// Function:    Push current pc to stack, pc = address
    private byte JSR(){
        pc--;

        write((char) (0x0100 + stkp), (byte) ((pc >> 8) & 0x00FF));
        stkp--;
        write((char) (0x0100 + stkp), (byte) (pc & 0x00FF));
        stkp--;

        pc = addr_abs;
        return 0;
    }

    // Instruction: Load The Accumulator
// Function:    A = M
// Flags Out:   N, Z
    private byte LDA(){
        fetch();
        a = fetched;
        SetFlag(FLAGS6502.Z, a == 0x00);
        SetFlag(FLAGS6502.N, ((a & 0x80) == 0) ? false : true);
        return 1;
    }
    private byte LDX();
    private byte LDY();
    private byte LSR();
    private byte NOP();
    private byte ORA();
    private byte PHA();
    private byte PHP();
    private byte PLA();
    private byte PLP();
    private byte ROL();
    private byte ROR();
    private byte RTI();
    private byte RTS();
    private byte SBC();
    private byte SEC();
    private byte SED();
    private byte SEI();
    private byte STA();
    private byte STX();
    private byte STY();
    private byte TAX();
    private byte TAY();
    private byte TSX();
    private byte TXA();
    private byte TXS();
    private byte TYA();

    // I capture all "unofficial" opcodes with this function. It is
    // functionally identical to a NOP
    private byte XXX();

}

//pattern ((temp & 0x80) == 0) ? false : true
// [^\t ]//  \n    //