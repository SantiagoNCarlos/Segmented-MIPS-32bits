--Autores: Carlos, Santiago
--         Lalanne, Agust�n
--Profesor: Vazquez, Martin
--
--06/2021

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity processor is
port(
	Clk         : in  std_logic;
	Reset       : in  std_logic;
	-- Instruction memory
	I_Addr      : out std_logic_vector(31 downto 0);
	I_RdStb     : out std_logic;
	I_WrStb     : out std_logic;
	I_DataOut   : out std_logic_vector(31 downto 0);
	I_DataIn    : in  std_logic_vector(31 downto 0);
	-- Data memory
	D_Addr      : out std_logic_vector(31 downto 0);
	D_RdStb     : out std_logic;
	D_WrStb     : out std_logic;
	D_DataOut   : out std_logic_vector(31 downto 0);
	D_DataIn    : in  std_logic_vector(31 downto 0)
);
end processor;

architecture processor_arq of processor is 
COMPONENT ControlUnit 
    PORT ( 
	   ID_Instruct   : in std_logic_vector(5 downto 0);--Primeros 6 bits de la salida de la memoria de instruccion
	   ID_RegWrite : out std_logic;                           
	   ID_MemToReg : out std_logic; 
       ID_BranchEquals : out std_logic;
       ID_MemRead  : out std_logic;     
       ID_MemWrite : out std_logic;     
       ID_RegDst   : out std_logic;   
       ID_AluOp    : out std_logic_vector(2 downto 0);    
       ID_AluSrc   : out std_logic
       );
    END COMPONENT;

	COMPONENT Registers
	PORT ( 
		clk : in STD_LOGIC;
		reset : in STD_LOGIC;
		wr : in STD_LOGIC;
		reg1_rd : in STD_LOGIC_VECTOR (4 downto 0);
		reg2_rd : in STD_LOGIC_VECTOR (4 downto 0);
		reg_wr : in STD_LOGIC_VECTOR (4 downto 0);
		data_wr : in STD_LOGIC_VECTOR (31 downto 0);
		data1_rd : out STD_LOGIC_VECTOR (31 downto 0);
		data2_rd : out STD_LOGIC_VECTOR (31 downto 0)
		);
	END COMPONENT;

	COMPONENT ALU
	PORT(
		a : in STD_LOGIC_VECTOR (31 downto 0);
		b : in STD_LOGIC_VECTOR (31 downto 0);
		sel : in STD_LOGIC_VECTOR (2 downto 0); -- selector
		result : out STD_LOGIC_VECTOR(31 downto 0);
		zero : out STD_LOGIC 
		);
	END COMPONENT;
	
	SIGNAL clock : std_logic;
	SIGNAL rst : STD_LOGIC := '0';


	--Seniales de la etapa IF
	signal IF_PcIn: std_logic_vector(31 downto 0); 
	signal IF_PCOut: std_logic_vector(31 downto 0);	
	signal IF_Next: std_logic_vector(31 downto 0);	-- Direccion PC + 4 
	signal IF_DataOut: std_logic_vector(31 downto 0);
	
	--Seniales de segmentacion IFID
	signal IFID_Next: std_logic_vector(31 downto 0);
	signal IFID_Inst: std_logic_vector(31 downto 0);

    --Seniales de la etapa ID
	signal ID_Instruction: std_logic_vector(31 downto 0);
	signal ID_SignExt: std_logic_vector(31 downto 0);
	signal ID_DataReg1: std_logic_vector(31 downto 0);
	signal ID_DataReg2: std_logic_vector(31 downto 0);
	signal ID_Next: std_logic_vector(31 downto 0);
	
	--Seniales de salida de UC
	signal ID_RegWrite: std_logic;
	signal ID_MemToReg: std_logic;
	signal ID_BranchEquals: std_logic;
	signal ID_MemRead: std_logic;
	signal ID_MemWrite: std_logic;
	signal ID_RegDst: std_logic;
	signal ID_AluOp: std_logic_vector(2 downto 0);
	signal ID_AluSrc: std_logic;
	
	--Seniales de segmentacion IDEX
	signal IDEX_RegWrite: std_logic;
	signal IDEX_MemToReg: std_logic;
	signal IDEX_BranchEquals: std_logic;
	signal IDEX_MemRead: std_logic;
	signal IDEX_MemWrite: std_logic;
	signal IDEX_RegDst: std_logic;
	signal IDEX_AluOp: std_logic_vector(2 downto 0);
	signal IDEX_AluSrc: std_logic;
	signal IDEX_PcNext: std_logic_vector(31 downto 0); 
    signal IDEX_DataReg1: std_logic_vector(31 downto 0);
    signal IDEX_DataReg2: std_logic_vector(31 downto 0);
    signal IDEX_SignExt: std_logic_vector(31 downto 0);
	signal IDEX_Rt: std_logic_vector(4 downto 0); -- (20 a 16)
	signal IDEX_Rd: std_logic_vector(4 downto 0); -- (15 a 11)

	--Señales de EX
	signal EXE_AluControl: std_logic_vector(2 downto 0);
	signal EXE_SignExt: std_logic_vector(31 downto 0);
	signal EXE_AluOp: std_logic_vector(2 downto 0);
	signal EXE_RegDst: std_logic;
	signal EXE_Rt: std_logic_vector(4 downto 0);
	signal EXE_Rd: std_logic_vector(4 downto 0);
	signal EXE_BranchAddress: std_logic_vector(31 downto 0);
	signal EXE_Result: std_logic_vector(31 downto 0); -- Resultado ALU
	signal EXE_zero: STD_LOGIC;
	signal EXE_AluMux: std_logic_vector(31 downto 0); -- Mux con dependencia en ALUSrc (2do operando ALU)
	signal EXE_DataReg1: std_logic_vector(31 downto 0);
	signal EXE_DataReg2: std_logic_vector(31 downto 0);
	signal EXE_AluSrc: std_logic;
	signal EXE_PcNext: std_logic_vector(31 downto 0);
	signal EXE_WriteDest: std_logic_vector(4 downto 0); -- Mux con dependencia en RgsDst
	signal EXE_BranchEquals: std_logic;
	signal EXE_MemWrite: std_logic;
    signal EXE_MemRead: std_logic;
    signal EXE_MemToReg: std_logic;
    signal EXE_RegWrite: std_logic;

	
    -- Señales de segmentacion EXMEM
	signal EXMEM_BranchAddress: std_logic_vector(31 downto 0);
	signal EXMEM_Zero: std_logic;
	signal EXMEM_BranchEquals: std_logic;
	signal EXMEM_MemWrite: std_logic;
	signal EXMEM_MemRead: std_logic; 
	signal EXMEM_MemToReg: std_logic;
	signal EXMEM_RegWrite: std_logic;
	signal EXMEM_Result: std_logic_vector(31 downto 0);
	signal EXMEM_WriteDest: std_logic_vector(4 downto 0);
	signal EXMEM_DataReg2: std_logic_vector(31 downto 0);


	-- Señales de la etapa de Memoria MEM
    signal MEM_WriteDest: std_logic_vector(4 downto 0);
    signal MEM_BranchEquals: std_logic;
    signal MEM_MemWrite: std_logic;
    signal MEM_MemRead: std_logic;
    signal MEM_MemToReg: std_logic;
    signal MEM_RegWrite: std_logic;
    signal MEM_Zero: std_logic;
    signal MEM_Result: std_logic_vector(31 downto 0);
    signal MEM_DataReg2: std_logic_vector(31 downto 0);
	signal MEM_PcSrc: std_logic; --selector de salto
	signal MEM_BranchAddress: std_logic_vector(31 downto 0); --direccion de salto
	signal MEM_DataOut: std_logic_vector(31 downto 0);
	
	--Seniales de segmentacion MEMWB
	signal MEMWB_RegWrite: std_logic; 
    signal MEMWB_MemToReg: std_logic;
    signal MEMWB_Result: std_logic_vector (31 downto 0);
    signal MEMWB_WriteDest: std_logic_vector(4 downto 0);
	signal MEMWB_DataOut: std_logic_vector(31 downto 0);

	--Señales de WriteBack WB
	signal WB_MemToReg: std_logic;
	signal WB_DataOut: std_logic_vector(31 downto 0);
	signal WB_Result: std_logic_vector(31 downto 0);
	signal WB_RegWrite: std_logic;
	signal WB_MuxWbResult: std_logic_vector(31 downto 0);
	signal WB_WriteDest: std_logic_vector(4 downto 0);
  
begin 	

------------------ Etapa IF ------------------------------------
    
    clock <= clk;
    rst <= reset;
    
	-- Conexion de entradas/salidas de la  memoria de instruccion a seniales internas del MIPS
	I_Addr <= IF_PCOut;
	I_RdStb <= '1'; 
	I_WrStb <= '0';
	I_DataOut <= x"00000000"; 
	IF_DataOut <= I_DataIn;
   
	PC_reg: process(clock,rst) -- Carga de la direccion de la siguiente instruccion
	
	begin
	     if (Reset = '1') then 
	   		IF_PCOut <= (others => '0');
	     elsif (rising_edge(clock)) then
			IF_PCOut <= IF_PCIn;
	   	end if;
	end process;

	IF_Next <= IF_PCOut + x"00000004";
	
	IF_PCIN <= MEM_BranchAddress when MEM_PcSrc = '1' else IF_Next;  --mux para saltos de instruccion
	
	SegmentacionIFID: process(clock,rst)
	begin
		if (Reset = '1') then 
			IFID_Next <= (others => '0');
			IFID_Inst <= (others => '0');
	     elsif rising_edge(clock) then
			IFID_Next <= IF_Next;
			IFID_Inst <= IF_DataOut;
	   		end if; 
	end process;

	--------------------EtapaID-------------------------
	
		
	Regs: Registers PORT MAP( -- Instanciacion banco de Registros
		clk => clock,
		reset => rst,
		wr => WB_RegWrite,   
		reg1_rd => ID_Instruction(25 downto 21),
		reg2_rd => ID_Instruction(20 downto 16),
		reg_wr => WB_WriteDest,  
		data_wr => WB_MuxWbResult,   
		data1_rd => ID_DataReg1,
		data2_rd => ID_DataReg2
		); 
	   
	
	   UC: ControlUnit PORT MAP(
	    ID_Instruct => ID_Instruction(31 downto 26),
		ID_RegWrite => ID_RegWrite,
		ID_MemToReg => ID_MemToReg, 
		ID_BranchEquals => ID_BranchEquals,
		ID_MemRead => ID_MemRead,
		ID_MemWrite => ID_MemWrite,
		ID_RegDst => ID_RegDst,
		ID_AluOp => ID_AluOp,
		ID_AluSrc => ID_AluSrc
	    );
	
	ID_Next <= IFID_Next;
	ID_Instruction <= IFID_Inst; 

    
	--Sign Extender
	ID_SignExt <= x"0000" & ID_Instruction(15 downto 0) when (ID_Instruction(15) = '0') else  (x"FFFF" & ID_Instruction(15 downto 0));
	
SegmentacionIDEX:process (clock,rst)
	begin
		if (rst = '1') then
			IDEX_PcNext <= (others => '0');
			IDEX_DataReg1 <=(others => '0');
			IDEX_DataReg2 <=(others => '0');
			IDEX_SignExt <=(others => '0');
			IDEX_RegDst <= '0';
			IDEX_AluSrc <= '0';
			IDEX_AluOp <= "000";
			IDEX_BranchEquals <= '0';
			IDEX_MemWrite <='0';
			IDEX_MemRead <= '0';
			IDEX_MemToReg <= '0';
			IDEX_RegWrite <='0';
			IDEX_Rt <= (others => '0'); 
			IDEX_Rd <= (others => '0'); 
		elsif (rising_edge(clock)) then
			IDEX_PcNext <= ID_Next;			
			IDEX_DataReg1 <= ID_DataReg1;		
			IDEX_DataReg2 <= ID_DataReg2;		
			IDEX_SignExt <= ID_SignExt;		
			IDEX_RegDst <= ID_RegDst;		
			IDEX_AluSrc <= ID_AluSrc;		
			IDEX_AluOp <= ID_AluOp;			
			IDEX_Rt <= ID_Instruction(20 downto 16); 
			IDEX_Rd <= ID_Instruction(15 downto 11); 
			IDEX_BranchEquals <= ID_BranchEquals;
			IDEX_MemWrite <= ID_MemWrite;
			IDEX_MemRead <= ID_MemRead;
			IDEX_MemToReg <= ID_MemToReg;
			IDEX_RegWrite <= ID_RegWrite;
		end if;
	end process;

--------------------EtapaEX-------------------------

AritmeticalLogicalUnity: ALU 
    PORT MAP(
        a => EXE_DataReg1,
        b => EXE_AluMux,
        sel => EXE_AluControl,
        result => EXE_Result,
        zero => EXE_Zero
    );

EXE_DataReg1 <= IDEX_DataReg1;
EXE_Rt <= IDEX_Rt;
EXE_Rd <= IDEX_Rd;
EXE_SignExt <= IDEX_SignExt;
EXE_AluOp <= IDEX_AluOp;
EXE_DataReg2 <= IDEX_DataReg2;
EXE_AluSrc <= IDEX_AluSrc;
EXE_PcNext <= IDEX_PcNext;
EXE_RegDst <= IDEX_RegDst;
EXE_BranchEquals <= IDEX_BranchEquals;
EXE_MemWrite <= IDEX_MemWrite;
EXE_MemRead <= IDEX_MemRead;
EXE_MemToReg <= IDEX_MemToReg;
EXE_RegWrite <= IDEX_RegWrite;
EXE_BranchAddress <= EXE_PcNext + (EXE_SignExt(29 downto 0) & "00");
EXE_AluMux <= EXE_DataReg2 when (EXE_AluSrc = '0') else EXE_SignExt;
EXE_WriteDest <= EXE_Rt when (EXE_RegDst = '0') else EXE_Rd;

AluControl: process (EXE_SignExt(5 downto 0), EXE_AluOp)
begin
     case(EXE_AluOp) is
       	when "010" => --Tipo R
            case (EXE_SignExt(5 downto 0)) is 
                when "100000"=>  	--ADD                  
                	EXE_AluControl <= "010";   
				when"100010" => 	--SUB 
					EXE_AluControl <= "110";
				when "100100" =>	 -- AND
                    EXE_AluControl <= "000";
				when "100101" =>	 -- OR
				    EXE_AluControl <= "001";
				when "101010" =>	 -- SLT
				 	EXE_AluControl <= "111";
				when others => 
                    EXE_AluControl <= "000";
            end case;
        when "101" => --ori
            EXE_AluControl <= "001";
        when "100" =>  --andi
            EXE_AluControl <= "000";
        when "111" => --addi (cambiado)
            EXE_AluControl <= "010"; 
   		when "001" =>  --BEQ 
   		    EXE_AluControl <= "110";
   		when "011" => -- lui (nuevo)
   		    EXE_AluControl <= "100";
		when "000" =>  -- MEM 
            EXE_AluControl <= "010";
        when others =>  
            EXE_AluControl <= "000"; 
    end case;   
end process;



SegmentacionEXMEM:process (clock,rst)
	begin
		if (Reset = '1') then
		    EXMEM_BranchAddress <= (others => '0');
            EXMEM_WriteDest <= (others => '0');
            EXMEM_BranchEquals <= '0';
            EXMEM_MemWrite <= '0';
            EXMEM_MemRead <= '0'; 
            EXMEM_MemToReg <= '0';
            EXMEM_RegWrite <= '0';
            EXMEM_Zero <= '0';
            EXMEM_Result <= (others => '0');
            EXMEM_DataReg2 <= (others => '0');
		elsif (rising_edge(clock)) then
		    EXMEM_BranchAddress <= EXE_BranchAddress;
            EXMEM_WriteDest <= EXE_WriteDest;
            EXMEM_BranchEquals <= EXE_BranchEquals;
            EXMEM_MemWrite <= EXE_MemWrite;
            EXMEM_MemRead <= EXE_MemRead; 
            EXMEM_MemToReg <= EXE_MemToReg;
            EXMEM_RegWrite <= EXE_RegWrite;
            EXMEM_Zero <= EXE_Zero;
            EXMEM_Result <=EXE_Result;
            EXMEM_DataReg2 <= EXE_DataReg2;
         end if;
     end process;


--------------Etapa MEM--------------------

MEM_WriteDest <= EXMEM_WriteDest;
MEM_BranchEquals <= EXMEM_BranchEquals;
MEM_MemWrite <= EXMEM_MemWrite;
MEM_MemRead <= EXMEM_MemRead; 
MEM_MemToReg <= EXMEM_MemToReg;
MEM_RegWrite <= EXMEM_RegWrite;
MEM_Zero <= EXMEM_Zero;
MEM_Result <= EXMEM_Result;
MEM_DataReg2 <= EXMEM_DataReg2;
MEM_BranchAddress <= EXMEM_BranchAddress;


D_Addr <= MEM_Result;
D_RdStb <= MEM_MemRead;
D_WrStb <= MEM_MemWrite;
D_DataOut <= MEM_DataReg2;
MEM_DataOut <= D_DataIn;
MEM_PcSrc <= (MEM_BranchEquals and MEM_Zero);

SegmentacionMEMWB:process (clock,rst)
	begin
		if (Reset = '1') then
		    MEMWB_RegWrite <= '0';
		    MEMWB_MemToReg <= '0';
		    MEMWB_DataOut <= (others => '0');
		    MEMWB_Result <= (others => '0');
		    MEMWB_WriteDest <= (others => '0');
		    
		elsif (rising_edge(clock)) then
		    MEMWB_RegWrite <= MEM_RegWrite;
		    MEMWB_MemToReg <= MEM_MemToReg;
		    MEMWB_DataOut <= MEM_DataOut;
		    MEMWB_Result <= MEM_Result;
		    MEMWB_WriteDest <= MEM_WriteDest;
         end if;
     end process;


------------------ETAPA WB---------------------

WB_RegWrite <= MEMWB_RegWrite;
WB_MemToReg <= MEMWB_MemToReg;
WB_DataOut <= MEMWB_DataOut;
WB_Result <= MEMWB_Result;
WB_WriteDest <= MEMWB_WriteDest;
WB_MuxWbResult <= WB_DataOut when (WB_MemToReg = '1') else WB_Result;


 
end processor_arq;
