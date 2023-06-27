--Autores: Carlos, Santiago
--         Lalanne, Agust�n
--Profesor: Vazquez, Martin
--
--06/2021

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;


entity ControlUnit is
    port(
         ID_Instruct   : in std_logic_vector(5 downto 0); --Primeros 6 bits de la salida de la memoria de instruccion                                                        
         ID_RegWrite : out std_logic;                           
         ID_MemToReg : out std_logic; 
         ID_BranchEquals : out std_logic;   
         ID_MemRead  : out std_logic;     
         ID_MemWrite : out std_logic;     
         ID_RegDst   : out std_logic;   
         ID_AluOp    : out std_logic_vector(2 downto 0);    
         ID_AluSrc   : out std_logic
    );
end ControlUnit;

architecture Behavioral of ControlUnit is

begin
   Control:process(ID_Instruct(5 downto 0))
          begin
               case ID_Instruct(5 downto 0) is
                    when "000000" => -- R-Type
                         ID_RegWrite    <= '1';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '1';
                         ID_AluOp    <= "010";
                         ID_AluSrc   <= '0';
                    when "100011" => --LOAD
                         ID_RegWrite    <= '1';
                         ID_MemToReg  <= '1';
                         ID_BranchEquals  <= '0';
                         ID_MemRead    <= '1';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "000";
                         ID_AluSrc   <= '1';
                    when "101011" => --STORE
                         ID_RegWrite    <= '0';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '1';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "000";
                         ID_AluSrc   <= '1';
                    when "000100" => --BEQ        
                         ID_RegWrite    <= '0';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '1';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "001";
                         ID_AluSrc   <= '0';
                    when "001111" => --LUI       
                         ID_RegWrite    <= '1';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "011";
                         ID_AluSrc   <= '1';
                 ---ahora si
		            when "001000" => --ADDI
                         ID_RegWrite    <= '1';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "111";
                         ID_AluSrc   <= '1';
                         
                    when "001100" => --ANDI
                         ID_RegWrite    <= '1';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "100";
                         ID_AluSrc   <= '1';
                         
                    when "001101" => --ORI
                         ID_RegWrite    <= '1';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "101";
                         ID_AluSrc   <= '1';
                       
                    when others => --OTHERS
                         ID_RegWrite    <= '0';
                         ID_MemToReg  <= '0';
                         ID_BranchEquals  <= '0';
                         ID_MemRead    <= '0';
                         ID_MemWrite   <= '0';
                         ID_RegDst   <= '0';
                         ID_AluOp    <= "000";
                         ID_AluSrc   <= '0';                         
               end case;
          end process;
end Behavioral;
