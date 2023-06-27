--Autores: Carlos, Santiago
--         Lalanne, Agust�n
--Profesor: Vazquez, Martin
--
--06/2021
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.all;


entity Registers is
    Port ( clk : in STD_LOGIC;
           reset : in STD_LOGIC;
           wr : in STD_LOGIC;
           reg1_rd : in STD_LOGIC_VECTOR (4 downto 0);
           reg2_rd : in STD_LOGIC_VECTOR (4 downto 0);
           reg_wr : in STD_LOGIC_VECTOR (4 downto 0);
           data_wr : in STD_LOGIC_VECTOR (31 downto 0);
           data1_rd : out STD_LOGIC_VECTOR (31 downto 0);
           data2_rd : out STD_LOGIC_VECTOR (31 downto 0));

end Registers;

architecture Behavioral of Registers is

type Reg_type is array (0 to 31) of std_logic_vector(31 downto 0);
signal reg: Reg_type;
begin

    reset_escritura: process (clk, reset)
    begin
        if (reset='1') then
            reg <= (others => x"00000000");
        else if (falling_edge(clk) and wr='1' and (reg_wr /= "00000")) then
            reg(to_integer(unsigned(reg_wr))) <= data_wr;
            end if;
        end if;
    end process;

    lectura: process (reg1_rd, reg2_rd)
    begin
        data1_rd <= reg(to_integer(unsigned(reg1_rd)));
        data2_rd <= reg(to_integer(unsigned(reg2_rd)));       
    end process;


end Behavioral;