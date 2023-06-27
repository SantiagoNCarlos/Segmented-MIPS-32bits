--Autores: Carlos, Santiago
--         Lalanne, Agustín
--Profesor: Vazquez, Martin
--
--06/2021
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_signed.all;

entity ALU is -- Declaracion de la ALU
    Port ( a : in STD_LOGIC_VECTOR (31 downto 0);
           b : in STD_LOGIC_VECTOR (31 downto 0);
           sel : in STD_LOGIC_VECTOR (2 downto 0);
           result : out STD_LOGIC_VECTOR (31 downto 0);
           zero : out STD_LOGIC);
end ALU;

architecture Behavioral of ALU is

    signal aux: std_logic_vector(31 downto 0); -- Usamos una variable auxiliar porque no podemos leer una salida.
begin

 ALU:process(a, b, sel) 
 begin 
    case(sel) is 
        when "000" => aux <= a and b; 
        when "001" => aux <= a or b;
        when "010" => aux <= a+b;
        when "100" => aux <= b(15 downto 0) & x"0000";
        when "110" => aux <= a-b;
        when "111" => 
            if (a < b) then 
                aux <= "00000000000000000000000000000001"; --ACA CUANDO
            else
                aux <= "00000000000000000000000000000000";
            end if;
        when others => aux <=  x"00000000";
    end case;
 end process;
 
 result <= aux; -- Asignacion de resultado. 

 zero1:process (aux)
    begin
        if (aux = x"00000000") then
            zero <= '1';
        else
            zero<='0';
        end if;
    end process;
 
end Behavioral;
