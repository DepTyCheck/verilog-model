-- Seed: 17600081155330153179,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity abu is
  port (lobtjqam : buffer std_logic_vector(2 downto 3); rwwaein : buffer integer; gxcnwxe : in bit_vector(3 downto 2));
end abu;

architecture fnaigk of abu is
  
begin
  -- Single-driven assignments
  rwwaein <= 16#D#;
  
  -- Multi-driven assignments
  lobtjqam <= (others => '0');
end fnaigk;

library ieee;
use ieee.std_logic_1164.all;

entity fenuxqdze is
  port (zh : inout real_vector(2 to 0); o : linkage std_logic_vector(4 to 4); tcx : linkage time);
end fenuxqdze;

architecture djo of fenuxqdze is
  
begin
  -- Single-driven assignments
  zh <= (others => 0.0);
end djo;

library ieee;
use ieee.std_logic_1164.all;

entity yuu is
  port (krtjrjtg : buffer std_logic; pmiwi : inout real; kcdcxgdg : in time);
end yuu;

library ieee;
use ieee.std_logic_1164.all;

architecture ksmkuydv of yuu is
  signal xjidkbpu : integer;
  signal bfjjqevmf : std_logic_vector(2 downto 3);
  signal ovbhbgrvi : bit_vector(3 downto 2);
  signal mqgpu : integer;
  signal kn : std_logic_vector(2 downto 3);
begin
  d : entity work.abu
    port map (lobtjqam => kn, rwwaein => mqgpu, gxcnwxe => ovbhbgrvi);
  dzg : entity work.abu
    port map (lobtjqam => bfjjqevmf, rwwaein => xjidkbpu, gxcnwxe => ovbhbgrvi);
  
  -- Single-driven assignments
  pmiwi <= 8#37660.7_3#;
  ovbhbgrvi <= ('1', '1');
  
  -- Multi-driven assignments
  kn <= (others => '0');
end ksmkuydv;



-- Seed after: 4823095123172036674,6697892553037813751
