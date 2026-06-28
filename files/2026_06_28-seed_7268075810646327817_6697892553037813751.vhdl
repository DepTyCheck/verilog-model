-- Seed: 7268075810646327817,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity omppbdq is
  port (kam : linkage std_logic; w : inout boolean_vector(1 downto 0));
end omppbdq;

architecture f of omppbdq is
  
begin
  
end f;

entity orwbclbnxj is
  port (t : in real);
end orwbclbnxj;

library ieee;
use ieee.std_logic_1164.all;

architecture yo of orwbclbnxj is
  signal iekaxuvjl : boolean_vector(1 downto 0);
  signal ntcf : boolean_vector(1 downto 0);
  signal lpstqh : std_logic;
  signal hfhde : boolean_vector(1 downto 0);
  signal vkr : std_logic;
begin
  ihvsxgpjm : entity work.omppbdq
    port map (kam => vkr, w => hfhde);
  e : entity work.omppbdq
    port map (kam => lpstqh, w => ntcf);
  denoazeccb : entity work.omppbdq
    port map (kam => lpstqh, w => iekaxuvjl);
  
  -- Multi-driven assignments
  vkr <= '1';
  vkr <= '-';
end yo;



-- Seed after: 8817096849311326110,6697892553037813751
