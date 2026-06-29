-- Seed: 16133558407591258099,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity ay is
  port (wmdzhx : in real_vector(0 downto 1); b : linkage std_logic_vector(1 to 3));
end ay;

architecture srck of ay is
  
begin
  
end srck;

entity gwtkgctxq is
  port (vlfh : inout time; craxxx : linkage integer);
end gwtkgctxq;

library ieee;
use ieee.std_logic_1164.all;

architecture hryoubukiu of gwtkgctxq is
  signal vbkptls : std_logic_vector(1 to 3);
  signal g : real_vector(0 downto 1);
begin
  kgeybqe : entity work.ay
    port map (wmdzhx => g, b => vbkptls);
  
  -- Single-driven assignments
  g <= (others => 0.0);
  vlfh <= 2#1110.0_1_0_1# ns;
  
  -- Multi-driven assignments
  vbkptls <= "W1X";
  vbkptls <= ('X', '0', '-');
end hryoubukiu;



-- Seed after: 13859408708616106956,17047277710231705797
