-- Seed: 375002681770220354,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity eqctr is
  port (vcvcgjt : out std_logic_vector(3 downto 3));
end eqctr;

architecture hxjuab of eqctr is
  
begin
  
end hxjuab;

entity o is
  port (xzccpdjo : linkage integer; lylhymt : inout boolean);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture ujn of o is
  signal lhmqgm : std_logic_vector(3 downto 3);
begin
  f : entity work.eqctr
    port map (vcvcgjt => lhmqgm);
  ewmzmkce : entity work.eqctr
    port map (vcvcgjt => lhmqgm);
  
  -- Single-driven assignments
  lylhymt <= TRUE;
  
  -- Multi-driven assignments
  lhmqgm <= (others => 'H');
  lhmqgm <= lhmqgm;
  lhmqgm <= lhmqgm;
  lhmqgm <= "X";
end ujn;



-- Seed after: 9319675396456701186,18037650846010261179
