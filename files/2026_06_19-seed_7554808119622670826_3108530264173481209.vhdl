-- Seed: 7554808119622670826,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity uemnqot is
  port (lhkvjhor : buffer std_logic_vector(1 downto 4); no : linkage std_logic_vector(2 downto 2));
end uemnqot;

architecture hzqxex of uemnqot is
  
begin
  -- Multi-driven assignments
  lhkvjhor <= "";
  lhkvjhor <= (others => '0');
  lhkvjhor <= "";
  lhkvjhor <= (others => '0');
end hzqxex;

library ieee;
use ieee.std_logic_1164.all;

entity yyo is
  port (etmjup : in std_logic; xkqhje : buffer integer_vector(1 downto 0); txvz : buffer real_vector(3 to 4); tfytzmii : out integer);
end yyo;

library ieee;
use ieee.std_logic_1164.all;

architecture oo of yyo is
  signal usqe : std_logic_vector(1 downto 4);
  signal dd : std_logic_vector(2 downto 2);
  signal obqmh : std_logic_vector(2 downto 2);
  signal dvp : std_logic_vector(1 downto 4);
begin
  xxnqgipyh : entity work.uemnqot
    port map (lhkvjhor => dvp, no => obqmh);
  zijtlk : entity work.uemnqot
    port map (lhkvjhor => dvp, no => dd);
  nec : entity work.uemnqot
    port map (lhkvjhor => usqe, no => dd);
  
  -- Single-driven assignments
  tfytzmii <= 2#1#;
  
  -- Multi-driven assignments
  obqmh <= "H";
  dd <= "U";
  dvp <= (others => '0');
  dvp <= (others => '0');
end oo;



-- Seed after: 4853916794828127076,3108530264173481209
