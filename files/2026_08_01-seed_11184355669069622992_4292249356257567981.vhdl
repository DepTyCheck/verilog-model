-- Seed: 11184355669069622992,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity izvjnbv is
  port (bxxqpxoq : in integer_vector(3 downto 4); fs : linkage std_logic; ooce : in real_vector(4 to 3));
end izvjnbv;

architecture y of izvjnbv is
  
begin
  
end y;

entity oj is
  port (btwkqkgadv : out integer);
end oj;

library ieee;
use ieee.std_logic_1164.all;

architecture mxzq of oj is
  signal dunkioxdkv : real_vector(4 to 3);
  signal a : real_vector(4 to 3);
  signal npjgj : std_logic;
  signal gnwzhaqtgc : integer_vector(3 downto 4);
begin
  xcrmaxqcql : entity work.izvjnbv
    port map (bxxqpxoq => gnwzhaqtgc, fs => npjgj, ooce => a);
  hhzjxw : entity work.izvjnbv
    port map (bxxqpxoq => gnwzhaqtgc, fs => npjgj, ooce => dunkioxdkv);
  
  -- Single-driven assignments
  a <= (others => 0.0);
  gnwzhaqtgc <= gnwzhaqtgc;
  btwkqkgadv <= 2#10#;
  dunkioxdkv <= dunkioxdkv;
  
  -- Multi-driven assignments
  npjgj <= npjgj;
  npjgj <= npjgj;
  npjgj <= npjgj;
  npjgj <= '1';
end mxzq;



-- Seed after: 10269023351627622404,4292249356257567981
