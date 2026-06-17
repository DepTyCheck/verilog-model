-- Seed: 1397333327532116189,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity qzsvn is
  port (gakhfssyzz : in std_logic_vector(0 downto 3); ijra : out integer; wdtbzburl : buffer time);
end qzsvn;

architecture i of qzsvn is
  
begin
  -- Single-driven assignments
  wdtbzburl <= 8#7.6_2_0# us;
end i;

entity asgk is
  port (gfynhcd : in integer; dhrwgb : linkage bit_vector(1 downto 2); clmjkcbe : out integer);
end asgk;

library ieee;
use ieee.std_logic_1164.all;

architecture eda of asgk is
  signal ghkxjk : time;
  signal ez : time;
  signal gjla : integer;
  signal palrgs : time;
  signal pkej : integer;
  signal re : std_logic_vector(0 downto 3);
begin
  ev : entity work.qzsvn
    port map (gakhfssyzz => re, ijra => pkej, wdtbzburl => palrgs);
  ffst : entity work.qzsvn
    port map (gakhfssyzz => re, ijra => gjla, wdtbzburl => ez);
  fie : entity work.qzsvn
    port map (gakhfssyzz => re, ijra => clmjkcbe, wdtbzburl => ghkxjk);
  
  -- Multi-driven assignments
  re <= (others => '0');
  re <= (others => '0');
end eda;



-- Seed after: 7072553943660018853,10557070023141912087
