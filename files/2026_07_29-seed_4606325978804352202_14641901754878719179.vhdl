-- Seed: 4606325978804352202,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity peitanqaj is
  port (auyg : inout real; tfrki : out std_logic_vector(0 to 3); ggzwkch : buffer std_logic_vector(1 to 0); gepfi : in bit);
end peitanqaj;

architecture cigcyy of peitanqaj is
  
begin
  -- Single-driven assignments
  auyg <= auyg;
  
  -- Multi-driven assignments
  ggzwkch <= "";
  tfrki <= tfrki;
  ggzwkch <= (others => '0');
end cigcyy;

library ieee;
use ieee.std_logic_1164.all;

entity jufopsz is
  port (qrbxoodhr : in std_logic_vector(4 downto 4); zfbghbgff : buffer time_vector(0 to 3));
end jufopsz;

library ieee;
use ieee.std_logic_1164.all;

architecture kjfco of jufopsz is
  signal t : bit;
  signal gnhzzjby : std_logic_vector(1 to 0);
  signal gtlva : std_logic_vector(0 to 3);
  signal nhcafgd : real;
  signal tkhsa : bit;
  signal lvgrxkxzjx : std_logic_vector(1 to 0);
  signal bovcrrwoav : std_logic_vector(0 to 3);
  signal cnms : real;
begin
  tmwhjfqx : entity work.peitanqaj
    port map (auyg => cnms, tfrki => bovcrrwoav, ggzwkch => lvgrxkxzjx, gepfi => tkhsa);
  zinnmwrx : entity work.peitanqaj
    port map (auyg => nhcafgd, tfrki => gtlva, ggzwkch => gnhzzjby, gepfi => t);
  
  -- Single-driven assignments
  zfbghbgff <= (3 min, 0 min, 8#6_5# ns, 44 ps);
  t <= tkhsa;
  tkhsa <= '1';
  
  -- Multi-driven assignments
  bovcrrwoav <= ('1', 'X', '0', '1');
end kjfco;



-- Seed after: 14676144612748328437,14641901754878719179
