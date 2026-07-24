-- Seed: 14181063078864991834,16461708287571398341

entity iql is
  port (gapxb : inout real; rmuazipitm : in time);
end iql;

architecture a of iql is
  
begin
  -- Single-driven assignments
  gapxb <= gapxb;
end a;

entity m is
  port (ou : in time);
end m;

architecture nhbyjvr of m is
  signal nbflio : time;
  signal ektdanmz : real;
  signal orourfsyvd : time;
  signal snzx : real;
begin
  sfpurbcr : entity work.iql
    port map (gapxb => snzx, rmuazipitm => orourfsyvd);
  jlpxxgoj : entity work.iql
    port map (gapxb => ektdanmz, rmuazipitm => nbflio);
  
  -- Single-driven assignments
  orourfsyvd <= orourfsyvd;
  nbflio <= ou;
end nhbyjvr;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (rdxxj : inout time; l : in std_logic_vector(2 to 4); oqvmt : in std_logic_vector(4 downto 0); mdbsieg : in string(4 downto 5));
end t;

architecture qtydcff of t is
  signal wnrtaidt : time;
begin
  srpmggsca : entity work.m
    port map (ou => rdxxj);
  n : entity work.m
    port map (ou => wnrtaidt);
  
  -- Single-driven assignments
  rdxxj <= 1 us;
  wnrtaidt <= 2 sec;
end qtydcff;

library ieee;
use ieee.std_logic_1164.all;

entity bralyyu is
  port (rtxlmgcpbv : linkage integer; pbcz : inout std_logic_vector(1 to 1));
end bralyyu;

architecture axurm of bralyyu is
  
begin
  -- Multi-driven assignments
  pbcz <= "Z";
  pbcz <= (others => 'H');
  pbcz <= (others => '0');
end axurm;



-- Seed after: 594905084492036102,16461708287571398341
