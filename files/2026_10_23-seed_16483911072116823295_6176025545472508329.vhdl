-- Seed: 16483911072116823295,6176025545472508329

entity m is
  port (i : linkage bit; setcnmwk : buffer bit; wihg : buffer real_vector(4 to 2));
end m;

architecture ic of m is
  
begin
  -- Single-driven assignments
  wihg <= (others => 0.0);
end ic;

library ieee;
use ieee.std_logic_1164.all;

entity hirdfkl is
  port (u : in std_logic_vector(3 to 0); gjvcnu : in std_logic_vector(2 to 3); so : out std_logic_vector(0 downto 4));
end hirdfkl;

architecture pcx of hirdfkl is
  
begin
  -- Multi-driven assignments
  so <= so;
  so <= (others => '0');
end pcx;

library ieee;
use ieee.std_logic_1164.all;

entity zk is
  port (hzzkig : in integer; npq : in std_logic_vector(1 to 0); jerkxh : buffer real; ghurlxihds : in time);
end zk;

library ieee;
use ieee.std_logic_1164.all;

architecture yyojnxw of zk is
  signal pg : std_logic_vector(0 downto 4);
  signal mzepii : std_logic_vector(2 to 3);
  signal amhpxb : std_logic_vector(0 downto 4);
  signal ozqypn : std_logic_vector(2 to 3);
  signal gqi : real_vector(4 to 2);
  signal vyoukmia : bit;
  signal rivdywkh : bit;
  signal brlcjz : real_vector(4 to 2);
  signal ulw : bit;
  signal zyuvvl : bit;
begin
  pcvr : entity work.m
    port map (i => zyuvvl, setcnmwk => ulw, wihg => brlcjz);
  zb : entity work.m
    port map (i => rivdywkh, setcnmwk => vyoukmia, wihg => gqi);
  bwfcbfhtg : entity work.hirdfkl
    port map (u => npq, gjvcnu => ozqypn, so => amhpxb);
  ildhrqrqb : entity work.hirdfkl
    port map (u => npq, gjvcnu => mzepii, so => pg);
  
  -- Single-driven assignments
  jerkxh <= jerkxh;
end yyojnxw;



-- Seed after: 1838861269269705783,6176025545472508329
