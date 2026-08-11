-- Seed: 677368510395605933,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity px is
  port (spwv : linkage std_logic_vector(4 downto 1); jszav : inout std_logic; smgq : out time);
end px;

architecture ppz of px is
  
begin
  -- Multi-driven assignments
  jszav <= 'W';
  jszav <= '-';
end ppz;

entity vt is
  port (gcfenhxh : buffer boolean_vector(4 downto 0));
end vt;

library ieee;
use ieee.std_logic_1164.all;

architecture mhf of vt is
  signal bbsurwknh : time;
  signal q : std_logic;
  signal bnx : std_logic_vector(4 downto 1);
  signal tal : time;
  signal gr : time;
  signal h : std_logic;
  signal ysqhxkmk : std_logic_vector(4 downto 1);
  signal ts : time;
  signal graosgrln : std_logic;
  signal avvweabe : std_logic_vector(4 downto 1);
begin
  znoszwwrer : entity work.px
    port map (spwv => avvweabe, jszav => graosgrln, smgq => ts);
  vxajvme : entity work.px
    port map (spwv => ysqhxkmk, jszav => h, smgq => gr);
  mldrav : entity work.px
    port map (spwv => avvweabe, jszav => graosgrln, smgq => tal);
  lzwskulfb : entity work.px
    port map (spwv => bnx, jszav => q, smgq => bbsurwknh);
  
  -- Single-driven assignments
  gcfenhxh <= (TRUE, FALSE, TRUE, TRUE, TRUE);
  
  -- Multi-driven assignments
  avvweabe <= avvweabe;
  avvweabe <= ysqhxkmk;
  bnx <= ('X', '1', 'W', 'X');
  graosgrln <= '0';
end mhf;



-- Seed after: 6518997345851163022,10594830431004325987
