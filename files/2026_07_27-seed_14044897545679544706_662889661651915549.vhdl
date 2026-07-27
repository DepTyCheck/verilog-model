-- Seed: 14044897545679544706,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity tja is
  port (hds : buffer std_logic; tzkviupbi : linkage integer; mm : inout integer; bmjbwzux : buffer std_logic_vector(2 downto 0));
end tja;

architecture rg of tja is
  
begin
  -- Single-driven assignments
  mm <= 2#011#;
  
  -- Multi-driven assignments
  hds <= hds;
  bmjbwzux <= bmjbwzux;
end rg;

entity pngh is
  port (sfma : out time_vector(1 to 2));
end pngh;

library ieee;
use ieee.std_logic_1164.all;

architecture iibkuxb of pngh is
  signal tumrb : integer;
  signal ejjzs : integer;
  signal rkczymnebh : std_logic_vector(2 downto 0);
  signal iysm : integer;
  signal fs : integer;
  signal umwx : std_logic;
  signal hvj : std_logic_vector(2 downto 0);
  signal iyfmrvv : integer;
  signal fqbixj : integer;
  signal sxow : std_logic;
begin
  zvhn : entity work.tja
    port map (hds => sxow, tzkviupbi => fqbixj, mm => iyfmrvv, bmjbwzux => hvj);
  ggfh : entity work.tja
    port map (hds => umwx, tzkviupbi => fs, mm => iysm, bmjbwzux => rkczymnebh);
  jvs : entity work.tja
    port map (hds => sxow, tzkviupbi => ejjzs, mm => tumrb, bmjbwzux => hvj);
  
  -- Single-driven assignments
  sfma <= sfma;
  
  -- Multi-driven assignments
  rkczymnebh <= ('U', 'X', '1');
  umwx <= 'X';
end iibkuxb;



-- Seed after: 4149773859332828327,662889661651915549
