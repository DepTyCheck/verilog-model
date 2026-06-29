-- Seed: 1161977105756130081,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity mcksojts is
  port (bspftnhky : inout integer; ngjfybgt : inout std_logic; zdomibk : out real);
end mcksojts;

architecture ob of mcksojts is
  
begin
  -- Single-driven assignments
  bspftnhky <= 2#1_1_1_0#;
  zdomibk <= 444.343;
  
  -- Multi-driven assignments
  ngjfybgt <= '1';
  ngjfybgt <= 'Z';
  ngjfybgt <= '1';
  ngjfybgt <= 'Z';
end ob;

entity xwb is
  port (zpubc : buffer time; c : inout integer);
end xwb;

library ieee;
use ieee.std_logic_1164.all;

architecture uthbr of xwb is
  signal l : real;
  signal kmr : std_logic;
begin
  qsyy : entity work.mcksojts
    port map (bspftnhky => c, ngjfybgt => kmr, zdomibk => l);
  
  -- Single-driven assignments
  zpubc <= 2022.1_4 ns;
  
  -- Multi-driven assignments
  kmr <= '1';
  kmr <= '1';
end uthbr;

library ieee;
use ieee.std_logic_1164.all;

entity pi is
  port (eoghcc : in std_logic_vector(4 downto 1); bly : linkage boolean; uwkgquolna : linkage time);
end pi;

library ieee;
use ieee.std_logic_1164.all;

architecture rcxrltwllh of pi is
  signal uimhtdp : integer;
  signal trqiduho : time;
  signal bgfqrsct : integer;
  signal gju : time;
  signal wwkte : integer;
  signal j : time;
  signal gpgsdfmgm : real;
  signal mo : std_logic;
  signal ntk : integer;
begin
  adhdlxams : entity work.mcksojts
    port map (bspftnhky => ntk, ngjfybgt => mo, zdomibk => gpgsdfmgm);
  kvayzeacho : entity work.xwb
    port map (zpubc => j, c => wwkte);
  cfecfptxt : entity work.xwb
    port map (zpubc => gju, c => bgfqrsct);
  gceomgfxiz : entity work.xwb
    port map (zpubc => trqiduho, c => uimhtdp);
  
  -- Multi-driven assignments
  mo <= '0';
  mo <= 'H';
  mo <= '1';
end rcxrltwllh;



-- Seed after: 8295404306907128038,17047277710231705797
