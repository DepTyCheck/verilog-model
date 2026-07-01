-- Seed: 2269207516556339530,6882842853887419669

entity vwzjdje is
  port (weuyy : linkage string(1 downto 5); zghlokhynb : in bit_vector(2 downto 4); dibyhh : inout time);
end vwzjdje;

architecture zt of vwzjdje is
  
begin
  
end zt;

library ieee;
use ieee.std_logic_1164.all;

entity kkq is
  port (v : out time; zlnv : inout integer; dzfbnjidi : in integer; fmlal : out std_logic_vector(3 downto 4));
end kkq;

architecture tp of kkq is
  signal pjsz : bit_vector(2 downto 4);
  signal mlyhlu : string(1 downto 5);
  signal reokppzlxl : time;
  signal lnst : string(1 downto 5);
  signal t : time;
  signal lkhi : bit_vector(2 downto 4);
  signal yaetmabtgj : string(1 downto 5);
begin
  jvuj : entity work.vwzjdje
    port map (weuyy => yaetmabtgj, zghlokhynb => lkhi, dibyhh => t);
  pxcjyrhvr : entity work.vwzjdje
    port map (weuyy => lnst, zghlokhynb => lkhi, dibyhh => reokppzlxl);
  ugefjp : entity work.vwzjdje
    port map (weuyy => mlyhlu, zghlokhynb => pjsz, dibyhh => v);
  
  -- Single-driven assignments
  zlnv <= 2#111#;
  lkhi <= (others => '0');
  pjsz <= (others => '0');
  
  -- Multi-driven assignments
  fmlal <= (others => '0');
  fmlal <= "";
  fmlal <= (others => '0');
end tp;



-- Seed after: 15393250273799383778,6882842853887419669
