-- Seed: 11646807629622308309,5906004015519833893

entity nbhwmg is
  port (zxumpdavfc : buffer integer; ogga : linkage real; yoclnbcajc : inout real);
end nbhwmg;

architecture tiomihjyq of nbhwmg is
  
begin
  -- Single-driven assignments
  zxumpdavfc <= zxumpdavfc;
  yoclnbcajc <= yoclnbcajc;
end tiomihjyq;

entity hfxx is
  port (wjo : linkage real);
end hfxx;

architecture g of hfxx is
  signal jlvhdpuedx : real;
  signal o : integer;
  signal ircrhnf : real;
  signal z : real;
  signal t : integer;
  signal idqdes : real;
  signal jdifshm : real;
  signal shxspel : integer;
begin
  mfwj : entity work.nbhwmg
    port map (zxumpdavfc => shxspel, ogga => jdifshm, yoclnbcajc => idqdes);
  jrk : entity work.nbhwmg
    port map (zxumpdavfc => t, ogga => z, yoclnbcajc => ircrhnf);
  ibuu : entity work.nbhwmg
    port map (zxumpdavfc => o, ogga => wjo, yoclnbcajc => jlvhdpuedx);
end g;

library ieee;
use ieee.std_logic_1164.all;

entity tcgpzxy is
  port (zjwfsjj : inout std_logic_vector(4 to 2); mgchp : linkage integer);
end tcgpzxy;

architecture zb of tcgpzxy is
  signal joupvbjz : real;
begin
  dgayit : entity work.hfxx
    port map (wjo => joupvbjz);
  
  -- Multi-driven assignments
  zjwfsjj <= (others => '0');
  zjwfsjj <= zjwfsjj;
end zb;



-- Seed after: 17478998738877972925,5906004015519833893
