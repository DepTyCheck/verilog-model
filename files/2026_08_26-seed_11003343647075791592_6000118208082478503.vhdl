-- Seed: 11003343647075791592,6000118208082478503

entity beufjzcn is
  port (nnhffv : out real; aryvbzogtl : buffer time);
end beufjzcn;

architecture ohfrqphw of beufjzcn is
  
begin
  -- Single-driven assignments
  aryvbzogtl <= aryvbzogtl;
  nnhffv <= nnhffv;
end ohfrqphw;

entity ahknfkgmw is
  port (zhtpto : in time; mbhixp : buffer real);
end ahknfkgmw;

architecture git of ahknfkgmw is
  signal avmofjlxxm : time;
begin
  nrc : entity work.beufjzcn
    port map (nnhffv => mbhixp, aryvbzogtl => avmofjlxxm);
end git;

library ieee;
use ieee.std_logic_1164.all;

entity mpwl is
  port (ioasznaz : inout std_logic_vector(2 to 1); kfjrlu : buffer std_logic; ft : linkage real; m : linkage time);
end mpwl;

architecture dzvajqs of mpwl is
  signal it : real;
  signal c : time;
  signal you : real;
  signal pbwkfhfp : real;
  signal dfbqtxgn : time;
begin
  udd : entity work.ahknfkgmw
    port map (zhtpto => dfbqtxgn, mbhixp => pbwkfhfp);
  xkb : entity work.beufjzcn
    port map (nnhffv => you, aryvbzogtl => c);
  pwgztymlb : entity work.beufjzcn
    port map (nnhffv => it, aryvbzogtl => dfbqtxgn);
  
  -- Multi-driven assignments
  kfjrlu <= kfjrlu;
end dzvajqs;



-- Seed after: 17240798927049556859,6000118208082478503
