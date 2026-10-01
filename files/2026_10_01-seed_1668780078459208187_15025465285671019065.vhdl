-- Seed: 1668780078459208187,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity rjsrsfswz is
  port (zqyuqj : buffer std_logic_vector(3 to 3));
end rjsrsfswz;

architecture rrobhawxe of rjsrsfswz is
  
begin
  -- Multi-driven assignments
  zqyuqj <= zqyuqj;
end rrobhawxe;

entity h is
  port (hzejnkywz : linkage boolean_vector(1 to 4); duqmfbrq : linkage integer);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture pffazabib of h is
  signal vnpufff : std_logic_vector(3 to 3);
  signal ufa : std_logic_vector(3 to 3);
begin
  mmlsqwy : entity work.rjsrsfswz
    port map (zqyuqj => ufa);
  bof : entity work.rjsrsfswz
    port map (zqyuqj => vnpufff);
  gbjbtbw : entity work.rjsrsfswz
    port map (zqyuqj => ufa);
end pffazabib;

entity l is
  port (rpmtqeh : out bit);
end l;

architecture g of l is
  signal ntytqbn : integer;
  signal qtkvurfh : boolean_vector(1 to 4);
begin
  lfscwrn : entity work.h
    port map (hzejnkywz => qtkvurfh, duqmfbrq => ntytqbn);
  
  -- Single-driven assignments
  rpmtqeh <= '0';
end g;



-- Seed after: 10629849348775795032,15025465285671019065
