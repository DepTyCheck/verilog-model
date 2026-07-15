-- Seed: 17895908100621773290,2983771601630957889

library ieee;
use ieee.std_logic_1164.all;

entity zd is
  port (gznxtlaqh : inout real; m : out std_logic_vector(4 downto 1); vdvxoksmz : inout std_logic);
end zd;

architecture cusalabva of zd is
  
begin
  -- Single-driven assignments
  gznxtlaqh <= 0_2_2.3242;
  
  -- Multi-driven assignments
  vdvxoksmz <= 'H';
  m <= m;
  vdvxoksmz <= vdvxoksmz;
end cusalabva;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ur is
  port ( kbeqworux : out std_logic
  ; variable o : inout file_value_mirror_pt
  ; variable exxwdkyvl : inout file_subtype_mirror_pt
  ; kh : inout severity_level
  );
end ur;

library ieee;
use ieee.std_logic_1164.all;

architecture vbxgpjgcty of ur is
  signal ojtbdtpqi : std_logic_vector(4 downto 1);
  signal chd : real;
  signal e : std_logic_vector(4 downto 1);
  signal su : real;
  signal qvgzlr : std_logic_vector(4 downto 1);
  signal wyins : real;
  signal kaahbnblnz : std_logic_vector(4 downto 1);
  signal vqrgbfsvo : real;
begin
  fpgl : entity work.zd
    port map (gznxtlaqh => vqrgbfsvo, m => kaahbnblnz, vdvxoksmz => kbeqworux);
  orbyihkop : entity work.zd
    port map (gznxtlaqh => wyins, m => qvgzlr, vdvxoksmz => kbeqworux);
  cqzpyjq : entity work.zd
    port map (gznxtlaqh => su, m => e, vdvxoksmz => kbeqworux);
  ohqz : entity work.zd
    port map (gznxtlaqh => chd, m => ojtbdtpqi, vdvxoksmz => kbeqworux);
  
  -- Single-driven assignments
  kh <= kh;
  
  -- Multi-driven assignments
  qvgzlr <= "HHXZ";
  kbeqworux <= '1';
  kaahbnblnz <= ('X', 'L', 'U', 'X');
end vbxgpjgcty;



-- Seed after: 11741049032412410748,2983771601630957889
