-- Seed: 11562450377289506819,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity de is
  port (cm : out std_logic);
end de;

architecture bllav of de is
  
begin
  -- Multi-driven assignments
  cm <= 'L';
  cm <= cm;
  cm <= cm;
end bllav;

entity ue is
  port (jocvib : buffer real);
end ue;

library ieee;
use ieee.std_logic_1164.all;

architecture diauxv of ue is
  signal vdfkprgwg : std_logic;
  signal hjwizlank : std_logic;
begin
  zbsihow : entity work.de
    port map (cm => hjwizlank);
  z : entity work.de
    port map (cm => vdfkprgwg);
  nhu : entity work.de
    port map (cm => vdfkprgwg);
  
  -- Single-driven assignments
  jocvib <= jocvib;
  
  -- Multi-driven assignments
  hjwizlank <= hjwizlank;
  hjwizlank <= hjwizlank;
  hjwizlank <= 'H';
end diauxv;

entity smkts is
  port (rhzewad : inout bit);
end smkts;

library ieee;
use ieee.std_logic_1164.all;

architecture h of smkts is
  signal byg : std_logic;
begin
  zetzrnst : entity work.de
    port map (cm => byg);
  taqlalr : entity work.de
    port map (cm => byg);
end h;



-- Seed after: 10947927860300594292,10594830431004325987
