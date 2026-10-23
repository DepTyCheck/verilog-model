-- Seed: 175270814852739071,6176025545472508329

entity kywb is
  port (wtd : out real_vector(4 to 3));
end kywb;

architecture y of kywb is
  
begin
  -- Single-driven assignments
  wtd <= wtd;
end y;

library ieee;
use ieee.std_logic_1164.all;

entity ob is
  port (ytcgxjdfn : out time; mzastp : in std_logic; po : inout real);
end ob;

architecture vnh of ob is
  signal kqxbdn : real_vector(4 to 3);
  signal rsfeaxzxx : real_vector(4 to 3);
begin
  lmic : entity work.kywb
    port map (wtd => rsfeaxzxx);
  m : entity work.kywb
    port map (wtd => kqxbdn);
  
  -- Single-driven assignments
  po <= 1_1_1_3_0.2;
  ytcgxjdfn <= 8#3.5# ps;
end vnh;



-- Seed after: 6461529987328072607,6176025545472508329
