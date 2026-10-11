-- Seed: 7250320257676834031,9099465110516446487

entity mckgcws is
  port (uiyvhenrgg : out real; ddcwmh : buffer integer_vector(2 downto 0); ofkfvzyddg : inout boolean_vector(0 to 4));
end mckgcws;

architecture mgdg of mckgcws is
  
begin
  -- Single-driven assignments
  ddcwmh <= (2#0_0_1#, 8#7_7_6_5_0#, 1);
  uiyvhenrgg <= 4_3_4.204;
  ofkfvzyddg <= ofkfvzyddg;
end mgdg;

library ieee;
use ieee.std_logic_1164.all;

entity zu is
  port (tniknc : buffer character; echplmpmw : buffer real; jpgpejw : in time_vector(3 downto 3); qwep : inout std_logic_vector(3 to 3));
end zu;

architecture onkwkuqtc of zu is
  signal ikmuze : boolean_vector(0 to 4);
  signal nm : integer_vector(2 downto 0);
  signal cbtgr : boolean_vector(0 to 4);
  signal giq : integer_vector(2 downto 0);
  signal qnyvfn : real;
begin
  grhzl : entity work.mckgcws
    port map (uiyvhenrgg => qnyvfn, ddcwmh => giq, ofkfvzyddg => cbtgr);
  ft : entity work.mckgcws
    port map (uiyvhenrgg => echplmpmw, ddcwmh => nm, ofkfvzyddg => ikmuze);
  
  -- Single-driven assignments
  tniknc <= 'p';
  
  -- Multi-driven assignments
  qwep <= "H";
  qwep <= qwep;
end onkwkuqtc;



-- Seed after: 13827892951500717310,9099465110516446487
