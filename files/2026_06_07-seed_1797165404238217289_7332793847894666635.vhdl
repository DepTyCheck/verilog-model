-- Seed: 1797165404238217289,7332793847894666635



entity flhyltzta is
  port (fsvwb : buffer real_vector(4 downto 4));
end flhyltzta;



architecture ddshbneh of flhyltzta is
  
begin
  
end ddshbneh;



entity tkupbmtfka is
  port (siyy : linkage real; c : linkage real; ffjvnl : in integer_vector(0 to 2));
end tkupbmtfka;



architecture mwjipfwsds of tkupbmtfka is
  signal zfqqv : real_vector(4 downto 4);
begin
  mhfduo : entity work.flhyltzta
    port map (fsvwb => zfqqv);
end mwjipfwsds;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (n : buffer std_logic_vector(0 to 3));
end x;



architecture rptdcis of x is
  signal tzkxvm : real_vector(4 downto 4);
  signal cqrr : integer_vector(0 to 2);
  signal pa : real;
begin
  zvnzq : entity work.tkupbmtfka
    port map (siyy => pa, c => pa, ffjvnl => cqrr);
  otszwpmou : entity work.flhyltzta
    port map (fsvwb => tzkxvm);
end rptdcis;



-- Seed after: 14803124460394154157,7332793847894666635
