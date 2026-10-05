-- Seed: 16757134042635378152,7304262412290825129

entity pimcbeunu is
  port (r : in time);
end pimcbeunu;

architecture gzvvq of pimcbeunu is
  
begin
  
end gzvvq;

entity khcph is
  port (fdo : out bit_vector(0 to 2); dddalioq : inout time);
end khcph;

architecture epac of khcph is
  signal uvznfzdi : time;
  signal gjpgoxs : time;
  signal hakkdha : time;
begin
  gedwyrcvs : entity work.pimcbeunu
    port map (r => hakkdha);
  mhbyqwnzd : entity work.pimcbeunu
    port map (r => gjpgoxs);
  mlt : entity work.pimcbeunu
    port map (r => uvznfzdi);
  
  -- Single-driven assignments
  dddalioq <= uvznfzdi;
end epac;

library ieee;
use ieee.std_logic_1164.all;

entity eampbrg is
  port (h : in std_logic; gevre : linkage bit; fdcbnvln : inout time);
end eampbrg;

architecture hdt of eampbrg is
  signal jpl : bit_vector(0 to 2);
begin
  jfvlp : entity work.khcph
    port map (fdo => jpl, dddalioq => fdcbnvln);
  z : entity work.pimcbeunu
    port map (r => fdcbnvln);
end hdt;



-- Seed after: 10001830751799577582,7304262412290825129
