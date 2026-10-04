-- Seed: 11756729628492274799,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity lubtnm is
  port (q : out bit_vector(0 to 2); ugexlo : buffer bit; lhnegf : buffer std_logic_vector(3 to 0));
end lubtnm;

architecture smaubrbyo of lubtnm is
  
begin
  -- Single-driven assignments
  ugexlo <= '1';
  
  -- Multi-driven assignments
  lhnegf <= lhnegf;
  lhnegf <= "";
  lhnegf <= lhnegf;
  lhnegf <= lhnegf;
end smaubrbyo;

library ieee;
use ieee.std_logic_1164.all;

entity wnzdemju is
  port (twpb : inout std_logic);
end wnzdemju;

library ieee;
use ieee.std_logic_1164.all;

architecture dnsq of wnzdemju is
  signal ppwejaba : std_logic_vector(3 to 0);
  signal nqbcrm : bit;
  signal wsor : bit_vector(0 to 2);
begin
  jyo : entity work.lubtnm
    port map (q => wsor, ugexlo => nqbcrm, lhnegf => ppwejaba);
  
  -- Multi-driven assignments
  twpb <= twpb;
end dnsq;

entity pvok is
  port (ss : linkage boolean);
end pvok;

library ieee;
use ieee.std_logic_1164.all;

architecture fhjwdb of pvok is
  signal cwzpnm : std_logic_vector(3 to 0);
  signal arev : bit;
  signal thy : bit_vector(0 to 2);
  signal oggydvwi : std_logic_vector(3 to 0);
  signal m : bit;
  signal zhi : bit_vector(0 to 2);
begin
  c : entity work.lubtnm
    port map (q => zhi, ugexlo => m, lhnegf => oggydvwi);
  h : entity work.lubtnm
    port map (q => thy, ugexlo => arev, lhnegf => cwzpnm);
  
  -- Multi-driven assignments
  oggydvwi <= (others => '0');
end fhjwdb;



-- Seed after: 669611743441655599,15795020531041709203
