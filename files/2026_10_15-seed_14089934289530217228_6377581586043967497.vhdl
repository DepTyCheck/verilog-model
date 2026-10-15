-- Seed: 14089934289530217228,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity vwqexzq is
  port (vt : in std_logic_vector(2 downto 1));
end vwqexzq;

architecture eym of vwqexzq is
  
begin
  
end eym;

library ieee;
use ieee.std_logic_1164.all;

entity ibzqggxvuh is
  port (bynpvgsyie : in std_logic_vector(4 to 3));
end ibzqggxvuh;

library ieee;
use ieee.std_logic_1164.all;

architecture s of ibzqggxvuh is
  signal dldpp : std_logic_vector(2 downto 1);
begin
  cdazpwd : entity work.vwqexzq
    port map (vt => dldpp);
  jxhzqt : entity work.vwqexzq
    port map (vt => dldpp);
end s;

library ieee;
use ieee.std_logic_1164.all;

entity jf is
  port (seb : in std_logic_vector(2 downto 1); exwt : out integer_vector(4 to 0); idwvupjz : buffer bit; aacxv : in time);
end jf;

library ieee;
use ieee.std_logic_1164.all;

architecture roj of jf is
  signal noruxhzsum : std_logic_vector(2 downto 1);
  signal fzxmeepm : std_logic_vector(2 downto 1);
  signal mmeic : std_logic_vector(4 to 3);
begin
  tggfhlo : entity work.ibzqggxvuh
    port map (bynpvgsyie => mmeic);
  iduqvtmrq : entity work.vwqexzq
    port map (vt => seb);
  tqqfbjdvdr : entity work.vwqexzq
    port map (vt => fzxmeepm);
  ld : entity work.vwqexzq
    port map (vt => noruxhzsum);
  
  -- Single-driven assignments
  idwvupjz <= idwvupjz;
  
  -- Multi-driven assignments
  fzxmeepm <= fzxmeepm;
  fzxmeepm <= seb;
  mmeic <= mmeic;
end roj;



-- Seed after: 4479234090232901024,6377581586043967497
