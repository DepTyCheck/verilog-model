-- Seed: 7817299071337855690,15306146742274979323

entity pc is
  port (ynlgpzhte : in bit_vector(4 downto 3));
end pc;

architecture nxqbx of pc is
  
begin
  
end nxqbx;

library ieee;
use ieee.std_logic_1164.all;

entity nqca is
  port (hap : in std_logic_vector(3 to 0); uolb : in real);
end nqca;

architecture t of nqca is
  signal gorfylet : bit_vector(4 downto 3);
  signal cjeragz : bit_vector(4 downto 3);
  signal zkjcvhx : bit_vector(4 downto 3);
begin
  ac : entity work.pc
    port map (ynlgpzhte => zkjcvhx);
  llszz : entity work.pc
    port map (ynlgpzhte => cjeragz);
  z : entity work.pc
    port map (ynlgpzhte => cjeragz);
  k : entity work.pc
    port map (ynlgpzhte => gorfylet);
end t;

library ieee;
use ieee.std_logic_1164.all;

entity xqtwc is
  port (toffbya : inout std_logic; uki : out std_logic_vector(4 to 3));
end xqtwc;

architecture nwatmon of xqtwc is
  signal bfemdx : bit_vector(4 downto 3);
  signal ugwozdsor : bit_vector(4 downto 3);
begin
  vzo : entity work.pc
    port map (ynlgpzhte => ugwozdsor);
  rhhahuo : entity work.pc
    port map (ynlgpzhte => bfemdx);
  
  -- Single-driven assignments
  ugwozdsor <= ('0', '0');
  bfemdx <= ugwozdsor;
end nwatmon;

library ieee;
use ieee.std_logic_1164.all;

entity pidwsuks is
  port (wydk : out std_logic; pdloc : inout integer; sb : buffer string(2 downto 2); il : linkage std_logic_vector(1 to 3));
end pidwsuks;

library ieee;
use ieee.std_logic_1164.all;

architecture euclxqfz of pidwsuks is
  signal jydgbtr : std_logic;
  signal yhdf : real;
  signal vyl : std_logic_vector(4 to 3);
  signal s : bit_vector(4 downto 3);
begin
  z : entity work.pc
    port map (ynlgpzhte => s);
  ha : entity work.nqca
    port map (hap => vyl, uolb => yhdf);
  crzwleoov : entity work.xqtwc
    port map (toffbya => jydgbtr, uki => vyl);
end euclxqfz;



-- Seed after: 5361890412020913731,15306146742274979323
