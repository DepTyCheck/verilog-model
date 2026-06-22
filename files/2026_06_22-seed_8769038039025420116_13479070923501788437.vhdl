-- Seed: 8769038039025420116,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity wlw is
  port (le : in bit; dgpgybrp : linkage time_vector(4 to 2); f : in std_logic);
end wlw;

architecture xg of wlw is
  
begin
  
end xg;

library ieee;
use ieee.std_logic_1164.all;

entity vqwwn is
  port (chhj : inout std_logic_vector(2 downto 1); ovji : inout std_logic_vector(2 downto 1); krv : in boolean; uuxicuk : buffer bit);
end vqwwn;

library ieee;
use ieee.std_logic_1164.all;

architecture uhngaib of vqwwn is
  signal amhwwcaeom : std_logic;
  signal urfzyvfcr : time_vector(4 to 2);
begin
  pn : entity work.wlw
    port map (le => uuxicuk, dgpgybrp => urfzyvfcr, f => amhwwcaeom);
  
  -- Single-driven assignments
  uuxicuk <= '0';
  
  -- Multi-driven assignments
  amhwwcaeom <= 'U';
end uhngaib;

entity efbbyax is
  port (aynkoalga : buffer time; ielu : inout bit_vector(2 to 3));
end efbbyax;

library ieee;
use ieee.std_logic_1164.all;

architecture amwypkmba of efbbyax is
  signal vzl : std_logic;
  signal tcunvso : time_vector(4 to 2);
  signal ddgnzjrumj : std_logic;
  signal mohwliniu : time_vector(4 to 2);
  signal fjnfmttu : bit;
  signal t : boolean;
  signal zclddbepzq : std_logic_vector(2 downto 1);
  signal vdh : bit;
  signal ghqsz : boolean;
  signal cfhavbdop : std_logic_vector(2 downto 1);
  signal quyfllpeys : std_logic_vector(2 downto 1);
begin
  apqqx : entity work.vqwwn
    port map (chhj => quyfllpeys, ovji => cfhavbdop, krv => ghqsz, uuxicuk => vdh);
  jrj : entity work.vqwwn
    port map (chhj => quyfllpeys, ovji => zclddbepzq, krv => t, uuxicuk => fjnfmttu);
  egfj : entity work.wlw
    port map (le => vdh, dgpgybrp => mohwliniu, f => ddgnzjrumj);
  wrododyxng : entity work.wlw
    port map (le => vdh, dgpgybrp => tcunvso, f => vzl);
  
  -- Multi-driven assignments
  cfhavbdop <= "WU";
  cfhavbdop <= ('-', '-');
  ddgnzjrumj <= '-';
  zclddbepzq <= ('X', 'U');
end amwypkmba;



-- Seed after: 9547688738632574991,13479070923501788437
