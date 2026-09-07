-- Seed: 11930157035165056907,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity wcgpchm is
  port (xshhb : inout std_logic; qrlvwrrote : inout time; oscv : out time_vector(4 to 0));
end wcgpchm;

architecture bdj of wcgpchm is
  
begin
  -- Single-driven assignments
  oscv <= oscv;
  qrlvwrrote <= qrlvwrrote;
  
  -- Multi-driven assignments
  xshhb <= '1';
  xshhb <= 'X';
end bdj;

entity gszclezn is
  port (qewlcuxins : inout bit_vector(3 to 1); y : inout time);
end gszclezn;

library ieee;
use ieee.std_logic_1164.all;

architecture zrbiyrpg of gszclezn is
  signal ajo : time_vector(4 to 0);
  signal cuxejadjj : time;
  signal hlzf : time_vector(4 to 0);
  signal hxcnfypax : time;
  signal r : std_logic;
begin
  bze : entity work.wcgpchm
    port map (xshhb => r, qrlvwrrote => hxcnfypax, oscv => hlzf);
  prvm : entity work.wcgpchm
    port map (xshhb => r, qrlvwrrote => cuxejadjj, oscv => ajo);
  
  -- Single-driven assignments
  y <= 2#10# ns;
  qewlcuxins <= (others => '0');
  
  -- Multi-driven assignments
  r <= 'U';
  r <= 'H';
  r <= 'H';
  r <= 'W';
end zrbiyrpg;

library ieee;
use ieee.std_logic_1164.all;

entity gtnljlqq is
  port (knzp : linkage std_logic; hfcpbuf : in std_logic; aaodkowng : out bit);
end gtnljlqq;

architecture ahqu of gtnljlqq is
  signal anooiia : time;
  signal qxj : bit_vector(3 to 1);
  signal fjjzgvnktx : time;
  signal ermjxap : bit_vector(3 to 1);
begin
  jhjym : entity work.gszclezn
    port map (qewlcuxins => ermjxap, y => fjjzgvnktx);
  ijfmrm : entity work.gszclezn
    port map (qewlcuxins => qxj, y => anooiia);
end ahqu;



-- Seed after: 12482374152135939579,12269339630485015285
