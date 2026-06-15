-- Seed: 17161293591777812953,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity kvigu is
  port (rcvyi : inout std_logic; j : linkage integer; rq : out std_logic_vector(1 downto 4); wyki : out real);
end kvigu;

architecture a of kvigu is
  
begin
  
end a;

library ieee;
use ieee.std_logic_1164.all;

entity bk is
  port (wayvu : in std_logic_vector(0 downto 0); vfmce : inout boolean);
end bk;

architecture ocawajxy of bk is
  
begin
  
end ocawajxy;

entity v is
  port (wapxcdxk : out integer; zhvofpfh : in real; yznsvp : in real; gaj : out string(3 to 2));
end v;

architecture ninkiuyrc of v is
  
begin
  
end ninkiuyrc;

library ieee;
use ieee.std_logic_1164.all;

entity kybhhg is
  port (lkrlmj : inout std_logic; xefjzwzy : buffer time; envncm : linkage boolean);
end kybhhg;

library ieee;
use ieee.std_logic_1164.all;

architecture jprymu of kybhhg is
  signal bsccqqqpor : std_logic_vector(1 downto 4);
  signal yezdqstvh : integer;
  signal dby : string(3 to 2);
  signal pzboaekqra : real;
  signal tofx : real;
  signal un : integer;
  signal sh : boolean;
  signal jevnnybg : std_logic_vector(0 downto 0);
begin
  qpdhk : entity work.bk
    port map (wayvu => jevnnybg, vfmce => sh);
  hxrr : entity work.v
    port map (wapxcdxk => un, zhvofpfh => tofx, yznsvp => pzboaekqra, gaj => dby);
  up : entity work.kvigu
    port map (rcvyi => lkrlmj, j => yezdqstvh, rq => bsccqqqpor, wyki => pzboaekqra);
  
  -- Multi-driven assignments
  lkrlmj <= 'L';
  bsccqqqpor <= "";
  lkrlmj <= 'Z';
  lkrlmj <= '0';
end jprymu;



-- Seed after: 10183702210849401629,15300320181035395489
