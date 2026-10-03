-- Seed: 423288302416351237,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity dfrx is
  port (rtdt : buffer std_logic_vector(0 to 2));
end dfrx;

architecture iu of dfrx is
  
begin
  -- Multi-driven assignments
  rtdt <= ('H', '-', 'X');
  rtdt <= ('-', 'U', 'Z');
  rtdt <= rtdt;
  rtdt <= rtdt;
end iu;

library ieee;
use ieee.std_logic_1164.all;

entity etspwfgvl is
  port (kpulxy : inout std_logic; xt : linkage integer_vector(1 to 0); cc : linkage std_logic; eaqo : out real);
end etspwfgvl;

library ieee;
use ieee.std_logic_1164.all;

architecture xfymydkbb of etspwfgvl is
  signal zejsnaakii : std_logic_vector(0 to 2);
  signal xechatpx : std_logic_vector(0 to 2);
  signal jtawdxxlvr : std_logic_vector(0 to 2);
  signal rpviwfbmpn : std_logic_vector(0 to 2);
begin
  wlcm : entity work.dfrx
    port map (rtdt => rpviwfbmpn);
  fq : entity work.dfrx
    port map (rtdt => jtawdxxlvr);
  ejvqrugjug : entity work.dfrx
    port map (rtdt => xechatpx);
  y : entity work.dfrx
    port map (rtdt => zejsnaakii);
end xfymydkbb;

entity ryucqfrq is
  port (n : inout time);
end ryucqfrq;

library ieee;
use ieee.std_logic_1164.all;

architecture dzijvkfsz of ryucqfrq is
  signal otjwqqvzjm : real;
  signal qwdgt : integer_vector(1 to 0);
  signal zxhibznlcz : std_logic;
  signal lcgm : std_logic_vector(0 to 2);
begin
  mum : entity work.dfrx
    port map (rtdt => lcgm);
  dkvjisrs : entity work.etspwfgvl
    port map (kpulxy => zxhibznlcz, xt => qwdgt, cc => zxhibznlcz, eaqo => otjwqqvzjm);
  
  -- Multi-driven assignments
  lcgm <= lcgm;
  lcgm <= lcgm;
  lcgm <= lcgm;
end dzijvkfsz;



-- Seed after: 3428741468722393119,6140041381800297705
