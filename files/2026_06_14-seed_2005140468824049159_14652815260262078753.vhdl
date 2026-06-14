-- Seed: 2005140468824049159,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity cpn is
  port (sg : in std_logic_vector(2 to 2));
end cpn;

architecture qfxm of cpn is
  
begin
  
end qfxm;

library ieee;
use ieee.std_logic_1164.all;

entity dp is
  port (hdfkpyloh : inout std_logic);
end dp;

library ieee;
use ieee.std_logic_1164.all;

architecture qh of dp is
  signal u : std_logic_vector(2 to 2);
  signal iovmjwgb : std_logic_vector(2 to 2);
begin
  yxfy : entity work.cpn
    port map (sg => iovmjwgb);
  geppdkdwos : entity work.cpn
    port map (sg => iovmjwgb);
  nevlst : entity work.cpn
    port map (sg => u);
  ncffcpn : entity work.cpn
    port map (sg => iovmjwgb);
  
  -- Multi-driven assignments
  u <= "W";
  hdfkpyloh <= '-';
  hdfkpyloh <= 'Z';
  hdfkpyloh <= '-';
end qh;

library ieee;
use ieee.std_logic_1164.all;

entity mu is
  port (rf : linkage integer; ip : linkage std_logic_vector(4 to 3); sgcdmmg : in real_vector(0 downto 0); mwx : buffer boolean);
end mu;

library ieee;
use ieee.std_logic_1164.all;

architecture fvdx of mu is
  signal fwuddpij : std_logic_vector(2 to 2);
  signal pkftgoapt : std_logic_vector(2 to 2);
begin
  qsaqr : entity work.cpn
    port map (sg => pkftgoapt);
  vbpmin : entity work.cpn
    port map (sg => pkftgoapt);
  icqpidd : entity work.cpn
    port map (sg => fwuddpij);
  
  -- Single-driven assignments
  mwx <= TRUE;
  
  -- Multi-driven assignments
  pkftgoapt <= "H";
  pkftgoapt <= (others => '-');
end fvdx;



-- Seed after: 4979719122446131265,14652815260262078753
