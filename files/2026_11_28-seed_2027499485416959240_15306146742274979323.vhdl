-- Seed: 2027499485416959240,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity fvrxj is
  port (wa : out std_logic_vector(3 downto 3); niezte : inout std_logic_vector(2 to 4));
end fvrxj;

architecture z of fvrxj is
  
begin
  
end z;

entity dyphsd is
  port (ibjoaue : linkage real);
end dyphsd;

architecture fszblsjk of dyphsd is
  
begin
  
end fszblsjk;

library ieee;
use ieee.std_logic_1164.all;

entity klxja is
  port (bo : buffer integer_vector(3 downto 4); qgeiruei : out integer_vector(4 to 3); oexddja : buffer std_logic; vbbtcs : buffer real);
end klxja;

library ieee;
use ieee.std_logic_1164.all;

architecture aokeuos of klxja is
  signal eg : std_logic_vector(2 to 4);
  signal nbaqn : std_logic_vector(3 downto 3);
  signal ndghxrz : real;
begin
  ttoj : entity work.dyphsd
    port map (ibjoaue => ndghxrz);
  lnjcn : entity work.fvrxj
    port map (wa => nbaqn, niezte => eg);
  
  -- Multi-driven assignments
  oexddja <= 'X';
end aokeuos;

entity a is
  port (zbtsegjl : out integer; zgleipceqs : buffer real);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture sjbthx of a is
  signal qdu : std_logic_vector(2 to 4);
  signal uqqxyxm : std_logic_vector(3 downto 3);
  signal gaeiu : std_logic;
  signal yv : integer_vector(4 to 3);
  signal eknyofgt : integer_vector(3 downto 4);
begin
  m : entity work.klxja
    port map (bo => eknyofgt, qgeiruei => yv, oexddja => gaeiu, vbbtcs => zgleipceqs);
  jbc : entity work.fvrxj
    port map (wa => uqqxyxm, niezte => qdu);
end sjbthx;



-- Seed after: 6753081108513703759,15306146742274979323
