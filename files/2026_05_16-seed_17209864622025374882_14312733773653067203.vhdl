-- Seed: 17209864622025374882,14312733773653067203

library ieee;
use ieee.std_logic_1164.all;

entity emgzpsvb is
  port (yw : buffer std_logic; ptnb : buffer real);
end emgzpsvb;



architecture gsquig of emgzpsvb is
  
begin
  
end gsquig;



entity ulhfu is
  port (avj : inout real; xpsgo : buffer bit; xxhbkprep : buffer real; woqeje : in integer);
end ulhfu;



architecture nrfkeisp of ulhfu is
  
begin
  
end nrfkeisp;

library ieee;
use ieee.std_logic_1164.all;

entity mrlgazsqcb is
  port (vglxjq : inout std_logic; jqqpl : out time; xlzpsezuz : inout integer; pvymcxxzx : in integer);
end mrlgazsqcb;

library ieee;
use ieee.std_logic_1164.all;

architecture tqoxjyfjny of mrlgazsqcb is
  signal ro : real;
  signal ndnjy : bit;
  signal afreju : real;
  signal cky : real;
  signal ncjuar : std_logic;
  signal qkwrez : real;
  signal h : std_logic;
begin
  njip : entity work.emgzpsvb
    port map (yw => h, ptnb => qkwrez);
  qooceevgb : entity work.emgzpsvb
    port map (yw => ncjuar, ptnb => cky);
  xrhywkkjxg : entity work.ulhfu
    port map (avj => afreju, xpsgo => ndnjy, xxhbkprep => ro, woqeje => xlzpsezuz);
end tqoxjyfjny;



entity sgydyf is
  port (rbeflwf : out real; wxgifce : out real);
end sgydyf;

library ieee;
use ieee.std_logic_1164.all;

architecture gkdbzod of sgydyf is
  signal wrjvmckrf : integer;
  signal wpbgxdwig : time;
  signal onsvwvq : integer;
  signal cronufm : bit;
  signal tcedpjudwd : real;
  signal dtyrrtge : integer;
  signal kr : time;
  signal afrgd : std_logic;
begin
  ivrgb : entity work.mrlgazsqcb
    port map (vglxjq => afrgd, jqqpl => kr, xlzpsezuz => dtyrrtge, pvymcxxzx => dtyrrtge);
  p : entity work.ulhfu
    port map (avj => tcedpjudwd, xpsgo => cronufm, xxhbkprep => wxgifce, woqeje => onsvwvq);
  rsk : entity work.mrlgazsqcb
    port map (vglxjq => afrgd, jqqpl => wpbgxdwig, xlzpsezuz => wrjvmckrf, pvymcxxzx => dtyrrtge);
  vdpprg : entity work.emgzpsvb
    port map (yw => afrgd, ptnb => rbeflwf);
end gkdbzod;



-- Seed after: 2525431252090111047,14312733773653067203
