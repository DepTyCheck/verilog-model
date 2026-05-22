-- Seed: 4343059293907983766,8881251984038598933

library ieee;
use ieee.std_logic_1164.all;

entity ff is
  port (bhkyskajaf : linkage std_logic; jpsqp : out time; vbnzfs : out std_logic);
end ff;



architecture jkdwgn of ff is
  
begin
  
end jkdwgn;

library ieee;
use ieee.std_logic_1164.all;

entity grkizet is
  port (mdgupfeky : in std_logic; xiljb : in time);
end grkizet;

library ieee;
use ieee.std_logic_1164.all;

architecture a of grkizet is
  signal atkmn : time;
  signal uu : std_logic;
  signal d : time;
  signal nnsnx : time;
  signal gawwuywxd : std_logic;
begin
  hicwujecd : entity work.ff
    port map (bhkyskajaf => gawwuywxd, jpsqp => nnsnx, vbnzfs => gawwuywxd);
  wjmaywdvf : entity work.ff
    port map (bhkyskajaf => mdgupfeky, jpsqp => d, vbnzfs => uu);
  naleghqcc : entity work.ff
    port map (bhkyskajaf => uu, jpsqp => atkmn, vbnzfs => gawwuywxd);
end a;



entity y is
  port (dccsleerfp : linkage time; oapw : in real);
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture yxrdsamnb of y is
  signal qoq : std_logic;
  signal ybkzibd : std_logic;
  signal dgdru : time;
  signal eabxy : std_logic;
  signal oj : std_logic;
  signal bczda : time;
  signal jnusfzt : std_logic;
begin
  o : entity work.ff
    port map (bhkyskajaf => jnusfzt, jpsqp => bczda, vbnzfs => oj);
  yrxk : entity work.ff
    port map (bhkyskajaf => eabxy, jpsqp => dgdru, vbnzfs => ybkzibd);
  ckeenxjj : entity work.grkizet
    port map (mdgupfeky => qoq, xiljb => dgdru);
end yxrdsamnb;



-- Seed after: 13335418545285846237,8881251984038598933
