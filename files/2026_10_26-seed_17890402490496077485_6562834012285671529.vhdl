-- Seed: 17890402490496077485,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (jnfob : linkage std_logic_vector(1 to 4); k : buffer std_logic);
end j;

architecture cmafbc of j is
  
begin
  -- Multi-driven assignments
  k <= k;
  k <= k;
  k <= k;
  k <= 'L';
end cmafbc;

entity bq is
  port (ljttmrt : buffer time; lcuio : buffer boolean);
end bq;

library ieee;
use ieee.std_logic_1164.all;

architecture mh of bq is
  signal xotykh : std_logic_vector(1 to 4);
  signal wjjccnuhta : std_logic;
  signal zfqxmycey : std_logic_vector(1 to 4);
begin
  llso : entity work.j
    port map (jnfob => zfqxmycey, k => wjjccnuhta);
  yn : entity work.j
    port map (jnfob => xotykh, k => wjjccnuhta);
  
  -- Multi-driven assignments
  xotykh <= zfqxmycey;
  zfqxmycey <= zfqxmycey;
  zfqxmycey <= zfqxmycey;
  xotykh <= "X10-";
end mh;



-- Seed after: 15542256876068311367,6562834012285671529
