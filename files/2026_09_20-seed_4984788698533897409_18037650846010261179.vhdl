-- Seed: 4984788698533897409,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity mdghccic is
  port (piu : inout time; iixokj : linkage std_logic_vector(4 to 4));
end mdghccic;

architecture revyzohtlx of mdghccic is
  
begin
  
end revyzohtlx;

library ieee;
use ieee.std_logic_1164.all;

entity ysi is
  port (u : buffer std_logic; bxzwezyw : in std_logic; vyyzw : inout bit; xhgzqcwj : out std_logic);
end ysi;

library ieee;
use ieee.std_logic_1164.all;

architecture nmnuqelc of ysi is
  signal nkgimg : time;
  signal crkaoab : std_logic_vector(4 to 4);
  signal hhkckrserd : time;
  signal gupzc : std_logic_vector(4 to 4);
  signal ditpeglzvk : time;
  signal cotczvg : std_logic_vector(4 to 4);
  signal h : time;
begin
  uawf : entity work.mdghccic
    port map (piu => h, iixokj => cotczvg);
  fvv : entity work.mdghccic
    port map (piu => ditpeglzvk, iixokj => gupzc);
  qv : entity work.mdghccic
    port map (piu => hhkckrserd, iixokj => crkaoab);
  wss : entity work.mdghccic
    port map (piu => nkgimg, iixokj => gupzc);
  
  -- Single-driven assignments
  vyyzw <= '1';
  
  -- Multi-driven assignments
  gupzc <= "X";
  xhgzqcwj <= u;
  xhgzqcwj <= 'W';
end nmnuqelc;

entity rbxbdrysem is
  port (ar : out real; svoerixx : buffer string(2 to 1));
end rbxbdrysem;

library ieee;
use ieee.std_logic_1164.all;

architecture lndvzjnka of rbxbdrysem is
  signal gmo : std_logic;
  signal w : bit;
  signal mhuqsl : std_logic;
  signal gzwun : std_logic;
  signal ud : std_logic_vector(4 to 4);
  signal ed : time;
begin
  t : entity work.mdghccic
    port map (piu => ed, iixokj => ud);
  y : entity work.ysi
    port map (u => gzwun, bxzwezyw => mhuqsl, vyyzw => w, xhgzqcwj => gmo);
  
  -- Multi-driven assignments
  ud <= ud;
  ud <= ud;
  gzwun <= '1';
  mhuqsl <= gzwun;
end lndvzjnka;



-- Seed after: 1136814765146557130,18037650846010261179
