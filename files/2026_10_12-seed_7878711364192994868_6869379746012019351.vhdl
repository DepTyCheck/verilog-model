-- Seed: 7878711364192994868,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity vyxs is
  port (trknyi : buffer std_logic; exsue : buffer std_logic; khfqv : in time);
end vyxs;

architecture guuokyqz of vyxs is
  
begin
  -- Multi-driven assignments
  exsue <= exsue;
  exsue <= 'U';
  exsue <= exsue;
  exsue <= 'W';
end guuokyqz;

entity rll is
  port (t : inout real; benhznohg : buffer time);
end rll;

architecture clfyy of rll is
  
begin
  -- Single-driven assignments
  benhznohg <= 1 ps;
end clfyy;

library ieee;
use ieee.std_logic_1164.all;

entity aebbj is
  port (can : in severity_level; kpkcyxeqf : buffer std_logic; evlefmf : out integer);
end aebbj;

library ieee;
use ieee.std_logic_1164.all;

architecture lobjcnnxe of aebbj is
  signal x : std_logic;
  signal sclzmkqeo : time;
  signal wmktm : std_logic;
  signal igitp : std_logic;
  signal dbpen : time;
  signal zvnqubh : std_logic;
  signal lkovow : std_logic;
begin
  piotge : entity work.vyxs
    port map (trknyi => lkovow, exsue => zvnqubh, khfqv => dbpen);
  nmkrqky : entity work.vyxs
    port map (trknyi => igitp, exsue => wmktm, khfqv => sclzmkqeo);
  ykl : entity work.vyxs
    port map (trknyi => igitp, exsue => kpkcyxeqf, khfqv => dbpen);
  bhgo : entity work.vyxs
    port map (trknyi => kpkcyxeqf, exsue => x, khfqv => sclzmkqeo);
  
  -- Single-driven assignments
  evlefmf <= 413;
  dbpen <= dbpen;
end lobjcnnxe;



-- Seed after: 4029729243435665424,6869379746012019351
