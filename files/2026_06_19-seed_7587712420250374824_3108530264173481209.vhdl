-- Seed: 7587712420250374824,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity aductt is
  port (ykxeyun : out time; i : inout character; l : out character; mggiufphd : inout std_logic);
end aductt;

architecture cceufqej of aductt is
  
begin
  -- Single-driven assignments
  l <= 'l';
  i <= 'x';
  ykxeyun <= 3_4.331 fs;
end cceufqej;

entity mg is
  port (juu : linkage real; ttu : in time);
end mg;

architecture omxlbl of mg is
  
begin
  
end omxlbl;

library ieee;
use ieee.std_logic_1164.all;

entity yjr is
  port (yd : inout real; e : inout std_logic_vector(1 to 2));
end yjr;

architecture gkwvnvy of yjr is
  
begin
  -- Single-driven assignments
  yd <= 1_1_3.1;
  
  -- Multi-driven assignments
  e <= ('U', 'L');
  e <= ('H', 'X');
  e <= ('U', 'L');
  e <= ('-', 'H');
end gkwvnvy;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (aatz : linkage time; pikmg : buffer std_logic);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture ivgs of h is
  signal xkvyctm : character;
  signal qgycrhcb : character;
  signal jwheh : time;
  signal jqo : real;
  signal moqire : std_logic;
  signal ae : character;
  signal qexholhxgr : character;
  signal kyonbbhm : time;
begin
  swkhjslama : entity work.aductt
    port map (ykxeyun => kyonbbhm, i => qexholhxgr, l => ae, mggiufphd => moqire);
  hh : entity work.mg
    port map (juu => jqo, ttu => kyonbbhm);
  p : entity work.aductt
    port map (ykxeyun => jwheh, i => qgycrhcb, l => xkvyctm, mggiufphd => pikmg);
  
  -- Multi-driven assignments
  pikmg <= '0';
  pikmg <= 'Z';
  moqire <= 'H';
end ivgs;



-- Seed after: 11155414054094301428,3108530264173481209
