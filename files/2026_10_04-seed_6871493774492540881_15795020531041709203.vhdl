-- Seed: 6871493774492540881,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (ntaueu : in std_logic_vector(1 to 4); gj : linkage std_logic; jwchx : in time; xyiu : buffer boolean_vector(4 to 2));
end e;

architecture nj of e is
  
begin
  -- Single-driven assignments
  xyiu <= xyiu;
end nj;

library ieee;
use ieee.std_logic_1164.all;

entity bfkotaw is
  port (gxbuhw : out std_logic; qamf : out time_vector(0 downto 3));
end bfkotaw;

library ieee;
use ieee.std_logic_1164.all;

architecture xej of bfkotaw is
  signal tunqah : boolean_vector(4 to 2);
  signal zo : time;
  signal fbndnxzpdj : std_logic_vector(1 to 4);
  signal gppybqw : boolean_vector(4 to 2);
  signal xodga : time;
  signal s : std_logic_vector(1 to 4);
begin
  pahm : entity work.e
    port map (ntaueu => s, gj => gxbuhw, jwchx => xodga, xyiu => gppybqw);
  pwurdohu : entity work.e
    port map (ntaueu => fbndnxzpdj, gj => gxbuhw, jwchx => zo, xyiu => tunqah);
  
  -- Multi-driven assignments
  gxbuhw <= gxbuhw;
end xej;

entity timfihrsn is
  port (zwg : in boolean);
end timfihrsn;

library ieee;
use ieee.std_logic_1164.all;

architecture yqnjwvms of timfihrsn is
  signal soqlbcs : boolean_vector(4 to 2);
  signal ox : time;
  signal gfjksppato : std_logic;
  signal uijihbuzod : std_logic_vector(1 to 4);
  signal hti : boolean_vector(4 to 2);
  signal bln : time;
  signal gwcwlqorht : std_logic_vector(1 to 4);
  signal s : time_vector(0 downto 3);
  signal bkh : std_logic;
begin
  tqyipdgfec : entity work.bfkotaw
    port map (gxbuhw => bkh, qamf => s);
  faffp : entity work.e
    port map (ntaueu => gwcwlqorht, gj => bkh, jwchx => bln, xyiu => hti);
  isj : entity work.e
    port map (ntaueu => uijihbuzod, gj => gfjksppato, jwchx => ox, xyiu => soqlbcs);
  
  -- Single-driven assignments
  ox <= 2#010.1_1_1_1# fs;
  bln <= 11 ps;
  
  -- Multi-driven assignments
  bkh <= bkh;
  gwcwlqorht <= ('L', 'L', 'X', '0');
  gfjksppato <= gfjksppato;
  bkh <= bkh;
end yqnjwvms;



-- Seed after: 13183436465436882142,15795020531041709203
