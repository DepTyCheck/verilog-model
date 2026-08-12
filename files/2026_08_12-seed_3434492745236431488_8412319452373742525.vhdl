-- Seed: 3434492745236431488,8412319452373742525

entity lgizp is
  port (oopfpzbffx : inout character);
end lgizp;

architecture b of lgizp is
  
begin
  -- Single-driven assignments
  oopfpzbffx <= 'q';
end b;

library ieee;
use ieee.std_logic_1164.all;

entity pwteitos is
  port (savxm : out time; dkupiotupf : inout std_logic);
end pwteitos;

architecture ebmpzfto of pwteitos is
  signal sunifisim : character;
  signal aisfbzuo : character;
  signal rue : character;
  signal tjal : character;
begin
  oacpdufecb : entity work.lgizp
    port map (oopfpzbffx => tjal);
  yciefx : entity work.lgizp
    port map (oopfpzbffx => rue);
  eoiqypowep : entity work.lgizp
    port map (oopfpzbffx => aisfbzuo);
  hfmcdlic : entity work.lgizp
    port map (oopfpzbffx => sunifisim);
  
  -- Single-driven assignments
  savxm <= 8#1_2.23# us;
end ebmpzfto;

entity gtyqzbt is
  port (v : buffer character; ff : linkage character; o : linkage real);
end gtyqzbt;

library ieee;
use ieee.std_logic_1164.all;

architecture ox of gtyqzbt is
  signal xfifpu : std_logic;
  signal hsd : time;
begin
  djjmml : entity work.pwteitos
    port map (savxm => hsd, dkupiotupf => xfifpu);
  mz : entity work.lgizp
    port map (oopfpzbffx => v);
  
  -- Multi-driven assignments
  xfifpu <= xfifpu;
end ox;



-- Seed after: 11435570514792842868,8412319452373742525
