-- Seed: 6796824306279955761,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity abqni is
  port (zzyczmn : in time; vzoaesst : in std_logic_vector(1 to 2));
end abqni;

architecture tsppcmumf of abqni is
  
begin
  
end tsppcmumf;

entity pfrlazcxef is
  port (wsvfhfnqh : inout time);
end pfrlazcxef;

library ieee;
use ieee.std_logic_1164.all;

architecture x of pfrlazcxef is
  signal xuitytmno : std_logic_vector(1 to 2);
  signal tsonzo : std_logic_vector(1 to 2);
  signal lghebag : std_logic_vector(1 to 2);
  signal afituoa : time;
begin
  zoidk : entity work.abqni
    port map (zzyczmn => afituoa, vzoaesst => lghebag);
  sngfyhh : entity work.abqni
    port map (zzyczmn => afituoa, vzoaesst => tsonzo);
  fedeyzwb : entity work.abqni
    port map (zzyczmn => wsvfhfnqh, vzoaesst => xuitytmno);
  
  -- Single-driven assignments
  wsvfhfnqh <= wsvfhfnqh;
  afituoa <= 1_2_4_1_1 ms;
end x;



-- Seed after: 3389749816508761277,15306146742274979323
