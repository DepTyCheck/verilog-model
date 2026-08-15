-- Seed: 6181604303660565333,2230106469645304029

entity qsle is
  port (tklga : out boolean_vector(1 to 1));
end qsle;

architecture nghocliew of qsle is
  
begin
  -- Single-driven assignments
  tklga <= (others => FALSE);
end nghocliew;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (cjtlc : inout time; yhz : inout std_logic_vector(0 to 4); jcanho : buffer bit);
end h;

architecture nfsb of h is
  signal qo : boolean_vector(1 to 1);
  signal fael : boolean_vector(1 to 1);
begin
  wi : entity work.qsle
    port map (tklga => fael);
  vsfqabzgl : entity work.qsle
    port map (tklga => qo);
  
  -- Multi-driven assignments
  yhz <= yhz;
end nfsb;

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (mkuddhsa : inout std_logic; qdkp : in std_logic; pzbezv : in boolean);
end l;

library ieee;
use ieee.std_logic_1164.all;

architecture pfukrxbub of l is
  signal znit : bit;
  signal denldpmjju : std_logic_vector(0 to 4);
  signal do : time;
  signal zcycjwp : boolean_vector(1 to 1);
begin
  hqqzkbyh : entity work.qsle
    port map (tklga => zcycjwp);
  evmqsb : entity work.h
    port map (cjtlc => do, yhz => denldpmjju, jcanho => znit);
  
  -- Multi-driven assignments
  mkuddhsa <= '1';
  mkuddhsa <= 'L';
  denldpmjju <= ('X', 'H', '0', '1', 'W');
end pfukrxbub;



-- Seed after: 18052448535055106970,2230106469645304029
