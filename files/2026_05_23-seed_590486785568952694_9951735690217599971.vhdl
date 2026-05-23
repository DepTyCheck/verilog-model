-- Seed: 590486785568952694,9951735690217599971

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (hpmmduaei : inout std_logic_vector(4 downto 2); amprjcyo : out integer; kbxnomgqi : linkage time);
end l;



architecture cgg of l is
  
begin
  
end cgg;



entity jhh is
  port (jcdcs : inout integer);
end jhh;

library ieee;
use ieee.std_logic_1164.all;

architecture q of jhh is
  signal aqxljeok : integer;
  signal wkboaolnic : time;
  signal mdzyp : integer;
  signal iqmq : time;
  signal v : integer;
  signal c : std_logic_vector(4 downto 2);
begin
  lw : entity work.l
    port map (hpmmduaei => c, amprjcyo => v, kbxnomgqi => iqmq);
  as : entity work.l
    port map (hpmmduaei => c, amprjcyo => mdzyp, kbxnomgqi => iqmq);
  x : entity work.l
    port map (hpmmduaei => c, amprjcyo => jcdcs, kbxnomgqi => wkboaolnic);
  vuymgqaz : entity work.l
    port map (hpmmduaei => c, amprjcyo => aqxljeok, kbxnomgqi => iqmq);
end q;



-- Seed after: 16786209927004413878,9951735690217599971
