-- Seed: 14805491405573902646,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity bporhhebe is
  port (dmbjuoffgt : in time; piaiwlm : out std_logic; dvavbf : inout time; zyhchrwpjk : in std_logic);
end bporhhebe;

architecture awdlah of bporhhebe is
  
begin
  -- Single-driven assignments
  dvavbf <= 2#0# ms;
  
  -- Multi-driven assignments
  piaiwlm <= 'L';
  piaiwlm <= zyhchrwpjk;
  piaiwlm <= '-';
end awdlah;

entity snly is
  port (zapdiznku : buffer real; gp : buffer time);
end snly;

library ieee;
use ieee.std_logic_1164.all;

architecture ry of snly is
  signal gbuftn : time;
  signal faepub : std_logic;
  signal jeqoqolil : std_logic;
  signal hlivccsu : time;
  signal d : std_logic;
  signal zvigvwpl : std_logic;
  signal rj : std_logic;
  signal tihubre : time;
begin
  ahxqtjozg : entity work.bporhhebe
    port map (dmbjuoffgt => tihubre, piaiwlm => rj, dvavbf => gp, zyhchrwpjk => zvigvwpl);
  va : entity work.bporhhebe
    port map (dmbjuoffgt => gp, piaiwlm => d, dvavbf => hlivccsu, zyhchrwpjk => jeqoqolil);
  juuwbo : entity work.bporhhebe
    port map (dmbjuoffgt => gp, piaiwlm => rj, dvavbf => tihubre, zyhchrwpjk => faepub);
  owizpfy : entity work.bporhhebe
    port map (dmbjuoffgt => gp, piaiwlm => zvigvwpl, dvavbf => gbuftn, zyhchrwpjk => rj);
  
  -- Single-driven assignments
  zapdiznku <= 8#5.62232#;
  
  -- Multi-driven assignments
  faepub <= rj;
end ry;

library ieee;
use ieee.std_logic_1164.all;

entity ybzf is
  port (sgrzv : out std_logic_vector(0 to 0));
end ybzf;

library ieee;
use ieee.std_logic_1164.all;

architecture gdgqpqegh of ybzf is
  signal mfghjezgr : std_logic;
  signal cnivxir : time;
  signal pyyygjcd : std_logic;
  signal m : time;
begin
  jcsdgqrp : entity work.bporhhebe
    port map (dmbjuoffgt => m, piaiwlm => pyyygjcd, dvavbf => cnivxir, zyhchrwpjk => mfghjezgr);
  
  -- Single-driven assignments
  m <= m;
  
  -- Multi-driven assignments
  sgrzv <= "-";
end gdgqpqegh;



-- Seed after: 14163120184034406519,17219977430298397867
