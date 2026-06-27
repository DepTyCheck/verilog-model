-- Seed: 15500005124458691721,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity ihaeamh is
  port (qxs : in std_logic);
end ihaeamh;

architecture wqnkbhni of ihaeamh is
  
begin
  
end wqnkbhni;

library ieee;
use ieee.std_logic_1164.all;

entity qllcjuirv is
  port (orvlnbvma : out std_logic; q : in real);
end qllcjuirv;

architecture ehbchkfr of qllcjuirv is
  
begin
  dthy : entity work.ihaeamh
    port map (qxs => orvlnbvma);
end ehbchkfr;

library ieee;
use ieee.std_logic_1164.all;

entity bwx is
  port (lldzfnt : linkage std_logic; assk : in boolean_vector(2 to 3));
end bwx;

library ieee;
use ieee.std_logic_1164.all;

architecture m of bwx is
  signal ewww : real;
  signal ke : std_logic;
begin
  o : entity work.qllcjuirv
    port map (orvlnbvma => ke, q => ewww);
  
  -- Single-driven assignments
  ewww <= 16#47C3.2_F_E_7_8#;
  
  -- Multi-driven assignments
  ke <= 'L';
  ke <= 'H';
  ke <= 'Z';
  ke <= '-';
end m;

library ieee;
use ieee.std_logic_1164.all;

entity ukhsc is
  port (favhcgorg : out std_logic_vector(2 to 2); q : out std_logic; z : buffer integer_vector(1 to 4); ste : inout std_logic);
end ukhsc;

library ieee;
use ieee.std_logic_1164.all;

architecture nwuveavy of ukhsc is
  signal stxol : boolean_vector(2 to 3);
  signal czefdwk : std_logic;
  signal ylxzh : std_logic;
begin
  ev : entity work.ihaeamh
    port map (qxs => ste);
  tnenvfoo : entity work.ihaeamh
    port map (qxs => ylxzh);
  r : entity work.ihaeamh
    port map (qxs => czefdwk);
  kjjxtfdon : entity work.bwx
    port map (lldzfnt => czefdwk, assk => stxol);
  
  -- Single-driven assignments
  z <= (3_1, 8#05#, 8#33236#, 03020);
  stxol <= (FALSE, TRUE);
  
  -- Multi-driven assignments
  ste <= '-';
end nwuveavy;



-- Seed after: 13634895541774781411,4860866131898729603
