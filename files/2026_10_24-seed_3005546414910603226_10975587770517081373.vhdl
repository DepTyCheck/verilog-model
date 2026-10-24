-- Seed: 3005546414910603226,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity jebleqppj is
  port (wng : out std_logic; yhby : out time);
end jebleqppj;

architecture ohaqy of jebleqppj is
  
begin
  -- Single-driven assignments
  yhby <= 4 sec;
  
  -- Multi-driven assignments
  wng <= '-';
  wng <= wng;
end ohaqy;

library ieee;
use ieee.std_logic_1164.all;

entity erxsny is
  port (thh : buffer integer; bvhm : in time_vector(4 downto 1); ygtpv : inout std_logic);
end erxsny;

library ieee;
use ieee.std_logic_1164.all;

architecture d of erxsny is
  signal yaesqzvz : time;
  signal apj : std_logic;
  signal dvtntg : time;
  signal afqz : time;
  signal zvancau : time;
  signal hqvnat : std_logic;
begin
  jxycnawa : entity work.jebleqppj
    port map (wng => hqvnat, yhby => zvancau);
  wfrdtgn : entity work.jebleqppj
    port map (wng => ygtpv, yhby => afqz);
  w : entity work.jebleqppj
    port map (wng => hqvnat, yhby => dvtntg);
  anibjtr : entity work.jebleqppj
    port map (wng => apj, yhby => yaesqzvz);
  
  -- Single-driven assignments
  thh <= 16#D6#;
  
  -- Multi-driven assignments
  apj <= apj;
end d;



-- Seed after: 11917827938892094115,10975587770517081373
