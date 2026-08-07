-- Seed: 5275485171133635558,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity fbarjzj is
  port (gjwkip : out std_logic);
end fbarjzj;

architecture mofig of fbarjzj is
  
begin
  
end mofig;

library ieee;
use ieee.std_logic_1164.all;

entity qgrhzqv is
  port (hdyfeu : in time; c : in bit_vector(0 to 2); suqhyxwpr : linkage std_logic_vector(3 downto 2));
end qgrhzqv;

library ieee;
use ieee.std_logic_1164.all;

architecture vhj of qgrhzqv is
  signal a : std_logic;
begin
  ogmvra : entity work.fbarjzj
    port map (gjwkip => a);
end vhj;

library ieee;
use ieee.std_logic_1164.all;

entity sagcjoxw is
  port (fop : out real_vector(2 to 2); dj : linkage std_logic);
end sagcjoxw;

library ieee;
use ieee.std_logic_1164.all;

architecture ibnekwfcri of sagcjoxw is
  signal kqpl : std_logic_vector(3 downto 2);
  signal ak : bit_vector(0 to 2);
  signal zcsgxbrwhp : time;
begin
  s : entity work.qgrhzqv
    port map (hdyfeu => zcsgxbrwhp, c => ak, suqhyxwpr => kqpl);
  
  -- Single-driven assignments
  fop <= fop;
  
  -- Multi-driven assignments
  kqpl <= "WZ";
  kqpl <= "ZU";
  kqpl <= kqpl;
end ibnekwfcri;

entity bwyibub is
  port (caczy : out integer);
end bwyibub;

library ieee;
use ieee.std_logic_1164.all;

architecture ad of bwyibub is
  signal kvvbtjigbd : std_logic;
begin
  ns : entity work.fbarjzj
    port map (gjwkip => kvvbtjigbd);
  
  -- Single-driven assignments
  caczy <= caczy;
  
  -- Multi-driven assignments
  kvvbtjigbd <= kvvbtjigbd;
end ad;



-- Seed after: 8357147316595644041,8068158652091157513
