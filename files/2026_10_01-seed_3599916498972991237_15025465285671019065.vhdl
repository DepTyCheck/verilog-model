-- Seed: 3599916498972991237,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity jiarcm is
  port (iyukhpde : buffer std_logic_vector(4 to 2));
end jiarcm;

architecture xlpbvyq of jiarcm is
  
begin
  -- Multi-driven assignments
  iyukhpde <= "";
  iyukhpde <= iyukhpde;
  iyukhpde <= (others => '0');
  iyukhpde <= iyukhpde;
end xlpbvyq;

library ieee;
use ieee.std_logic_1164.all;

entity fvua is
  port (oqiqnh : in time; isffhbcygx : out time; f : buffer std_logic_vector(0 downto 4); jqkcmsi : in time);
end fvua;

library ieee;
use ieee.std_logic_1164.all;

architecture svumati of fvua is
  signal ecpgl : std_logic_vector(4 to 2);
  signal zmsofgoss : std_logic_vector(4 to 2);
begin
  nlvzzcfs : entity work.jiarcm
    port map (iyukhpde => f);
  s : entity work.jiarcm
    port map (iyukhpde => zmsofgoss);
  xbbwgaqr : entity work.jiarcm
    port map (iyukhpde => ecpgl);
  
  -- Multi-driven assignments
  ecpgl <= ecpgl;
  f <= f;
end svumati;

entity osr is
  port (btxomub : out time; wcphv : inout real; noast : buffer character);
end osr;

library ieee;
use ieee.std_logic_1164.all;

architecture lpre of osr is
  signal tpyqabxwj : std_logic_vector(4 to 2);
begin
  mwbc : entity work.jiarcm
    port map (iyukhpde => tpyqabxwj);
  ujdgew : entity work.fvua
    port map (oqiqnh => btxomub, isffhbcygx => btxomub, f => tpyqabxwj, jqkcmsi => btxomub);
  yqenvbzt : entity work.jiarcm
    port map (iyukhpde => tpyqabxwj);
  
  -- Multi-driven assignments
  tpyqabxwj <= tpyqabxwj;
end lpre;



-- Seed after: 15917662789447473151,15025465285671019065
