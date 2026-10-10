-- Seed: 14340297828160208659,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity ohv is
  port (iwivxirzcv : in std_logic_vector(0 to 4));
end ohv;

architecture flmz of ohv is
  
begin
  
end flmz;

library ieee;
use ieee.std_logic_1164.all;

entity fj is
  port (jrzdeb : in time; zx : out time_vector(0 to 0); qllzymbnod : out std_logic_vector(0 downto 3));
end fj;

library ieee;
use ieee.std_logic_1164.all;

architecture yimbbkiu of fj is
  signal onws : std_logic_vector(0 to 4);
  signal vefhfj : std_logic_vector(0 to 4);
begin
  ep : entity work.ohv
    port map (iwivxirzcv => vefhfj);
  lrol : entity work.ohv
    port map (iwivxirzcv => vefhfj);
  qkeapwp : entity work.ohv
    port map (iwivxirzcv => onws);
  
  -- Multi-driven assignments
  qllzymbnod <= (others => '0');
end yimbbkiu;

entity ismic is
  port (isx : out integer);
end ismic;

library ieee;
use ieee.std_logic_1164.all;

architecture csgqfilqy of ismic is
  signal g : std_logic_vector(0 downto 3);
  signal dttuhsig : time_vector(0 to 0);
  signal hcwm : time;
  signal s : std_logic_vector(0 to 4);
  signal hiu : std_logic_vector(0 downto 3);
  signal gwfn : time_vector(0 to 0);
  signal wq : time;
begin
  bsh : entity work.fj
    port map (jrzdeb => wq, zx => gwfn, qllzymbnod => hiu);
  fhllweyv : entity work.ohv
    port map (iwivxirzcv => s);
  gwkhlskm : entity work.ohv
    port map (iwivxirzcv => s);
  tppts : entity work.fj
    port map (jrzdeb => hcwm, zx => dttuhsig, qllzymbnod => g);
  
  -- Single-driven assignments
  isx <= 01;
  wq <= 1 hr;
  
  -- Multi-driven assignments
  hiu <= g;
  s <= s;
  hiu <= "";
  hiu <= "";
end csgqfilqy;



-- Seed after: 17524505711347619049,511364357853360275
