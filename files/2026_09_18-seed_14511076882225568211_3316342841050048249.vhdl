-- Seed: 14511076882225568211,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity bkp is
  port ( aviyj : inout time
  ; dusenr : buffer std_logic_vector(4 downto 4)
  ; vbbwo : linkage time_vector(4 downto 2)
  ; lvjuygtr : inout std_logic_vector(1 to 1)
  );
end bkp;

architecture quiymum of bkp is
  
begin
  -- Multi-driven assignments
  dusenr <= "X";
  lvjuygtr <= "H";
  dusenr <= dusenr;
  lvjuygtr <= (others => '1');
end quiymum;

entity nhexg is
  port (aer : buffer severity_level; pwwlpys : out time; ijpap : linkage time);
end nhexg;

library ieee;
use ieee.std_logic_1164.all;

architecture vadj of nhexg is
  signal ttiyizhjyk : std_logic_vector(1 to 1);
  signal pi : time_vector(4 downto 2);
  signal mzmdoucbzy : std_logic_vector(1 to 1);
  signal vdnxxkifpa : time_vector(4 downto 2);
  signal srzvumgx : std_logic_vector(4 downto 4);
  signal zqkdzwnxdm : time;
begin
  gvvjeefgk : entity work.bkp
    port map (aviyj => zqkdzwnxdm, dusenr => srzvumgx, vbbwo => vdnxxkifpa, lvjuygtr => mzmdoucbzy);
  whe : entity work.bkp
    port map (aviyj => pwwlpys, dusenr => srzvumgx, vbbwo => pi, lvjuygtr => ttiyizhjyk);
  
  -- Single-driven assignments
  aer <= WARNING;
  
  -- Multi-driven assignments
  srzvumgx <= (others => 'X');
  srzvumgx <= mzmdoucbzy;
  ttiyizhjyk <= "L";
end vadj;

entity gu is
  port (ufmcorzms : inout real);
end gu;

library ieee;
use ieee.std_logic_1164.all;

architecture j of gu is
  signal jucrex : std_logic_vector(1 to 1);
  signal nxaevolpr : time_vector(4 downto 2);
  signal y : std_logic_vector(4 downto 4);
  signal jrma : time;
  signal adhqetx : std_logic_vector(1 to 1);
  signal auhr : time_vector(4 downto 2);
  signal histcqckbi : std_logic_vector(4 downto 4);
  signal varhcnbnj : time;
begin
  ngedps : entity work.bkp
    port map (aviyj => varhcnbnj, dusenr => histcqckbi, vbbwo => auhr, lvjuygtr => adhqetx);
  maazyeih : entity work.bkp
    port map (aviyj => jrma, dusenr => y, vbbwo => nxaevolpr, lvjuygtr => jucrex);
  
  -- Single-driven assignments
  ufmcorzms <= 8#100.53#;
end j;



-- Seed after: 4247255331937234046,3316342841050048249
