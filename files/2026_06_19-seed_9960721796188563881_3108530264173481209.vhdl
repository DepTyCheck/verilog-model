-- Seed: 9960721796188563881,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity tprrulon is
  port (mvspgoxmm : buffer real; osr : inout std_logic_vector(3 downto 0));
end tprrulon;

architecture dphqyxcsal of tprrulon is
  
begin
  -- Single-driven assignments
  mvspgoxmm <= 2#1_1_1_1.1101#;
  
  -- Multi-driven assignments
  osr <= ('L', 'W', 'W', 'X');
end dphqyxcsal;

library ieee;
use ieee.std_logic_1164.all;

entity dqiyevhcir is
  port (rague : linkage integer; eodvwm : in real_vector(4 downto 1); jvl : linkage std_logic);
end dqiyevhcir;

library ieee;
use ieee.std_logic_1164.all;

architecture fvxk of dqiyevhcir is
  signal dbbldh : real;
  signal brsvk : real;
  signal obcjdzxs : std_logic_vector(3 downto 0);
  signal ddx : real;
  signal t : std_logic_vector(3 downto 0);
  signal jl : real;
begin
  cuyis : entity work.tprrulon
    port map (mvspgoxmm => jl, osr => t);
  fkccnxgk : entity work.tprrulon
    port map (mvspgoxmm => ddx, osr => obcjdzxs);
  ksprxilhfg : entity work.tprrulon
    port map (mvspgoxmm => brsvk, osr => t);
  stwkduriq : entity work.tprrulon
    port map (mvspgoxmm => dbbldh, osr => t);
end fvxk;

entity hiizvlreo is
  port (ejyl : out real);
end hiizvlreo;

architecture cubv of hiizvlreo is
  
begin
  -- Single-driven assignments
  ejyl <= 2#1_0.0#;
end cubv;

entity jqtubrjjow is
  port (vkgvjtigl : out time);
end jqtubrjjow;

library ieee;
use ieee.std_logic_1164.all;

architecture wtvj of jqtubrjjow is
  signal zha : real;
  signal tiauhikzgv : real;
  signal uemk : std_logic_vector(3 downto 0);
  signal enxnvzh : real;
  signal xinpqj : std_logic;
  signal y : real_vector(4 downto 1);
  signal jmy : integer;
begin
  jfpumojocd : entity work.dqiyevhcir
    port map (rague => jmy, eodvwm => y, jvl => xinpqj);
  yh : entity work.tprrulon
    port map (mvspgoxmm => enxnvzh, osr => uemk);
  mh : entity work.tprrulon
    port map (mvspgoxmm => tiauhikzgv, osr => uemk);
  nfpmxyiho : entity work.tprrulon
    port map (mvspgoxmm => zha, osr => uemk);
  
  -- Single-driven assignments
  vkgvjtigl <= 1 hr;
  y <= (4_0.4, 320.00, 04020.4413, 8#543.134#);
  
  -- Multi-driven assignments
  xinpqj <= '-';
  xinpqj <= 'Z';
end wtvj;



-- Seed after: 18088382028985177571,3108530264173481209
