-- Seed: 399068657341308267,12260394286515585877

entity zynqrnotbx is
  port (k : out time; ocvbm : buffer integer);
end zynqrnotbx;

architecture qavsyhovza of zynqrnotbx is
  
begin
  -- Single-driven assignments
  ocvbm <= ocvbm;
  k <= 8#3_6.5# fs;
end qavsyhovza;

entity pwcd is
  port (tr : linkage time; hrcjxndett : buffer time);
end pwcd;

architecture qiox of pwcd is
  signal jqurkllq : integer;
  signal awktepoup : time;
  signal wpijx : integer;
  signal pxtzxpatw : time;
  signal xyt : integer;
begin
  hpkaazym : entity work.zynqrnotbx
    port map (k => hrcjxndett, ocvbm => xyt);
  lvkmf : entity work.zynqrnotbx
    port map (k => pxtzxpatw, ocvbm => wpijx);
  sleppgph : entity work.zynqrnotbx
    port map (k => awktepoup, ocvbm => jqurkllq);
end qiox;

library ieee;
use ieee.std_logic_1164.all;

entity hokhui is
  port (lyny : inout time; ogzzen : out integer; ujlylazye : buffer std_logic_vector(3 downto 3); enqlccmlh : buffer std_logic);
end hokhui;

architecture qajileozj of hokhui is
  signal ycu : time;
  signal ppisjymnho : time;
  signal atrdmnla : time;
  signal anxc : integer;
  signal hihm : time;
  signal ka : time;
  signal vercyhvzr : time;
begin
  zbh : entity work.pwcd
    port map (tr => vercyhvzr, hrcjxndett => ka);
  y : entity work.zynqrnotbx
    port map (k => hihm, ocvbm => anxc);
  ilpfcs : entity work.pwcd
    port map (tr => atrdmnla, hrcjxndett => lyny);
  c : entity work.pwcd
    port map (tr => ppisjymnho, hrcjxndett => ycu);
  
  -- Single-driven assignments
  ogzzen <= 1_4_2_3;
  
  -- Multi-driven assignments
  ujlylazye <= ujlylazye;
  ujlylazye <= "1";
  enqlccmlh <= enqlccmlh;
  enqlccmlh <= enqlccmlh;
end qajileozj;



-- Seed after: 10141001536204131246,12260394286515585877
