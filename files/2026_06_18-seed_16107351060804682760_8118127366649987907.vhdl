-- Seed: 16107351060804682760,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity ovldmuvg is
  port (yxas : linkage boolean_vector(1 to 0); vi : linkage std_logic_vector(4 to 3); e : in std_logic; ombrhodhjp : linkage real);
end ovldmuvg;

architecture kquykbtlnt of ovldmuvg is
  
begin
  
end kquykbtlnt;

library ieee;
use ieee.std_logic_1164.all;

entity kuhpkxpnu is
  port (cif : buffer std_logic; kabb : in std_logic_vector(4 downto 4); xvpy : inout integer);
end kuhpkxpnu;

library ieee;
use ieee.std_logic_1164.all;

architecture vekvsfv of kuhpkxpnu is
  signal yomdmwqbl : real;
  signal adowwndtun : std_logic_vector(4 to 3);
  signal tqttpfymnd : boolean_vector(1 to 0);
  signal wtv : real;
  signal cfxluy : boolean_vector(1 to 0);
  signal kffkd : real;
  signal yqgctji : std_logic_vector(4 to 3);
  signal npwmkjgv : boolean_vector(1 to 0);
begin
  e : entity work.ovldmuvg
    port map (yxas => npwmkjgv, vi => yqgctji, e => cif, ombrhodhjp => kffkd);
  xejjapw : entity work.ovldmuvg
    port map (yxas => cfxluy, vi => yqgctji, e => cif, ombrhodhjp => wtv);
  xggre : entity work.ovldmuvg
    port map (yxas => tqttpfymnd, vi => adowwndtun, e => cif, ombrhodhjp => yomdmwqbl);
  
  -- Single-driven assignments
  xvpy <= 2#1_0_0#;
  
  -- Multi-driven assignments
  cif <= '1';
  adowwndtun <= (others => '0');
  cif <= 'W';
end vekvsfv;

entity goafmrw is
  port (bbqxwpyyb : buffer time; omqbgg : inout time);
end goafmrw;

library ieee;
use ieee.std_logic_1164.all;

architecture wxcruwaoj of goafmrw is
  signal wqaukds : real;
  signal ylprtgbdd : std_logic_vector(4 to 3);
  signal quujknzfo : boolean_vector(1 to 0);
  signal svvo : real;
  signal o : std_logic_vector(4 to 3);
  signal xqvgkhspwi : boolean_vector(1 to 0);
  signal yaw : real;
  signal mb : std_logic_vector(4 to 3);
  signal njn : boolean_vector(1 to 0);
  signal umgxz : integer;
  signal kpqg : std_logic_vector(4 downto 4);
  signal wjmq : std_logic;
begin
  ogyqysw : entity work.kuhpkxpnu
    port map (cif => wjmq, kabb => kpqg, xvpy => umgxz);
  xnxjifvkb : entity work.ovldmuvg
    port map (yxas => njn, vi => mb, e => wjmq, ombrhodhjp => yaw);
  i : entity work.ovldmuvg
    port map (yxas => xqvgkhspwi, vi => o, e => wjmq, ombrhodhjp => svvo);
  qzau : entity work.ovldmuvg
    port map (yxas => quujknzfo, vi => ylprtgbdd, e => wjmq, ombrhodhjp => wqaukds);
end wxcruwaoj;

library ieee;
use ieee.std_logic_1164.all;

entity tlghb is
  port (n : in time; x : linkage real; xbhtsoahw : inout real; uhpalpj : buffer std_logic_vector(2 to 3));
end tlghb;

library ieee;
use ieee.std_logic_1164.all;

architecture hibr of tlghb is
  signal iiwlbm : std_logic;
  signal vhxico : boolean_vector(1 to 0);
  signal veumedfgc : real;
  signal o : boolean_vector(1 to 0);
  signal xpprmrm : real;
  signal myr : std_logic;
  signal zyn : std_logic_vector(4 to 3);
  signal wyjuluiucq : boolean_vector(1 to 0);
  signal joiiunmbot : time;
  signal aa : time;
begin
  t : entity work.goafmrw
    port map (bbqxwpyyb => aa, omqbgg => joiiunmbot);
  fc : entity work.ovldmuvg
    port map (yxas => wyjuluiucq, vi => zyn, e => myr, ombrhodhjp => xpprmrm);
  je : entity work.ovldmuvg
    port map (yxas => o, vi => zyn, e => myr, ombrhodhjp => veumedfgc);
  qjh : entity work.ovldmuvg
    port map (yxas => vhxico, vi => zyn, e => iiwlbm, ombrhodhjp => xbhtsoahw);
  
  -- Multi-driven assignments
  zyn <= "";
  iiwlbm <= 'U';
  uhpalpj <= ('L', '1');
end hibr;



-- Seed after: 10514765172605380221,8118127366649987907
