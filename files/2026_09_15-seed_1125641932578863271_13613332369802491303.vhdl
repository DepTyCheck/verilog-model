-- Seed: 1125641932578863271,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity nsez is
  port (uejluep : linkage integer; titjeeob : out std_logic; rnbcoiqv : buffer std_logic_vector(4 to 4); mgnyjcwoq : buffer time);
end nsez;

architecture cskzs of nsez is
  
begin
  
end cskzs;

entity kxlkydb is
  port (wwplmub : out time);
end kxlkydb;

library ieee;
use ieee.std_logic_1164.all;

architecture aiwac of kxlkydb is
  signal xjsqjtbmb : integer;
  signal ulomz : time;
  signal ddw : std_logic_vector(4 to 4);
  signal aalnoskthu : integer;
  signal njsdc : time;
  signal eex : integer;
  signal ux : time;
  signal vxgjoxcjpy : std_logic_vector(4 to 4);
  signal vxbuiappm : std_logic;
  signal n : integer;
begin
  yqlqph : entity work.nsez
    port map (uejluep => n, titjeeob => vxbuiappm, rnbcoiqv => vxgjoxcjpy, mgnyjcwoq => ux);
  ohytimygaa : entity work.nsez
    port map (uejluep => eex, titjeeob => vxbuiappm, rnbcoiqv => vxgjoxcjpy, mgnyjcwoq => njsdc);
  qq : entity work.nsez
    port map (uejluep => aalnoskthu, titjeeob => vxbuiappm, rnbcoiqv => ddw, mgnyjcwoq => ulomz);
  pigzql : entity work.nsez
    port map (uejluep => xjsqjtbmb, titjeeob => vxbuiappm, rnbcoiqv => ddw, mgnyjcwoq => wwplmub);
end aiwac;

library ieee;
use ieee.std_logic_1164.all;

entity okyoe is
  port (z : inout std_logic; jqof : inout std_logic_vector(1 to 4); menrdzcesr : out integer);
end okyoe;

architecture cjorayugp of okyoe is
  signal whsharoi : time;
  signal ccvumd : time;
begin
  chtqvhzn : entity work.kxlkydb
    port map (wwplmub => ccvumd);
  ov : entity work.kxlkydb
    port map (wwplmub => whsharoi);
  
  -- Single-driven assignments
  menrdzcesr <= 8#1#;
  
  -- Multi-driven assignments
  z <= z;
  jqof <= jqof;
end cjorayugp;



-- Seed after: 1457903270957050380,13613332369802491303
