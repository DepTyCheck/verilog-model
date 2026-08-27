-- Seed: 18077495356407289649,6299883410057943775

entity wkdjyypadm is
  port (yqjkzlvc : linkage time; lhwrri : linkage time; hi : linkage severity_level);
end wkdjyypadm;

architecture iyjzy of wkdjyypadm is
  
begin
  
end iyjzy;

library ieee;
use ieee.std_logic_1164.all;

entity ik is
  port (hwtez : inout std_logic);
end ik;

architecture zsiwmzgzcn of ik is
  signal cphemc : severity_level;
  signal cadcblv : time;
  signal jmhlhf : time;
  signal i : severity_level;
  signal dhauivx : time;
  signal hptsxsis : time;
  signal wjsaqfvaza : severity_level;
  signal ezejg : time;
  signal wsvfugiw : time;
begin
  ayyiirjvz : entity work.wkdjyypadm
    port map (yqjkzlvc => wsvfugiw, lhwrri => ezejg, hi => wjsaqfvaza);
  xfylwqgtlx : entity work.wkdjyypadm
    port map (yqjkzlvc => hptsxsis, lhwrri => dhauivx, hi => i);
  zq : entity work.wkdjyypadm
    port map (yqjkzlvc => jmhlhf, lhwrri => cadcblv, hi => cphemc);
  
  -- Multi-driven assignments
  hwtez <= hwtez;
end zsiwmzgzcn;

entity vgtbz is
  port (djegpy : inout real; rh : in time; sdgpggc : buffer time_vector(3 downto 3));
end vgtbz;

architecture xxytbqgrs of vgtbz is
  
begin
  -- Single-driven assignments
  sdgpggc <= sdgpggc;
end xxytbqgrs;

entity gpwiwal is
  port (hswztgbp : in character; chd : inout boolean);
end gpwiwal;

library ieee;
use ieee.std_logic_1164.all;

architecture ahdzy of gpwiwal is
  signal luvwv : std_logic;
  signal lxm : time_vector(3 downto 3);
  signal jl : time;
  signal ggdriw : real;
  signal wsjzm : time_vector(3 downto 3);
  signal rikyy : time;
  signal ei : real;
begin
  iwvm : entity work.vgtbz
    port map (djegpy => ei, rh => rikyy, sdgpggc => wsjzm);
  axzkvyuz : entity work.vgtbz
    port map (djegpy => ggdriw, rh => jl, sdgpggc => lxm);
  lkvvyjomr : entity work.ik
    port map (hwtez => luvwv);
  
  -- Single-driven assignments
  chd <= FALSE;
  jl <= 43.2_0 fs;
  rikyy <= jl;
  
  -- Multi-driven assignments
  luvwv <= 'U';
  luvwv <= '0';
end ahdzy;



-- Seed after: 13307786809547469043,6299883410057943775
