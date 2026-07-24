-- Seed: 14788443712041591530,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity fiwdgncwhu is
  port (dsn : buffer time; apx : inout std_logic_vector(0 to 2));
end fiwdgncwhu;

architecture nm of fiwdgncwhu is
  
begin
  -- Single-driven assignments
  dsn <= 8#1_5# ms;
  
  -- Multi-driven assignments
  apx <= ('0', 'X', 'X');
  apx <= apx;
  apx <= "H0X";
end nm;

library ieee;
use ieee.std_logic_1164.all;

entity altjuo is
  port (obkmk : buffer std_logic_vector(0 to 3); ccefwc : buffer std_logic_vector(1 to 2); wyzgefohr : out real);
end altjuo;

library ieee;
use ieee.std_logic_1164.all;

architecture bbjsvwgj of altjuo is
  signal hmo : time;
  signal h : time;
  signal aper : std_logic_vector(0 to 2);
  signal jziticcf : time;
  signal thzbzqti : std_logic_vector(0 to 2);
  signal kdpaufn : time;
begin
  lku : entity work.fiwdgncwhu
    port map (dsn => kdpaufn, apx => thzbzqti);
  gmeh : entity work.fiwdgncwhu
    port map (dsn => jziticcf, apx => aper);
  tto : entity work.fiwdgncwhu
    port map (dsn => h, apx => thzbzqti);
  m : entity work.fiwdgncwhu
    port map (dsn => hmo, apx => aper);
  
  -- Single-driven assignments
  wyzgefohr <= wyzgefohr;
  
  -- Multi-driven assignments
  aper <= thzbzqti;
  thzbzqti <= thzbzqti;
end bbjsvwgj;

entity wbrhjkwnfz is
  port (mpai : out bit);
end wbrhjkwnfz;

architecture qvtculvxrm of wbrhjkwnfz is
  
begin
  -- Single-driven assignments
  mpai <= '0';
end qvtculvxrm;



-- Seed after: 1689085249003312518,16461708287571398341
