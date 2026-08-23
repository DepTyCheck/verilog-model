-- Seed: 7533831773618974244,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity sfcell is
  port (gigbtdnc : in std_logic);
end sfcell;

architecture xj of sfcell is
  
begin
  
end xj;

entity utqwsorvl is
  port (jteof : inout boolean_vector(4 to 1));
end utqwsorvl;

library ieee;
use ieee.std_logic_1164.all;

architecture cjvzip of utqwsorvl is
  signal jfzloluvc : std_logic;
begin
  enqgj : entity work.sfcell
    port map (gigbtdnc => jfzloluvc);
  hiksrlpooy : entity work.sfcell
    port map (gigbtdnc => jfzloluvc);
  
  -- Single-driven assignments
  jteof <= (others => TRUE);
  
  -- Multi-driven assignments
  jfzloluvc <= '0';
  jfzloluvc <= 'X';
end cjvzip;

library ieee;
use ieee.std_logic_1164.all;

entity le is
  port (ycs : out std_logic);
end le;

library ieee;
use ieee.std_logic_1164.all;

architecture qkgnxz of le is
  signal mlp : std_logic;
begin
  yt : entity work.sfcell
    port map (gigbtdnc => ycs);
  qopxuxhfg : entity work.sfcell
    port map (gigbtdnc => mlp);
  hitbdhor : entity work.sfcell
    port map (gigbtdnc => ycs);
  b : entity work.sfcell
    port map (gigbtdnc => ycs);
  
  -- Multi-driven assignments
  ycs <= ycs;
  ycs <= 'U';
end qkgnxz;



-- Seed after: 650020434340315387,4245627776430562977
