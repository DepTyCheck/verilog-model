-- Seed: 10636200656834015221,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity kqaczws is
  port (yallx : buffer std_logic; yvtvtmbi : buffer character; m : out std_logic_vector(2 downto 4));
end kqaczws;

architecture lxirlipzti of kqaczws is
  
begin
  
end lxirlipzti;

entity xjmmrdraca is
  port (hzvnjlw : buffer character; kdriybxckr : out bit_vector(0 downto 1));
end xjmmrdraca;

library ieee;
use ieee.std_logic_1164.all;

architecture gtmqodkmnc of xjmmrdraca is
  signal zolvngti : std_logic_vector(2 downto 4);
  signal mvsnus : std_logic;
begin
  vdcawiz : entity work.kqaczws
    port map (yallx => mvsnus, yvtvtmbi => hzvnjlw, m => zolvngti);
  
  -- Single-driven assignments
  kdriybxckr <= (others => '0');
end gtmqodkmnc;



-- Seed after: 1858085873509384080,16461708287571398341
