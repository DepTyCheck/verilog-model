-- Seed: 9326731111307802015,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity qt is
  port (xfcu : out severity_level; aktpqg : in integer; qg : out std_logic; tj : buffer real);
end qt;

architecture b of qt is
  
begin
  -- Single-driven assignments
  tj <= tj;
  xfcu <= xfcu;
  
  -- Multi-driven assignments
  qg <= 'U';
end b;



-- Seed after: 16351003524581058394,6377581586043967497
